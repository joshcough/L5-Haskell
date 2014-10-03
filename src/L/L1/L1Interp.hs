module L.L1.L1Interp (interpL1) where

import Control.Lens hiding (set)
import Data.Int
import qualified Data.Vector as Vector
import Debug.Trace
import L.Computer
import L.L1L2AST 
import L.L1L2MainAdjuster (adjustMain)
import Prelude hiding (print)

interpL1 :: L1 -> String
interpL1 p = showComputerOutput $ interpL1' p

-- run the given L1 program to completion on a new computer
-- return the computer as the final result.
interpL1' :: L1 -> Computer L1Instruction
interpL1' p = runComputer step (newComputer $ adjustMain p) 
  --(traceSA (unlines . map show . zip [0..] . Vector.toList $ c^.program) c)

step :: Computer L1Instruction -> Computer L1Instruction
step c = let
  debug f = trace ("ip: " ++ show (c^.ip) ++ " inst: " ++ show (currentInst c)) f

  {- uncomment debug below to print ip and env for each instruction -}
  in {-debug $-} case currentInst c of

    -- Assignment statements
    (Assign r (CompRHS (Comp s1 op s2))) -> nextInstWR r
      (if cmp op (readS s1 c) (readS s2 c) then 1 else 0) c
    (Assign r (MemRead (MemLoc x offset))) ->
      let index = readReg x c + fromIntegral offset
      in nextInstWR r (readMem "step MemRead" index c) c
    (Assign r (Allocate size datum)) ->
      let (h, c') = allocate (readS size c) (readS datum c) c
      in nextInstWR r h c'
    (Assign r (Print s)) -> nextInstWR r 1 $ print  (readS s c)  c
    (Assign _ (ArrayError s1 s2)) -> arrayError (readS s1 c) (readS s2 c) c
    (Assign r (SRHS s)) -> nextInstWR r (readS s c) c
    -- Math Inst
    (MathInst r op s) -> nextInst $
      writeReg r (runOp op (readReg r c) (readS s c)) c
    -- CJump
    (CJump (Comp s1 op s2) l1 l2) ->
      let l = findLabelIndex (if cmp op (readS s1 c) (readS s2 c) then l1 else l2) c
      in goto l c
    -- MemWrite
    (MemWrite (MemLoc x offset) s) ->
      let index = readReg x c + fromIntegral offset
      in nextInst $ writeMem "step MemWrite" index (readS s c) c
    -- Goto
    (Goto l) -> goto (findLabelIndex l c) c
    -- LabelDec, just advance
    (LabelDeclaration _) -> nextInst c
    -- Call
    (Call s) ->
      let func = readS s c
          c'   = push (c^.ip + 1) c
      in goto func c'
    -- TailCall
    (TailCall s) -> goto (readS s c) c
    -- Return
    Return ->
      let rspVal = readReg rsp c
          done   = rspVal >= (fromIntegral $ Vector.length (c^.memory) * 8)
          c'     = writeReg rsp (rspVal + 8) c
          c''    = goto (readMem "step Return" rspVal c') c'
      in if done then halt c else c''

readS :: L1S -> Computer L1Instruction -> Int64
readS (NumberL1S n) = \_ -> n
readS (RegL1S r)    = readReg r
readS (LabelL1S l)  = findLabelIndex l

-- goto the next instruction after writing a register
nextInstWR :: Register -> Int64 -> Computer a -> Computer a
nextInstWR r i c = nextInst $ writeReg r i c
