module L.L2.L2Interp (interpL2) where

import Control.Lens hiding (set)
import Data.Int
import qualified Data.Vector as Vector
import L.Computer
import L.L1L2AST 
import Prelude hiding (print)

interpL2 :: L2 -> String
interpL2 p = showComputerOutput $ interpL2' p

-- run the given L2 program to completion on a new computer
-- return the computer as the final result.
interpL2' :: L2 -> Computer L2Instruction
interpL2' p = runComputer step (newComputer $ adjustMain p) 
  --(traceSA (unlines . map show . zip [0..] . Vector.toList $ c^.program) c)

step :: L2Instruction -> Computer L2Instruction -> Computer L2Instruction
-- Assignment statements
step (Assign r (CompRHS (Comp s1 op s2))) c  = nextInstWR r 
  (if cmp op (readS s1 c) (readS s2 c) then 1 else 0) c
step (Assign r (MemRead (MemLoc x offset))) c =
  let index = readReg x c + fromIntegral offset
  in nextInstWR r (readMem "step MemRead" index c) c
step (Assign r (Allocate size datum)) c   = 
  let (h, c') = allocate (readS size c) (readS datum c) c
  in nextInstWR r h c'
step (Assign r (Print s)) c = nextInstWR r 1 $ print  (readS s c)  c
step (Assign r (ArrayError s1 s2)) c = arrayError (readS s1 c) (readS s2 c) c
step (Assign r (SRHS s)) c = nextInstWR r (readS s c) c
-- Math Inst
step (MathInst r op s) c = nextInst $ 
  writeReg r (runOp op (readReg r c) (readS s c)) c
-- CJump
step (CJump (Comp s1 op s2) l1 l2) c = 
  let l = findLabelIndex (if cmp op (readS s1 c) (readS s2 c) then l1 else l2) c
  in goto l c
-- MemWrite
step (MemWrite (MemLoc x offset) s) c =
  let index = readReg x c + fromIntegral offset
  in nextInst $ writeMem "step MemWrite" index (readS s c) c
-- Goto
step (Goto l) c = goto (findLabelIndex l c) c
-- LabelDec, just advance
step (LabelDeclaration _) c = nextInst c
-- Call
step (Call s) c =
  let func = readS s c
      c'   = push (c^.ip + 1) c
  in goto func c'
-- TailCall
step (TailCall s) c = goto (readS s c) c
-- Return
step Return c =
  let rspVal = readReg rsp c
      done   = rspVal >= (fromIntegral $ Vector.length (c^.memory) * 8)
      c'     = writeReg rsp (rspVal + 8) c
      c''    = goto (readMem "step Return" rspVal c') c'
  in if done then halt c else c''

readS :: L2S -> Computer L2Instruction -> Int64
readS (NumberL2S n) = \_ -> n
readS (RegL2S r)    = readReg r
readS (LabelL2S l)  = findLabelIndex l
