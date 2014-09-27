{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}

module L.L1.L1Interp 
  (
    Computer(..)
   ,interpL1
   ,newComputer
   ,showComputerOutput
  )
where

import Control.Applicative
import Control.Monad (ap)
import Control.Monad.ST
import Control.Monad.State.Class
import Control.Lens hiding (set)
import Data.Bits
import Data.Int
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Tuple
import Data.Vector (Vector)
import qualified Data.Vector as Vector
import Debug.Trace
import L.IOHelpers
import L.L1L2AST hiding (registers)
import L.L1L2Parser
import L.Read
import L.Utils
import L.L1.MainAdjuster (adjustMain)

type RegisterState = Map Register Int64
type Memory = Vector Int64
type Output = [String]
type Ip = Int64 -- instruction pointer

data Computer = Computer {
   _registers :: RegisterState
 , _memory    :: Memory
 , _program   :: Vector L1Instruction
 , _labels    :: Map L1Instruction Int64
 , _output    :: Output
 , _ip        :: Ip
 , _heapP     :: Int64 -- pointer to top of heap
 , _halted    :: Bool
}
makeClassy ''Computer

data PrintableComputer  = PrintableComputer {
   registersP :: RegisterState
 , memoryP    :: Memory
 , outputP    :: Output
 , ipP        :: Ip
 , instP      :: L1Instruction
 , heapPP     :: Int64 -- pointer to top of heap
 , haltedP    :: Bool
} deriving Show

instance Show Computer where
  show c = show $ printable c where
    printable c =
      PrintableComputer
        (c^.registers) (c^.memory) (c^.output) (c^.ip)
        (currentInst c) (c^.heapP) (c^.halted)

showComputerOutput c = mkString "" $ reverse (c^.output)

oneMeg = 1048576
twoMeg = oneMeg * 2
memSize = 2048 :: Int -- twoMeg
rspStart = fromIntegral memSize * 8
zero = 0 :: Int64
registerStartState = Map.fromList [
 (rax, zero),
 (rbx, zero),
 (rcx, zero),
 (rdx, zero),
 (rdi, zero),
 (r8 , zero),
 (r9 , zero),
 (r10, zero),
 (r11, zero),
 (r12, zero),
 (r13, zero),
 (r14, zero),
 (r15, zero),
 (rsi, zero),
 (rbp, zero),
 (rsp, rspStart) ]
emptyMem = Vector.replicate memSize zero

newComputer (Program main fs) = Computer rss emptyMem prog indices [] 0 0 False where
  rss = registerStartState
  -- put all the instructions into a single vector
  insts = Prelude.concat $ fmap body $ main : fs
  prog = Vector.fromList insts
  indices = Map.fromList $ fmap f $ zipWithIndex insts where
    f (x,i) = (x, fromIntegral i)

-- set the value of a register to an int value
writeReg :: Register -> Int64 -> Computer -> Computer
writeReg reg newValue c = newReg (Map.insert reg newValue $ c^.registers) c

-- set the value of r1 to the value of r2
set :: Register -> Register -> Computer -> Computer
set r1 r2 c = writeReg r1 (readReg r2 c) c

-- read the value of a register
readReg :: Register -> Computer -> Int64
readReg r c = 
  maybe (error $ "error: unitialized register: " ++ (show r)) id (Map.lookup r $ c^.registers)

-- write an int into memory at the given address
writeMem :: String -> Int64 -> Int64 -> Computer -> Computer
writeMem caller addr value c = go where
  index = fromIntegral addr `div` 8
  go | index < memSize = newMem ((c^.memory) Vector.// [(index, value)]) c
     | otherwise = error $ caller ++ "tried to write out of bounds memory index: " ++ show index

-- read a single int from memory
readMem :: String -> Int64 -> Computer -> Int64
readMem caller addr c = go where
  index = fromIntegral addr `div` 8
  go | index < memSize = (c^.memory) Vector.! index
     | otherwise       = error $ caller ++ "tried to access out of bounds memory index: " ++ show index

-- read an array from memory
readArray :: Int64 -> Computer -> Vector Int64
readArray addr c = go where
  size       = fromIntegral $ readMem "readArray" addr c
  startIndex = fromIntegral addr `div` 8 + 1
  go | startIndex + size < memSize = Vector.slice startIndex size (c^.memory)
     | otherwise = error $ "readArray tried to access out of bounds memory index: " ++ show startIndex

-- push the given int argument onto the top of the stack
-- adjust rsp accordingly
-- from: http://www.cs.virginia.edu/~evans/cs216/guides/x86.html
-- "push first decrements ESP by 4, then places its operand 
--  into the contents of the 32-bit location at address [ESP]."
push :: Int64 -> Computer -> Computer
push value c =
  let rspVal = readReg rsp c - 8
      c'     = writeReg rsp rspVal c
  in  writeMem "push" rspVal value c'

-- pop the top value off the stack into the given register
-- adjust rsp accordingly.
pop :: Register -> Computer -> Computer
pop r c =
  let rspVal = readReg rsp c
      c'     = writeReg r (readMem "pop" rspVal c) c
  in writeReg rsp (rspVal + 8) c'

adjustNum :: Int64 -> Int64
adjustNum n = shiftR n 1

-- TODO: test if heap runs into stack, and vice-versa
allocate :: Int64 -> Int64 -> Computer -> (Int64, Computer)
allocate size n c =
  let size'   = adjustNum size
      ns      = Prelude.replicate (fromIntegral size') n
      indices :: [Int]
      indices = [(fromIntegral $ c^.heapP `div` 8)..]
      nsWithIndices = zip indices $ size' : ns
      heap    = newMem ((c^.memory) Vector.// nsWithIndices) (c & heapP +~ ((size'+1)*8))
  in (c^.heapP, heap)

-- print a number or an array
--   if the int argument is an encoded int, prints the int
--   else it's an array, print the contents of the array (and recur)
l1print :: Int64 -> Computer -> Computer
l1print n c = addOutput (printContent n 0 ++ "\n") c where
  printContent n depth
    | depth >= 4   = "..."
    | n .&. 1 == 1 = show $ shiftR n 1
    | otherwise    =
      let size  = readMem "l1print" n c
          arr   = readArray n c
          contentsV = Vector.map (\n -> printContent n $ depth + 1) arr
          contents  = mkString ", " $ show size : Vector.toList contentsV
      in "{s:" ++ contents ++ "}"

-- print an array error
arrayError :: Int64 -> Int64 -> Computer -> Computer
arrayError a x c = haltWith msg c where
  pos  = show $ adjustNum x
  size = show $ readMem "arrayError" a c
  msg  = "attempted to use position " ++ pos ++ 
         " in an array that only has "++ size ++" positions"

findLabelIndex :: Label -> Computer -> Int64
findLabelIndex l c =
  maybe (error $ "no such label: " ++ l) id (Map.lookup (LabelDeclaration l) (c^.labels))

readS :: L1S -> Computer -> Int64
readS (NumberL1S n) _ = n
readS (RegL1S r)    c = readReg r c
readS (LabelL1S l)  c = findLabelIndex l c

newReg :: RegisterState -> Computer -> Computer
newReg r c = c & registers .~ r
newMem :: Memory -> Computer -> Computer
newMem m c = c & memory .~ m
goto :: Ip -> Computer -> Computer
goto m c = c & ip .~ m
addOutput :: String -> Computer -> Computer
addOutput s c = c & output %~ (s:)
haltWith msg c = c & addOutput msg & halted .~ True
halt c = c & halted .~ True
currentInst c = go where
  ip' :: Int
  ip' = fromIntegral $ c^.ip
  go | ip' < memSize = (c^.program) Vector.! ip'
     | otherwise     = error $ "instruction out of bounds: " ++ show ip'

hasNextInst c = (c^.ip) < (fromIntegral $ Vector.length (c^.program))
-- advance the computer to the next instruction
nextInst :: Computer -> Computer
nextInst c = goto (c^.ip + 1) c
-- goto the next instruction after writing a register
nextInstWR :: Register -> Int64 -> Computer -> Computer
nextInstWR r i c = nextInst $ writeReg r i c

-- run the given L1 program to completion on a new computer
-- return the computer as the final result.
interpL1 :: L1 -> Computer
interpL1 p = let
  c = newComputer $ adjustMain p
  in go c --(traceSA (unlines . map show . zip [0..] . Vector.toList $ c^.program) c)

-- the main loop, runs a computer until completion
go :: Computer -> Computer
go c 
  | c^.halted || not (hasNextInst c) = c
  | otherwise = go $ newC where --trace (show newC) newC
      newC = step (currentInst c) c

step :: L1Instruction -> Computer -> Computer
-- Assignment statements
step (Assign r (CompRHS (Comp s1 op s2))) c  = nextInstWR r 
  (if cmp op (readS s1 c) (readS s2 c) then 1 else 0) c
step (Assign r (MemRead (MemLoc x offset))) c =
  let index = readReg x c + fromIntegral offset
  in nextInstWR r (readMem "step MemRead" index c) c
step (Assign r (Allocate size datum)) c   = 
  let (h, c') = allocate (readS size c) (readS datum c) c
  in nextInstWR r h c'
step (Assign r (Print s)) c = nextInstWR r 1 $ l1print  (readS s c)  c
step (Assign r (ArrayError s1 s2)) c = arrayError (readS s1 c) (readS s2 c) c
step (Assign r (SRHS s)) c = nextInstWR r (readS s c) c
-- Math Inst
step (MathInst r op s) c = nextInst $ 
  writeReg r (runOp op (readReg r c) (readS s c)) c
-- CJump
step (CJump (Comp s1 op s2) l1 l2) c = 
  let l = if cmp op (readS s1 c) (readS s2 c) 
          then findLabelIndex l1 c else findLabelIndex l2 c
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

