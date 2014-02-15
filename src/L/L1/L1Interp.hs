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
   ,interpL1File
   ,interpL1OrDie
   ,interpL1String
   ,showOutput
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
import L.CompilationUnit
import L.IOHelpers
import L.L1L2AST
import L.L1L2Parser
import L.Read
import L.Utils

type RegisterState ws = Map Register ws
type Memory ws = Vector ws
type Output = [String]
type Ip ws = ws -- instruction pointer

class (Bits ws, Num ws, Integral ws, Ord ws, Show ws) => WordSize ws where

instance WordSize Int32
instance WordSize Int64

-- ws is Word Size
data Computer ws = Computer {
   _registers :: RegisterState ws
 , _memory    :: Memory ws
 , _program   :: Vector (L1Instruction ws)
 , _labels    :: Map (L1Instruction ws) ws
 , _output    :: Output
 , _ip        :: Ip ws
 , _heapP     :: ws -- pointer to top of heap
 , _halted    :: Bool
} deriving Show

makeClassy ''Computer

showOutput c = mkString "" $ reverse (c^.output)

oneMeg = 1048576
twoMeg = oneMeg * 2
memSize = 2048 -- twoMeg
ebpStart :: WordSize ws => ws
ebpStart = ((fromIntegral memSize - 1) * 4)
espStart :: WordSize ws => ws
espStart = ebpStart
zero :: WordSize ws => ws
zero = 0
registerStartState :: WordSize ws => RegisterState ws
registerStartState = Map.fromList [(eax, zero), (ebx, zero), (ecx, zero), (edx, zero),
                                   (edi, zero), (esi, zero), (ebp, ebpStart), (esp, espStart)]
emptyMem :: WordSize ws => Memory ws
emptyMem = Vector.replicate memSize 0

newComputer :: WordSize ws => L1 ws -> Computer ws
newComputer (Program main fs) = Computer rss emptyMem prog indices [] 0 0 False where
  rss = registerStartState
  -- put all the instructions into a single vector
  insts = Prelude.concat $ (body main ++ [Return]) : (fmap body fs)
  prog = Vector.fromList insts
  indices = Map.fromList $ fmap f $ zipWithIndex insts where
    f (x,i) = (x, fromIntegral i)

-- set the value of a register to an int value
writeReg :: WordSize ws => Register -> ws -> Computer ws -> Computer ws
writeReg reg newValue c = newReg (Map.insert reg newValue $ c^.registers) c

-- set the value of r1 to the value of r2
set :: WordSize ws => Register -> Register -> Computer ws -> Computer ws
set r1 r2 c = writeReg r1 (readReg r2 c) c

-- read the value of a register
readReg :: WordSize ws => Register -> Computer ws -> ws
readReg r c = 
  maybe (error $ "error: unitialized register: " ++ (show r)) id (Map.lookup r $ c^.registers)

-- write an int into memory at the given address
writeMem :: WordSize ws => ws -> ws -> Computer ws -> Computer ws
writeMem addr value c = newMem ((c^.memory) Vector.// [(fromIntegral addr `div` 4, value)]) c

-- read a single int from memory
readMem :: WordSize ws => ws -> Computer ws -> ws
readMem addr c = (c^.memory) Vector.! (fromIntegral addr `div` 4)

-- read an array from memory
readArray :: WordSize ws => ws -> Computer ws -> Vector ws
readArray addr c = 
  let size = readMem addr c
  in Vector.slice (fromIntegral addr `div` 4 + 1) (fromIntegral size) (c^.memory)

-- push the given int argument onto the top of the stack
-- adjust esp accordingly
-- from: http://www.cs.virginia.edu/~evans/cs216/guides/x86.html
-- "push first decrements ESP by 4, then places its operand 
--  into the contents of the 32-bit location at address [ESP]."
push :: WordSize ws => ws -> Computer ws -> Computer ws
push value c =
  let espVal = readReg esp c - 4
      c'     = writeReg esp espVal c
  in  writeMem espVal value c'

-- pop the top value off the stack into the given register
-- adjust esp accordingly.
pop :: WordSize ws => Register -> Computer ws -> Computer ws
pop r c =
  let espVal   = readReg esp c
      c' = writeReg r (readMem espVal c) c
  in writeReg esp (espVal + 4) c'

ret :: WordSize ws => Computer ws -> Computer ws
ret c = 
  let espVal = readReg esp c
      done   = espVal >= (fromIntegral $ Vector.length (c^.memory) * 4)
      c'     = writeReg esp (espVal + 4) c
      c''    = goto (readMem espVal c') c'
  in if done then halt c else c''

adjustNum :: Bits ws => ws -> ws
adjustNum n = shiftR n 1

-- TODO: test if heap runs into stack, and vice-versa
allocate :: WordSize ws => ws -> ws -> Computer ws -> (ws, Computer ws)
allocate size n c =
  let size'   = adjustNum size
      ns      = Prelude.replicate (fromIntegral size') n
      indices :: [Int]
      indices = [(fromIntegral $ c^.heapP `div` 4)..]
      heap    = newMem ((c^.memory) Vector.// (zip indices $ size' : ns))
                       (c & heapP +~ ((size'+1)*4))
  in (c^.heapP, heap)

-- print a number or an array
--   if the int argument is an encoded int, prints the int
--   else it's an array, print the contents of the array (and recur)
l1print :: WordSize ws => ws -> Computer ws -> Computer ws
l1print n c = addOutput (printContent n 0 ++ "\n") c where
  --printContent :: Num ws1 => ws1 -> ws1 -> String
  printContent n depth
    | depth >= 4   = "..."
    | n .&. 1 == 1 = show $ shiftR n 1
    | otherwise    =
      let size  = readMem n c
          arr   = readArray n c
          contentsV = Vector.map (\n -> printContent n $ depth + 1) arr
          contents  = mkString ", " $ show size : Vector.toList contentsV
      in "{s:" ++ contents ++ "}"

-- print an array error
arrayError :: WordSize ws => ws -> ws -> Computer ws -> Computer ws
arrayError a x c = haltWith msg c where
  pos  = show $ adjustNum x
  size = show $ readMem a c
  msg  = "attempted to use position " ++ pos ++ 
         " in an array that only has "++ size ++" positions"

findLabelIndex :: WordSize ws => Label -> Computer ws -> ws
findLabelIndex l c =
  maybe (error $ "no such label: " ++ l) id (Map.lookup (LabelDeclaration l) (c^.labels))

readS :: WordSize ws => L1S ws -> Computer ws -> ws
readS (NumberL1S n) _ = n
readS (RegL1S r)    c = readReg r c
readS (LabelL1S l)  c = findLabelIndex l c

newReg :: WordSize ws => RegisterState ws -> Computer ws -> Computer ws
newReg r c = c & registers .~ r
newMem :: Memory ws -> Computer ws -> Computer ws
newMem m c = c & memory .~ m
goto :: Ip ws -> Computer ws -> Computer ws
goto m c = c & ip .~ m
addOutput :: String -> Computer ws -> Computer ws
addOutput s c = c & output %~ (s:)
haltWith msg c = c & addOutput msg & halted .~ True
halt c = c & halted .~ True
currentInst c = (c^.program) Vector.! (fromIntegral $ c^.ip)
hasNextInst c = (c^.ip) < (fromIntegral $ Vector.length (c^.program))
-- advance the computer to the next instruction
nextInst :: WordSize ws => Computer ws -> Computer ws
nextInst c = goto (c^.ip + 1) c
-- goto the next instruction after writing a register
nextInstWR :: WordSize ws => Register -> ws -> Computer ws -> Computer ws
nextInstWR r i c = nextInst $ writeReg r i c

-- run the given L1 program to completion on a new computer
-- return the computer as the final result.
interpL1 :: WordSize ws => L1 ws -> Computer ws
interpL1 p = go (newComputer p)

-- the main loop, runs a computer until completion
go :: WordSize ws => Computer ws -> Computer ws
go c 
  | c^.halted || not (hasNextInst c) = c
  | otherwise = go $ step (currentInst c) c

step :: WordSize ws => L1Instruction ws -> Computer ws -> Computer ws
-- Assignment statements
step (Assign r (CompRHS (Comp s1 op s2))) c  = nextInstWR r 
  (if cmp op (readS s1 c) (readS s2 c) then 1 else 0) c
step (Assign r (MemRead (MemLoc x offset))) c =
  let index = readReg x c + fromIntegral offset
  in nextInstWR r (readMem index c) c
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
  in nextInst $ writeMem index (readS s c) c
-- Goto
step (Goto l) c = goto (findLabelIndex l c) c
-- LabelDec, just advance
step (LabelDeclaration _) c = nextInst c
-- Call
--   TODO: push return location onto stack, have return goto it.
step (Call s) c = 
  let func = readS s c
      c'   = push (c^.ip + 1) c
      c''  = push (readReg ebp c') c' -- pushl %ebp
      c''' = set ebp esp c''          -- movl %esp, %ebp
  in goto func c'''
-- TailCall
step (TailCall s) c = goto (readS s c) c
-- Return
step Return c = ret $ pop ebp $ set esp ebp c

interpL1String :: String -> Either String String
interpL1String code = showOutput . interpL1 <$> parseL132 (sread code)

interpL1OrDie :: String -> String
interpL1OrDie = (either error id) . interpL1String

interpL1File = 
  do s <- compile_ $ (either error id) . interpL1String
     putStrLn (snd s)
