{-# LANGUAGE TemplateHaskell #-}

module L.L1.L1Interp (
  Computer(..)
 ,interp
 ,interpL1File
 ,interpL1OrDie
 ) where

import Control.Applicative
import Control.Lens hiding (set)
import Data.Bits
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

type RegisterState = Map Register Int
type Memory = Vector Int
type Output = [String]
type Ip = Int -- instruction pointer

data Computer = Computer {
   _registers :: RegisterState
 , _memory    :: Memory
 , _program   :: Vector L1Instruction
 , _labels    :: Map L1Instruction Int
 , _output    :: Output
 , _ip        :: Ip
 , _heapP     :: Int -- pointer to top of heap
 , _halted    :: Bool
} deriving (Show)

makeClassy ''Computer

showOutput c = mkString "" $ reverse (c^.output)

oneMeg = 1048576
twoMeg = oneMeg * 2
memSize = 2048 -- twoMeg
ebpStart = (memSize - 1) * 4
espStart = ebpStart
registerStartState = Map.fromList [(eax, 0), (ebx, 0), (ecx, 0), (edx, 0),
                                   (edi, 0), (esi, 0), (ebp, ebpStart), (esp, espStart)]
emptyMem = Vector.replicate memSize 0
newComputer (Program main fs) = Computer rss emptyMem prog indices [] 0 0 False where
  rss = registerStartState
  -- put all the instructions into a single vector
  insts = Prelude.concat $ (body main ++ [Return]) : (fmap body fs)
  prog :: Vector L1Instruction
  prog = Vector.fromList insts
  indices :: Map L1Instruction Int
  indices = Map.fromList $ zipWithIndex insts

-- set the value of a register to an int value
writeReg :: Register -> Int -> Computer -> Computer
writeReg reg newValue c = newReg (Map.insert reg newValue $ c^.registers) c

-- set the value of r1 to the value of r2
set :: Register -> Register -> Computer -> Computer
set r1 r2 c = writeReg r1 (readReg r2 c) c

-- read the value of a register
readReg :: Register -> Computer -> Int
readReg r c = 
  maybe (error $ "error: unitialized register: " ++ (show r)) id (Map.lookup r $ c^.registers)

-- write an int into memory at the given address
writeMem :: Int -> Int -> Computer -> Computer
writeMem addr value c = newMem ((c^.memory) Vector.// [(addr `div` 4, value)]) c

-- read a single int from memory
readMem :: Int -> Computer -> Int
readMem addr c = (c^.memory) Vector.! (addr `div` 4)

-- read an array from memory
readArray :: Int -> Computer -> Vector Int
readArray addr c = 
  let size = readMem addr c
  in Vector.slice (addr `div` 4 + 1) size (c^.memory)

-- push the given int argument onto the top of the stack
-- adjust esp accordingly
-- from: http://www.cs.virginia.edu/~evans/cs216/guides/x86.html
-- "push first decrements ESP by 4, then places its operand 
--  into the contents of the 32-bit location at address [ESP]."
push :: Int -> Computer -> Computer
push value c =
  let espVal = readReg esp c - 4
      incEsp = writeReg esp espVal c
      newMem = writeMem espVal value incEsp
  in newMem

-- pop the top value off the stack into the given register
-- adjust esp accordingly.
pop :: Register -> Computer -> Computer
pop r c =
  let espVal   = readReg esp c
      newState = writeReg r (readMem espVal c) c
  in writeReg esp (espVal + 4) newState

ret :: Computer -> Computer
ret c = 
  let espVal = readReg esp c
      done   = espVal >= Vector.length (c^.memory) * 4
      c'     = writeReg esp (espVal + 4) c
      c''    = goto (readMem espVal c') c'
  in if done then halt c else c''

adjustNum n = shiftR n 1

-- TODO: test if heap runs into stack, and vice-versa
allocate :: Int -> Int -> Computer -> (Int, Computer)
allocate size n c =
  let size'   = adjustNum size
      ns      = Prelude.replicate size' n
      indices = [(c^.heapP `div` 4)..]
      heap    = newMem ((c^.memory) Vector.// (zip indices (size' : ns)))
                       (bumpHeap ((size'+1)*4) c)
  in (c^.heapP, heap)

-- print a number or an array
--   if the int argument is an encoded int, prints the int
--   else it's an array, print the contents of the array (and recur)
l1print :: Int -> Computer -> Computer
l1print n c = addOutput (printContent n 0 ++ "\n") c where
  printContent :: Int -> Int -> String
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
arrayError :: Int -> Int -> Computer -> Computer
arrayError a x c = haltWith msg c where
  pos  = show $ adjustNum x
  size = show $ readMem a c
  msg  = "attempted to use position " ++ pos ++ 
         " in an array that only has "++ size ++" positions"

findLabelIndex :: Label -> Computer -> Int
findLabelIndex l c =
  maybe (error $ "no such label: " ++ l) id (Map.lookup (LabelDeclaration l) (c^.labels))

readS :: L1S -> Computer -> Int
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
bumpHeap  n c = c & heapP +~ n
haltWith msg c = c & addOutput msg & halted .~ True
halt c = c & halted .~ True
currentInst c = (c^.program) Vector.! (c^.ip)
hasNextInst c = (c^.ip) < Vector.length (c^.program)
-- advance the computer to the next instruction
nextInst :: Computer -> Computer
nextInst c = goto (c^.ip + 1) c
-- goto the next instruction after writing a register
nextInstWR :: Register -> Int -> Computer -> Computer
nextInstWR r i c = nextInst $ writeReg r i c

-- run the given L1 program to completion on a new computer
-- return the computer as the final result.
interp :: L1 -> Computer
interp p = go (newComputer p)

-- the main loop, runs a computer until completion
go :: Computer -> Computer
go c 
  | c^.halted || not (hasNextInst c) = c
  | otherwise = go $ step (currentInst c) c

step :: L1Instruction -> Computer -> Computer
-- Assignment statements
step (Assign r (CompRHS (Comp s1 op s2))) c  = nextInstWR r 
  (if cmp op (readS s1 c) (readS s2 c) then 1 else 0) c
step (Assign r (MemRead (MemLoc x offset))) c =
  let index = readReg x c + offset
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
  let index = readReg x c + offset
  in nextInst $ writeMem index (readS s c) c
-- Goto
step (Goto l) c = goto (findLabelIndex l c) c
-- LabelDec, just advance
step (LabelDeclaration _) c = nextInst c
-- Call
--   TODO: push return location onto stack, have return goto it.
step (Call s) c = 
  let func      = readS s c
      newStack  = push (c^.ip + 1) c
      newStack' = push (readReg ebp newStack) newStack -- pushl %ebp
      newEbp    = set ebp esp newStack'                -- movl %esp, %ebp
  in goto func newEbp
-- TailCall
step (TailCall s) c = goto (readS s c) c
-- Return
step Return c = 
  let newEsp    = set esp ebp c
      newEbp    = pop ebp newEsp
  in ret newEbp

interpL1 :: String -> Either String String
interpL1 code = showOutput . interp <$> parseL164 (sread code)

interpL1OrDie :: String -> String
interpL1OrDie = (either error id) . interpL1

interpL1File = 
  do s <- compile_ $ (either error id) . interpL1
     putStrLn (snd s)
