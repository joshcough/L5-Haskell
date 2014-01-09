module L.L1.L1Interp (
  Computer(..)
 ,interp
 ,interpL1File
 ,interpL1OrDie
 ) where

import Control.Applicative
import Data.Bits
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Tuple
import Data.Vector (Vector)
import qualified Data.Vector as Vector
import L.CompilationUnit
import L.L1L2AST
import L.L1L2Parser
import L.Read
import L.Utils

type RegisterState = Map Register Int
type Memory = Vector Int
type Output = [String]
type Ip = Int -- instruction pointer

data Computer = Computer {
  registers :: RegisterState
 ,memory    :: Memory
 ,program   :: Vector L1Instruction
 ,labels    :: Map L1Instruction Int
 ,output    :: Output
 ,ip        :: Ip
 ,heapP     :: Int -- pointer to top of heap
 ,halted    :: Bool
} deriving (Show)

showOutput c = mkString "" $ reverse (output c)

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

-- advance the computer to the next instruction
nextInst :: Computer -> Computer
nextInst c = goto (ip c + 1) c

-- set the value of a register to an int value
writeReg :: Register -> Int -> Computer -> Computer
writeReg reg newValue c = newReg (Map.insert reg newValue $ registers c) c

-- set the value of r1 to the value of r2
set :: Register -> Register -> Computer -> Computer
set r1 r2 c = writeReg r1 (readReg r2 c) c

-- read the value of a register
readReg :: Register -> Computer -> Int
readReg r c = 
  maybe (error $ "error: unitialized register: " ++ (show r)) id (Map.lookup r $ registers c)

-- write an int into memory at the given address
writeMem :: Int -> Int -> Computer -> Computer
writeMem addr value c = newMem (memory c Vector.// [(addr `div` 4, value)]) c

-- read a single int from memory
readMem :: Int -> Computer -> Int
readMem addr c = memory c Vector.! (addr `div` 4)

-- read an array from memory
readArray :: Int -> Computer -> Vector Int
readArray addr c = 
  let size = readMem addr c
  in Vector.slice (addr `div` 4 + 1) size (memory c)

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

adjustNum n = shiftR n 1

-- TODO: test if heap runs into stack, and vice-versa
allocate :: Int -> Int -> Computer -> (Int, Computer)
allocate size n c =
  let size'   = adjustNum size
      ns      = Prelude.replicate size' n
      indices = [(heapP c `div` 4)..]
      heap    = newMem (memory c Vector.// (zip indices (size' : ns)))
                       (bumpHeap ((size'+1)*4) c)
  in (heapP c, heap)

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
          contents = mkString ", " $ show size : Vector.toList contentsV
      in "{s:" ++ contents ++ "}"

-- print an array error
arrayError :: Int -> Int -> Computer -> Computer
arrayError a x c = halt msg c where
  pos  = show $ adjustNum x
  size = show $ readMem a c
  msg  = "attempted to use position " ++ pos ++ 
         " in an array that only has "++ size ++" positions"

findLabelIndex :: Label -> Computer -> Int
findLabelIndex l c =
  maybe (error $ "no such label: " ++ l) id (Map.lookup (LabelDeclaration l) (labels c))

readS :: L1S -> Computer -> Int
readS (NumberL1S n) _ = n
readS (RegL1S r)    c = readReg r c
readS (LabelL1S l)  c = findLabelIndex l c

-- all this should be able to be replaced with Lens
newReg :: RegisterState -> Computer -> Computer
newReg rs c = Computer rs (memory c) (program c) (labels c) (output c) 
                         (ip c) (heapP c) (halted c)
newMem :: Memory -> Computer -> Computer
newMem m c = Computer (registers c) m (program c) (labels c) (output c) 
                        (ip c) (heapP c) (halted c)
goto :: Ip -> Computer -> Computer
goto ip c = Computer (registers c) (memory c) (program c) (labels c) (output c) 
                       ip (heapP c) (halted c)
addOutput :: String -> Computer -> Computer
addOutput s c = Computer (registers c) (memory c) (program c) (labels c) (s : output c) 
                           (ip c) (heapP c) (halted c)
bumpHeap n c = Computer (registers c) (memory c) (program c) (labels c) (output c) 
                          (ip c) (heapP c + n) (halted c)
halt msg c = Computer (registers c) (memory c) (program c) (labels c) (msg : output c)
                        (ip c) (heapP c) True
currentInst c = (program c) Vector.! (ip c)
hasNextInst c = ip c < Vector.length (program c) - 1

-- run the computer with the given L1 program
interp :: L1 -> Computer
interp p  = go $ newComputer p where -- starts go at instruction 0
  -- the main loop
  go :: Computer -> Computer
  -- TODO: check if halted, check if done (ip > number of instructions)
  go c = let c' = step (currentInst c) c
         in if halted c' || not (hasNextInst c') then c' else go c'

advance :: Computer -> Computer
advance = nextInst
advanceWR :: Register -> Int -> Computer -> Computer
advanceWR r i c = advance $ writeReg r i c

step :: L1Instruction -> Computer -> Computer
-- Assignment statements
step (Assign r (CompRHS (Comp s1 op s2))) c  = advanceWR r 
  (if (cmp op (readS s1 c) (readS s2 c)) then 1 else 0) c
step (Assign r (MemRead (MemLoc x offset))) c =
  let index = (readReg x c) + offset
  in advanceWR r (readMem index c) c
step (Assign r (Allocate size datum)) c   = 
  let (h, c') = allocate (readS size c) (readS datum c) c
  in advanceWR r h c'
step (Assign r (Print s)) c          = 
  advanceWR r 1 $ l1print  (readS s c)  c
step (Assign r (ArrayError s1 s2)) c = 
  arrayError (readS s1 c) (readS s2 c) c
step (Assign r (SRHS s)) c           = 
  advanceWR r (readS s c) c
-- Math Inst
step (MathInst r op s) c = advance $ 
  writeReg r (runOp op (readReg r c) (readS s c)) c
-- CJump
step (CJump (Comp s1 op s2) l1 l2) c = 
  let l = if cmp op (readS s1 c) (readS s2 c) 
          then findLabelIndex l1 c else findLabelIndex l2 c
  in goto l c
-- MemWrite
step (MemWrite (MemLoc x offset) s) c =
  let index = (readReg x c) + offset
  in advance $ writeMem index (readS s c) c
-- Goto
step (Goto l) c = goto (findLabelIndex l c) c
-- LabelDec, just advance
step (LabelDeclaration _) c = advance c
-- Call
--   TODO: push return location onto stack, have return goto it.
step (Call s) c  = 
  let func     = readS s c
      newStack = push (readReg ebp c) c -- pushl %ebp
      newEbp   = set ebp esp newStack   -- movl %esp, %ebp
      res      = goto func newEbp
  -- TODO: what if calling s threw an error?
  in goto (ip c + 1) res
-- TailCall
step (TailCall s) c = goto (readS s c) c
-- Return
step Return c = pop ebp $ set esp ebp c 

interpL1 :: String -> Either String String
interpL1 code = showOutput . interp <$> parseL164 (sread code)

interpL1OrDie :: String -> String
interpL1OrDie = (either error id) . interpL1

interpL1File = 
  do s <- compile_ $ (either error id) . interpL1
     putStrLn (snd s)
