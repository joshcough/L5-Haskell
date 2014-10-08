{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}

module L.Computer where

import Control.Lens hiding (set)
import Data.Bits
import Data.Int
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Vector (Vector)
import qualified Data.Vector as Vector
--import Debug.Trace
import L.L1L2AST hiding (registers)
import L.Utils

type RegisterState = Map Register Int64
type Memory = Vector Int64
type Output = [String]
type Ip = Int64 -- instruction pointer

data Computer a = Computer {
   _registers :: RegisterState    -- contains runtime values (Int64 for L1/L2, but probably a data type for other languages)
 , _memory    :: Memory           -- "" (same as registers)
 , _program   :: Vector a         -- ok...non-linear programs (L3 and above) might have trouble here.
 , _labels    :: Map Label Int64  -- however, this could me a map from label to something else
 , _output    :: Output           -- output is the same across all languages [String], but, could a computer also possibly have a final result?
 , _ip        :: Ip               -- Ip might mean nothing to L3 and above, but then again maybe theres a way to make use of it.
 , _heapP     :: Int64 -- pointer to top of heap  -- this is good across all languages!
 , _halted    :: Bool             -- can this be used across all languages? I'm not sure...
}
makeClassy ''Computer

data PrintableComputer a = PrintableComputer {
   registersP :: RegisterState
 , memoryP    :: Memory
 , outputP    :: Output
 , ipP        :: Ip
 , instP      :: a
 , heapPP     :: Int64 -- pointer to top of heap
 , haltedP    :: Bool
} deriving Show

instance Show a => Show (Computer a) where
  show c = show $ printable c where
    printable c =
      PrintableComputer
        (c^.registers) (c^.memory) (c^.output) (c^.ip)
        (currentInst c) (c^.heapP) (c^.halted)

showComputerOutput :: HasComputer c a => c -> String
showComputerOutput c = mkString "" $ reverse (c^.output)

oneMeg, twoMeg, memSize :: Int
oneMeg = 1048576
twoMeg = oneMeg * 2
memSize = 2048  -- twoMeg
rspStart :: Int64
rspStart = fromIntegral memSize * 8
zero :: Int64
zero = 0
registerStartState :: Map Register Int64
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
emptyMem :: Vector Int64
emptyMem = Vector.replicate memSize zero

newComputer :: Program x s -> Computer (Instruction x s)
newComputer p = Computer { 
  _registers = registerStartState,
  _memory    = emptyMem,
  _program   = Vector.fromList insts,
  _labels    = Map.map fromIntegral $ labelIndices insts,
  _output    = [],
  _ip        = 0,
  _heapP     = 0,
  _halted    = False 
} where insts = programToList p

-- set the value of a register to an int value
writeReg :: HasComputer c a => Register -> Int64 -> c -> c
writeReg reg newValue c = newReg (Map.insert reg newValue $ c^.registers) c

-- set the value of r1 to the value of r2
set :: HasComputer c a => Register -> Register -> c -> c
set r1 r2 c = writeReg r1 (readReg r2 c) c

-- read the value of a register
readReg :: HasComputer c a => Register -> c -> Int64
readReg r c = 
  maybe (error $ "error: unitialized register: " ++ show r) id (Map.lookup r $ c^.registers)

-- write an int into memory at the given address
writeMem :: HasComputer c a => String -> Int64 -> Int64 -> c -> c
writeMem caller addr value c = go where
  index = fromIntegral addr `div` 8
  go | index < memSize = newMem ((c^.memory) Vector.// [(index, value)]) c
     | otherwise = error $ caller ++ "tried to write out of bounds memory index: " ++ show index

-- read a single int from memory
readMem :: HasComputer c a => String -> Int64 -> c -> Int64
readMem caller addr c = go where
  index = fromIntegral addr `div` 8
  go | index < memSize = (c^.memory) Vector.! index
     | otherwise       = error $ caller ++ "tried to access out of bounds memory index: " ++ show index

-- read an array from memory
readArray :: HasComputer c a => Int64 -> c -> Vector Int64
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
push :: HasComputer c a => Int64 -> c -> c
push value c =
  let rspVal = readReg rsp c - 8
      c'     = writeReg rsp rspVal c
  in  writeMem "push" rspVal value c'

-- pop the top value off the stack into the given register
-- adjust rsp accordingly.
pop :: HasComputer c a => Register -> c -> c
pop r c =
  let rspVal = readReg rsp c
      c'     = writeReg r (readMem "pop" rspVal c) c
  in writeReg rsp (rspVal + 8) c'

encodeNum :: Int64 -> Int64
encodeNum n = shiftR n 1

-- TODO: test if heap runs into stack, and vice-versa
allocate :: HasComputer c a => Int64 -> Int64 -> c -> (Int64, c)
allocate size n c =
  let size'   = encodeNum size
      ns      = Prelude.replicate (fromIntegral size') n
      indices :: [Int]
      indices = [(fromIntegral $ c^.heapP `div` 8)..]
      nsWithIndices = zip indices $ size' : ns
      heap    = newMem ((c^.memory) Vector.// nsWithIndices) (c & heapP +~ ((size'+1)*8))
  in (c^.heapP, heap)

-- print a number or an array
--   if the int argument is an encoded int, prints the int
--   else it's an array, print the contents of the array (and recur)
print :: HasComputer c a => Int64 -> c -> c
print n c = addOutput (printContent n 0 ++ "\n") c where
  printContent :: Int64 -> Int -> String
  printContent n depth
    | depth >= 4   = "..."
    | n .&. 1 == 1 = show $ shiftR n 1
    | otherwise    =
      let size  = readMem "print" n c
          arr   = readArray n c
          contentsV = Vector.map (\n -> printContent n $ depth + 1) arr
          contents  = mkString ", " $ show size : Vector.toList contentsV
      in "{s:" ++ contents ++ "}"

-- print an array error
arrayError :: HasComputer c a => Int64 -> Int64 -> c -> c
arrayError a x c = haltWith msg c where
  pos  = show $ encodeNum x
  size = show $ readMem "arrayError" a c
  msg  = "attempted to use position " ++ pos ++ 
         " in an array that only has "++ size ++" positions"

findLabelIndex :: HasComputer c a => Label -> c -> Int64
findLabelIndex l c = maybe (error $ "no such label: " ++ l) id (Map.lookup l (c^.labels))

newReg :: HasComputer c a => RegisterState -> c -> c
newReg r c = c & registers .~ r


newMem :: HasComputer c a => Memory -> c -> c
newMem m c = c & memory .~ m

goto :: HasComputer c a => Ip -> c -> c
goto m c = c & ip .~ m

addOutput :: HasComputer c a => String -> c -> c
addOutput s c = c & output %~ (s:)

haltWith :: HasComputer c a => String -> c -> c
haltWith msg c = c & addOutput msg & halted .~ True

halt :: HasComputer c a => c -> c
halt c = c & halted .~ True

currentInst :: HasComputer c a => c -> a
currentInst c = go where
  ip' :: Int
  ip' = fromIntegral $ c^.ip
  go | ip' < memSize = (c^.program) Vector.! ip'
     | otherwise     = error $ "instruction out of bounds: " ++ show ip'

hasNextInst :: HasComputer c a => c -> Bool
hasNextInst c = (c^.ip) < (fromIntegral $ Vector.length (c^.program))

-- advance the computer to the next instruction
nextInst :: HasComputer c a => c -> c
nextInst c = goto (c^.ip + 1) c

-- the main loop, runs a computer until completion
runComputer :: HasComputer c a => (c -> c) -> c -> c
runComputer step = runIdentity . runComputerM (return . step)

-- the main loop, runs a computer until completion
runComputerM :: (HasComputer c a, Monad m) =>
  (c -> m c) -> c -> m c
runComputerM step c
  | c^.halted || not (hasNextInst c) = return c
  | otherwise = step c >>= runComputerM step
