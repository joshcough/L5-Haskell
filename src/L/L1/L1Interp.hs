{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DeriveFunctor #-}

module L.L1.L1Interp where


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

type RegisterState = Map Register Int32
type Memory = Vector Int32
type Output = [String]
type Ip = Int32 -- instruction pointer

data Computer = Computer {
   _registers :: RegisterState
 , _memory    :: Memory
 , _program   :: Vector L1Instruction
 , _labels    :: Map L1Instruction Int32
 , _output    :: Output
 , _ip        :: Ip
 , _heapP     :: Int32 -- pointer to top of heap
 , _halted    :: Bool
} deriving Show

makeClassy ''Computer

{-
data Result a 
  = OK !Computer a
  | Halted !Computer
  deriving (Functor, Show)

instance HasComputer (Result a) where
  computer f (OK c a) = (`OK` a) <$> f c
  computer f (Halted c) = Halted <$> f c

newtype M s a = M { unM :: Computer -> ST s (Result a) }
  deriving Functor

instance Applicative (M s) where
  pure = return
  (<*>) = ap

instance Monad (M s) where
  return a = M $ \c -> return $ OK c a
  fail _ = M $ \c -> return $ Halted c
  M m >>= f = M $ \c -> m c >>= \r -> case r of
    OK c' a   -> unM (f a) c'
    Halted c' -> return $ Halted c'

instance MonadState Computer (M s) where
  get = M $ \c -> return $ OK c c
  put c = M $ \_ -> return $ OK c ()

runM :: (forall s. M s a) -> Computer -> Result a
runM m c = runST (unM m c)
-}

showOutput c = mkString "" $ reverse (c^.output)

oneMeg = 1048576
twoMeg = oneMeg * 2
memSize = 2048 :: Int -- twoMeg
ebpStart = ((fromIntegral memSize - 1) * 4) :: Int32
espStart = ebpStart
zero = 0 :: Int32
registerStartState = Map.fromList [(eax, zero), (ebx, zero), (ecx, zero), (edx, zero),
                                   (edi, zero), (esi, zero), (ebp, ebpStart), (esp, espStart)]
emptyMem = Vector.replicate memSize (0::Int32)
newComputer (Program main fs) = Computer rss emptyMem prog indices [] 0 0 False where
  rss = registerStartState
  -- put all the instructions into a single vector
  insts = Prelude.concat $ (body main ++ [Return]) : (fmap body fs)
  prog :: Vector L1Instruction
  prog = Vector.fromList insts
  indices :: Map L1Instruction Int32
  indices = Map.fromList $ fmap f $ zipWithIndex insts where
    f (x,i) = (x, fromIntegral i)

-- set the value of a register to an int value
writeReg :: Register -> Int32 -> Computer -> Computer
writeReg reg newValue c = newReg (Map.insert reg newValue $ c^.registers) c

-- set the value of r1 to the value of r2
set :: Register -> Register -> Computer -> Computer
set r1 r2 c = writeReg r1 (readReg r2 c) c

-- read the value of a register
readReg :: Register -> Computer -> Int32
readReg r c = 
  maybe (error $ "error: unitialized register: " ++ (show r)) id (Map.lookup r $ c^.registers)

-- write an int into memory at the given address
writeMem :: Int32 -> Int32 -> Computer -> Computer
writeMem addr value c = newMem ((c^.memory) Vector.// [(fromIntegral addr `div` 4, value)]) c

-- read a single int from memory
readMem :: Int32 -> Computer -> Int32
readMem addr c = (c^.memory) Vector.! (fromIntegral addr `div` 4)

-- read an array from memory
readArray :: Int32 -> Computer -> Vector Int32
readArray addr c = 
  let size = readMem addr c
  in Vector.slice (fromIntegral addr `div` 4 + 1) (fromIntegral size) (c^.memory)

-- push the given int argument onto the top of the stack
-- adjust esp accordingly
-- from: http://www.cs.virginia.edu/~evans/cs216/guides/x86.html
-- "push first decrements ESP by 4, then places its operand 
--  into the contents of the 32-bit location at address [ESP]."
push :: Int32 -> Computer -> Computer
push value c =
  let espVal = readReg esp c - 4
      c'     = writeReg esp espVal c
  in  writeMem espVal value c'

-- pop the top value off the stack into the given register
-- adjust esp accordingly.
pop :: Register -> Computer -> Computer
pop r c =
  let espVal   = readReg esp c
      c' = writeReg r (readMem espVal c) c
  in writeReg esp (espVal + 4) c'

ret :: Computer -> Computer
ret c = 
  let espVal = readReg esp c
      done   = espVal >= (fromIntegral $ Vector.length (c^.memory) * 4)
      c'     = writeReg esp (espVal + 4) c
      c''    = goto (readMem espVal c') c'
  in if done then halt c else c''

adjustNum :: Int32 -> Int32
adjustNum n = shiftR n 1

-- TODO: test if heap runs into stack, and vice-versa
allocate :: Int32 -> Int32 -> Computer -> (Int32, Computer)
allocate size n c =
  let size'   = adjustNum size
      ns :: [Int32]
      ns      = Prelude.replicate (fromIntegral size') n
      indices :: [Int]
      indices = [(fromIntegral $ c^.heapP `div` 4)..]
      heap    = newMem ((c^.memory) Vector.// (zip indices $ size' : ns))
                       (c & heapP +~ ((size'+1)*4))
  in (c^.heapP, heap)

-- print a number or an array
--   if the int argument is an encoded int, prints the int
--   else it's an array, print the contents of the array (and recur)
l1print :: Int32 -> Computer -> Computer
l1print n c = addOutput (printContent n 0 ++ "\n") c where
  printContent :: Int32 -> Int32 -> String
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
arrayError :: Int32 -> Int32 -> Computer -> Computer
arrayError a x c = haltWith msg c where
  pos  = show $ adjustNum x
  size = show $ readMem a c
  msg  = "attempted to use position " ++ pos ++ 
         " in an array that only has "++ size ++" positions"

findLabelIndex :: Label -> Computer -> Int32
findLabelIndex l c =
  maybe (error $ "no such label: " ++ l) id (Map.lookup (LabelDeclaration l) (c^.labels))

readS :: L1S -> Computer -> Int32
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
currentInst c = (c^.program) Vector.! (fromIntegral $ c^.ip)
hasNextInst c = (c^.ip) < (fromIntegral $ Vector.length (c^.program))
-- advance the computer to the next instruction
nextInst :: Computer -> Computer
nextInst c = goto (c^.ip + 1) c
-- goto the next instruction after writing a register
nextInstWR :: Register -> Int32 -> Computer -> Computer
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

interpL1 :: String -> Either String String
interpL1 code = showOutput . interp <$> parseL164 (sread code)

interpL1OrDie :: String -> String
interpL1OrDie = (either error id) . interpL1

interpL1File = 
  do s <- compile_ $ (either error id) . interpL1
     putStrLn (snd s)
