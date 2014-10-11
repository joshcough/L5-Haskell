{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}

module L.Computer where

import Control.Lens hiding (set)
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Trans.Error
import Control.Monad.Writer
import Data.Bits
import Data.Int
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Monoid()
import Data.Vector (Vector)
import qualified Data.Vector as Vector
import System.IO
import L.L1L2AST hiding (registers)
import L.Utils

type RegisterState = Map Register Int64
type Memory = Vector Int64
type Ip = Int64 -- instruction pointer

class Monad m => MonadOutput m where
  stdOut :: String -> m ()
  stdErr :: String -> m ()
  default stdOut :: (MonadTrans t, MonadOutput n, m ~ t n) => String -> m ()
  stdOut = lift . stdOut
  default stdErr :: (MonadTrans t, MonadOutput n, m ~ t n) => String -> m ()
  stdErr = lift . stdErr

instance MonadOutput IO where
  stdOut = hPutStr stdout
  stdErr = hPutStr stderr

instance Monad m => MonadOutput (OutputT m) where
  stdOut s = OutputT $ return ([StdOut s], ())
  stdErr s = OutputT $ return ([StdErr s], ())

instance MonadOutput m => MonadOutput (StateT s m)
instance MonadOutput m => MonadOutput (ReaderT s m)
instance (Error s, MonadOutput m)  => MonadOutput (ErrorT s m)
instance (Monoid w, MonadOutput m) => MonadOutput (WriterT w m)

data Output = StdOut String | StdErr String
outputText :: Output -> String
outputText (StdOut s) = s
outputText (StdErr s) = s

newtype OutputT m a = OutputT { runOutputT :: m ([Output], a) }

instance Functor f => Functor (OutputT f) where
  fmap fun (OutputT f) = OutputT $ fmap (\(xs, a) -> (xs, fun a)) f

instance Monad m => Monad (OutputT m) where
  return a = OutputT $ return ([], a)
  OutputT m >>= f = OutputT $ do
    (xs, a) <- m
    (ys, b) <- runOutputT $ f a
    return (xs ++ ys, b)

data Halt = Normal | Exceptional String
halt :: Monad m => ErrorT Halt m a
halt = throwError Normal
exception :: Monad m => String -> ErrorT Halt m a
exception s = throwError (Exceptional s)
data RunState = Running | Halted Halt

instance Error Halt where
  noMsg  = Normal
  strMsg = Exceptional

data ComputationState a = ComputationResult {
   _output    :: [Output]
  ,_haltState :: RunState
  ,_internals :: a
}
mkComputationState :: ([Output], (Either Halt (), a)) -> ComputationState a
mkComputationState (o, (Left  h , c)) = ComputationResult o (Halted h) c
mkComputationState (o, (Right (), c)) = ComputationResult o Running c

data Computer a = Computer {
   _registers :: RegisterState    -- contains runtime values (Int64 for L1/L2, but probably a data type for other languages)
 , _memory    :: Memory           -- "" (same as registers)
 , _program   :: Vector a         -- ok...non-linear programs (L3 and above) might have trouble here.
 , _labels    :: Map Label Int64  -- however, this could me a map from label to something else
 , _ip        :: Ip               -- Ip might mean nothing to L3 and above, but then again maybe theres a way to make use of it.
 , _heapP     :: Int64 -- pointer to top of heap  -- this is good across all languages!
} deriving Show
makeClassy ''Computer

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

newComputer :: (Show x, Show s) => Program x s -> Computer (Instruction x s)
newComputer p = Computer {
  _registers = registerStartState,
  _memory    = emptyMem,
  _program   = Vector.fromList insts,
  _labels    = Map.map fromIntegral $ labelIndices insts,
  _ip        = 0,
  _heapP     = 0
} where insts = programToList p

-- set the value of a register to an int value
writeReg :: (MonadState c m, HasComputer c a) => Register -> Int64 -> m ()
writeReg reg newValue = registers.at reg ?= newValue

-- set the value of r1 to the value of r2
set :: (MonadState c m, HasComputer c a) => Register -> Register -> m ()
set r1 r2 = (registers.at r1) <~ use (registers.at r2)

-- read the value of a register
readReg :: (MonadState c m, HasComputer c a) => Register -> ErrorT Halt m Int64
readReg r = use (registers.at r) >>= maybe (throwError . Exceptional $ "error: unitialized register: " ++ show r) return

-- write an int into memory at the given address
writeMem :: (MonadState c m, HasComputer c a) => String -> Int64 -> Int64 -> ErrorT Halt m ()
writeMem caller addr value = go where
  index = fromIntegral addr `div` 8
  go | index < memSize = memory.ix index .= value
     | otherwise = exception $ caller ++ "tried to write out of bounds memory index: " ++ show index

-- read a single int from memory
readMem :: (MonadState c m, HasComputer c a) => String -> Int64 -> ErrorT Halt m Int64
readMem caller addr = go where
  index = fromIntegral addr `div` 8
  go | index < memSize = use $ singular $ memory.ix index
     | otherwise       = exception $ caller ++ "tried to access out of bounds memory index: " ++ show index

-- read an array from memory
readArray :: (MonadState c m, HasComputer c a) => Int64 -> ErrorT Halt m (Vector Int64)
readArray addr = do
  s <- fromIntegral `liftM` readMem "readArray" addr
  let startIndex = fromIntegral addr `div` 8 + 1
  if startIndex + s < memSize
    then uses memory (Vector.slice startIndex s)
    else exception $ "readArray tried to access out of bounds memory index: " ++ show startIndex

-- push the given int argument onto the top of the stack
-- adjust rsp accordingly
-- from: http://www.cs.virginia.edu/~evans/cs216/guides/x86.html
-- "push first decrements ESP by 4, then places its operand 
--  into the contents of the 32-bit location at address [ESP]."
push :: (MonadState c m, HasComputer c a) => Int64 -> ErrorT Halt m ()
push value = do
  rspVal <- readReg rsp
  writeReg rsp (rspVal - 8)
  writeMem "push" (rspVal - 8) value

-- pop the top value off the stack into the given register
-- adjust rsp accordingly.
pop :: (MonadState c m, HasComputer c a) => Register -> ErrorT Halt m ()
pop r = do
  rspVal <- readReg rsp
  s      <- readMem "pop" rspVal
  writeReg r s
  writeReg rsp (rspVal + 8)

encodeNum :: Int64 -> Int64
encodeNum n = shiftR n 1

-- TODO: test if heap runs into stack, and vice-versa
allocate :: (MonadState c m, HasComputer c a) => Int64 -> Int64 -> m Int64
allocate size n = do
  hp <- use heapP
  let size'   = encodeNum size
      ns      = Prelude.replicate (fromIntegral size') n
      indices :: [Int]
      indices = [(fromIntegral $ hp `div` 8)..]
      nsWithIndices = zip indices $ size' : ns
  memory %= (Vector.// nsWithIndices)
  heapP <<+= ((size'+1) * 8)

-- print a number or an array
--   if the int argument is an encoded int, prints the int
--   else it's an array, print the contents of the array (and recur)
print :: forall m c a . (MonadState c m, MonadOutput m, HasComputer c a) => Int64 -> ErrorT Halt m ()
print n = printContent n 0 >>= \s -> stdOut (s ++ "\n") where
  printContent :: Int64 -> Int -> ErrorT Halt m String
  printContent n depth
    | depth >= 4   = return $ "..."
    | n .&. 1 == 1 = return . show $ shiftR n 1
    | otherwise    = do
      size      <- readMem "print" n
      arr       <- readArray n
      contentsV <- Vector.mapM (\n -> printContent n $ depth + 1) arr
      return $ "{s:" ++ (mkString ", " $ show size : Vector.toList contentsV) ++ "}"

-- print an array error
arrayError :: (MonadState c m, MonadOutput m, HasComputer c a) => Int64 -> Int64 -> ErrorT Halt m ()
arrayError a x = do
  size <- readMem "arrayError" a
  stdErr $ "attempted to use position " ++ show (encodeNum x) ++
           " in an array that only has " ++ show size ++ " positions"
  halt

findLabelIndex :: (MonadState c m, HasComputer c a) => Label -> ErrorT Halt m Int64
findLabelIndex l = use (labels.at l) >>= maybe (exception $ "no such label: " ++ l) return

goto :: (MonadState c m, HasComputer c a) => Ip -> m ()
goto i = ip .= i

currentInst :: (MonadState c m, HasComputer c a) => ErrorT Halt m a
currentInst = do
  ip' <- uses ip fromIntegral
  p   <- use program
  when (ip' >= memSize || ip' < 0) halt
  return $ p Vector.! ip'

hasNextInst :: (MonadState c m, HasComputer c a) => m Bool
hasNextInst = do
  ip' <- use ip
  p   <- use program
  return $ ip' < (fromIntegral $ Vector.length p)

-- advance the computer to the next instruction
nextInst :: (MonadState c m, HasComputer c a) => m ()
nextInst = ip += 1

-- goto the next instruction after writing a register
nextInstWR :: (MonadState c m, MonadOutput m, HasComputer c a) => Register -> Int64 -> m ()
nextInstWR r i = do writeReg r i; nextInst

-- the main loop, runs a computer until completion
-- todo: if we go to get next inst and it aint there, halt! (mzero)
runComputerM :: Monad m => m () -> m ()
runComputerM = forever
