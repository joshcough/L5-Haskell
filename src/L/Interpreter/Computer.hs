{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}

module L.Interpreter.Computer where

import Control.Applicative
import Control.Lens hiding (set)
import Control.Monad.Error.Class
import Control.Monad.Reader
import Control.Monad.ST.Class
import Data.Int
import Data.List (intersperse)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Monoid()
import Data.Vector (Vector, freeze)
import qualified Data.Vector as Vector
import Prelude hiding (read)
import L.L1L2AST
import L.Interpreter.Memory
import L.Interpreter.Output
import L.Interpreter.Runtime

type RegisterState = Map Register Runtime
type Ip = Int64 -- instruction pointer

data ComputationResult a = ComputationResult {
   _output    :: [Output]
  ,_haltState :: RunState
  ,_internals :: a
} deriving (Functor)

mkComputationResult :: ([Output], (Either Halt (), a)) -> ComputationResult a
mkComputationResult (o, (Left  h , c)) = ComputationResult o (Halted h) c
mkComputationResult (o, (Right (), c)) = ComputationResult o Running c

data Computer s a = Computer {
   _registers   :: RegisterState    -- contains runtime values (Int64 for L1/L2, but probably a data type for other languages)
 , _computerMem :: Memory s         -- "" (same as registers)
 , _program     :: Vector a         -- ok...non-linear programs (L3 and above) might have trouble here.
 , _labels      :: Map Label Int64  -- however, this could me a map from label to something else
 , _ip          :: Ip               -- Ip might mean nothing to L3 and above, but then again maybe theres a way to make use of it.
}
makeClassy ''Computer

data FrozenComputer = FrozenComputer {
  frozenRegisters :: RegisterState
 ,frozenMemory    :: Vector Runtime
 ,frozenHeapP     :: Int64
}

-- TODO: we have the ability to distinguish between stdout and stderr
--       we shold be able to use that
--       but forcing us into a string here makes this difficult.
instance Show (ComputationResult FrozenComputer) where
  -- no need to show anything other than the output, if halted normally.
  show (ComputationResult output (Halted Normal) _) = concat $ fmap outputText output
  show (ComputationResult output rs c) = concat $ intersperse "\n" [
    "Output:    " ++ (concat $ fmap outputText output),
    "Run State: " ++ (show rs),
    "Registers: " ++ (show . Map.map showRuntime . Map.filter ((/=) (Num 0)) $ frozenRegisters c),
    "Memory:    " ++ (show . map showRuntime . take 10 . Vector.toList $ frozenMemory c),
    "Heap Ptr:  " ++ (show $ frozenHeapP c)]

instance HasMemory (Computer s a) s where memory = computerMem

freezeComputer :: MonadST m => Computer (World m) a -> m FrozenComputer
freezeComputer c = do
  m <- liftST $ freeze (_runMemory $ _computerMem c)
  return $ FrozenComputer (_registers c) m (_heapP $ _computerMem c)

type MonadComputer c m a = (HasComputer c (World m) a, MonadMemory c m)

bind2 :: Monad m => (a -> b -> m c) -> m a -> m b -> m c
bind2 f ma mb = do a <- ma; b <- mb; f a b

rspStart :: Int64
rspStart = fromIntegral memSize * 8

registerStartState :: Map Register Runtime
registerStartState = Map.insert rsp (Num rspStart) $ Map.fromList $ zip registersList (repeat $ Num 0)

newComputer :: (MonadST m, Show x, Show s) => Program x s -> m (Computer (World m) (Instruction x s))
newComputer p = do
  m <-  newMem
  return $ Computer {
    _registers   = registerStartState,
    _computerMem = m,
    _program     = Vector.fromList insts,
    _labels      = Map.map fromIntegral $ labelIndices insts,
    _ip          = 0
  } where insts = programToList p

-- set the value of a register to an int value
writeReg :: MonadComputer c m a => Register -> Runtime -> m ()
writeReg reg newValue = registers.at reg ?= newValue

-- set the value of r1 to the value of r2
set :: MonadComputer c m a => Register -> Register -> m ()
set r1 r2 = (registers.at r1) <~ use (registers.at r2)

-- read the value of a register
readReg :: MonadComputer c m a => Register -> m Runtime
readReg r = use (registers.at r) >>= maybe (throwError . Exceptional $ "error: unitialized register: " ++ show r) return

-- push the given int argument onto the top of the stack
-- adjust rsp accordingly
-- from: http://www.cs.virginia.edu/~evans/cs216/guides/x86.html
-- "push first decrements ESP by 4, then places its operand 
--  into the contents of the 32-bit location at address [ESP]."
push :: MonadComputer c m a => Runtime -> m ()
push value = do
  rspVal  <- readReg rsp
  rspVal' <- expectNum rspVal
  writeReg rsp (Num $ rspVal' - 8)
  writeMem "push" (Num $ rspVal' - 8) value

-- pop the top value off the stack into the given register
-- adjust rsp accordingly.
pop :: MonadComputer c m a => Register -> m ()
pop r = do
  rspVal  <- readReg rsp
  rspValN <- expectNum rspVal
  s      <- readMem "pop" rspVal
  writeReg r s
  writeReg rsp (Num $ rspValN + 8)

findLabelIndex :: MonadComputer c m a => Label -> m Int64
findLabelIndex l = use (labels.at l) >>= maybe (exception $ "no such label: " ++ l) return

goto :: MonadComputer c m a => Runtime -> m ()
goto (Num i) = ip .= i
goto (Pointer p) = exception $ "goto called with pointer: " ++ show p
goto (FunctionPointer l) = do i <- findLabelIndex l; ip .= i

currentInst :: MonadComputer c m a => m a
currentInst = do
  ip' <- uses ip fromIntegral
  p   <- use program
  when (ip' >= memSize || ip' < 0) halt
  return $ p Vector.! (fromIntegral ip')

hasNextInst :: MonadComputer c m a => m Bool
hasNextInst = do
  ip' <- use ip
  p   <- use program
  return $ ip' < (fromIntegral $ Vector.length p)

-- advance the computer to the next instruction
nextInst :: MonadComputer c m a => m ()
nextInst = ip += 1

-- goto the next instruction after writing a register
nextInstWR :: MonadComputer c m a => Register -> Runtime -> m ()
nextInstWR r i = writeReg r i >> nextInst

-- the main loop, runs a computer until completion
runComputer :: (MonadOutput m, MonadComputer c m a) => (a -> m ()) -> m ()
runComputer step = forever $ currentInst >>= step
