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

import Control.Lens hiding (set)
import Control.Monad.Error.Class
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.ST.Class
import Data.Int
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Monoid()
import Data.Vector (Vector, freeze)
import qualified Data.Vector as Vector
import Prelude hiding (read)
import L.L1L2AST hiding (registers)
import L.Interpreter.Memory

type RegisterState = Map Register Int64
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
 ,frozenMemory    :: Vector Int64
 ,frozenHeap      :: Int64
}

instance HasMemory (Computer s a) s where memory = computerMem

freezeComputer :: MonadST m => Computer (World m) a -> m FrozenComputer
freezeComputer c = do
  m <- liftST $ freeze (_runMemory $ _computerMem c)
  return $ FrozenComputer (_registers c) m (_heapP $ _computerMem c)

type MonadComputer c m a = (HasComputer c (World m) a, MonadMemory c m a)

bind2 :: Monad m => (a -> b -> m c) -> m a -> m b -> m c
bind2 f ma mb = do a <- ma; b <- mb; f a b

rspStart :: Int64
rspStart = fromIntegral memSize * 8

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
writeReg :: (MonadState c m, HasComputer c v a) => Register -> Int64 -> m ()
writeReg reg newValue = registers.at reg ?= newValue

-- set the value of r1 to the value of r2
set :: (MonadState c m, HasComputer c v a) => Register -> Register -> m ()
set r1 r2 = (registers.at r1) <~ use (registers.at r2)

-- read the value of a register
readReg :: MonadComputer c m a => Register -> m Int64
readReg r = use (registers.at r) >>= maybe (throwError . Exceptional $ "error: unitialized register: " ++ show r) return

-- push the given int argument onto the top of the stack
-- adjust rsp accordingly
-- from: http://www.cs.virginia.edu/~evans/cs216/guides/x86.html
-- "push first decrements ESP by 4, then places its operand 
--  into the contents of the 32-bit location at address [ESP]."
push :: MonadComputer c m a => Int64 -> m ()
push value = do
  rspVal <- readReg rsp
  writeReg rsp (rspVal - 8)
  writeMem "push" (rspVal - 8) value

-- pop the top value off the stack into the given register
-- adjust rsp accordingly.
pop :: MonadComputer c m a => Register -> m ()
pop r = do
  rspVal <- readReg rsp
  s      <- readMem "pop" rspVal
  writeReg r s
  writeReg rsp (rspVal + 8)

findLabelIndex :: MonadComputer c m a => Label -> m Int64
findLabelIndex l = use (labels.at l) >>= maybe (exception $ "no such label: " ++ l) return

goto :: (MonadState c m, HasComputer c v a) => Ip -> m ()
goto i = ip .= i

currentInst :: MonadComputer c m a => m a
currentInst = do
  ip' <- uses ip fromIntegral
  p   <- use program
  when (ip' >= memSize || ip' < 0) halt
  return $ p Vector.! ip'

hasNextInst :: (MonadState c m, HasComputer c v a) => m Bool
hasNextInst = do
  ip' <- use ip
  p   <- use program
  return $ ip' < (fromIntegral $ Vector.length p)

-- advance the computer to the next instruction
nextInst :: (MonadState c m, HasComputer c v a) => m ()
nextInst = ip += 1

-- goto the next instruction after writing a register
nextInstWR :: MonadComputer c m a => Register -> Int64 -> m ()
nextInstWR r i = writeReg r i >> nextInst

-- the main loop, runs a computer until completion
runComputer :: (MonadOutput m, MonadComputer c m a) => (a -> m ()) -> m ()
runComputer step = forever $ currentInst >>= step
