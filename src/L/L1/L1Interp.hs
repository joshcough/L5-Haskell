{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}

module L.L1.L1Interp (interpL1) where

import Control.Applicative
import Control.Lens hiding (set)
import Control.Monad.State
import Control.Monad.ST
import Control.Monad.ST.Class
import Control.Monad.Trans.Error
import Data.Int
import Data.Vector.Generic.Mutable (length)
import L.Interpreter.Computer
import L.Interpreter.Memory
import L.Interpreter.Output
import L.Interpreter.Runtime
import L.L1L2AST
import L.L1L2MainAdjuster (adjustMain)
import Prelude hiding (length, print)

-- run the given L1 program to completion on a new computer
-- return the computer as the final result.
interpL1 :: L1 -> String
interpL1 p = runST $ show <$> runL1Computation p

runL1Computation :: (Functor m, MonadST m) => L1 -> m (ComputationResult FrozenComputer)
runL1Computation p = do
  c    <- newComputer $ adjustMain p
  (output, (haltEither, comp)) <- runOutputT $ runStateT (runErrorT $ runComputer step) c
  fzc  <- freezeComputer comp
  return $ mkComputationResult (output, (haltEither, fzc))

step :: (MonadOutput m, MonadComputer c m L1Instruction) => L1Instruction -> m ()
step (Assign r (CompRHS (Comp s1 op s2))) = do
  s1' <- readNum s1
  s2' <- readNum s2
  nextInstWR r $ if cmp op s1' s2' then Num 1 else Num 0
step (Assign r (MemRead (MemLoc x offset))) = do
  x'    <- readReg x
  index <- addRuntimes x' (Num $ fromIntegral offset)
  readMem "MemRead" index >>= nextInstWR r
step (Assign r (Allocate size datum)) = do
  hp <- bind2 allocate (readS size) (readS datum)
  nextInstWR r hp
step (Assign r (Print s)) = (readS s >>= print) >> nextInstWR r (Num 1)
step (Assign _ (ArrayError s1 s2)) = bind2 arrayError (readS s1) (readS s2)
step (Assign r (SRHS s)) = readS s >>= nextInstWR r
step (MathInst r op s) =
  bind2 (\v s' -> nextInstWR r $ Num $ runOp op v s') (readReg r >>= expectNum) (readS s >>= expectNum)
step (CJump (Comp s1 op s2) l1 l2) = do
  s1' <- readNum s1
  s2' <- readNum s2
  goto (FunctionPointer $ if cmp op s1' s2' then l1 else l2)
step (MemWrite (MemLoc x offset) s) = do
  x'    <- readReg x
  index <- addRuntimes x' (Num $ fromIntegral offset)
  readS s >>= writeMem "step MemWrite" index
  nextInst
step (Goto l) = goto (FunctionPointer l)
step (LabelDeclaration _) = nextInst
step (Call s) = do
  func <- readS s
  use ip >>= push . Num . (1+)
  goto func
step (TailCall s) = readS s >>= goto
step Return = do
  rspVal    <- readReg rsp >>= expectNum
  memLength <- liftM (8*) $ uses memory (length . _runMemory)
  newRspVal <- addRuntimes (Num rspVal) (Num 8)
  writeReg rsp newRspVal
  let done = rspVal >= fromIntegral memLength
  if done then halt else readMem "step Return" (Num rspVal) >>= goto

readS :: (MonadOutput m, MonadComputer c m a) => L1S -> m Runtime
readS (NumberL1S n) = return $ Num n
readS (RegL1S r)    = readReg r
readS (LabelL1S l)  = return $ FunctionPointer l

readNum :: (MonadOutput m, MonadComputer c m a) => L1S -> m Int64
readNum s = readS s >>= expectNum
