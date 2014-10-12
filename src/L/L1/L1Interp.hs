{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}

module L.L1.L1Interp (interpL1) where

import Control.Lens hiding (set)
import Control.Monad.State
import Control.Monad.Trans.Error
import Data.Int
import qualified Data.Vector as Vector
import L.Computer
import L.L1L2AST 
import L.L1L2MainAdjuster (adjustMain)
import Prelude hiding (print)

-- run the given L1 program to completion on a new computer
-- return the computer as the final result.
interpL1 :: L1 -> String
interpL1 p = handleResult . mkComputationState . runIdentity . runOutputT $ runStateT
  (runErrorT $ runComputer step) (newComputer $ adjustMain p)

-- TODO: we have the ability to distinguish between stdout and stderr
--       we shold be able to use that
--       but forcing us into a string here makes this difficult.
handleResult :: ComputationResult (Computer L1Instruction) -> String
handleResult (ComputationResult output (Halted Normal) c) =
  concat $ fmap outputText output
-- todo: maybe show output thus far for the follow error cases
handleResult (ComputationResult output (Halted (Exceptional msg)) c) =
  error msg
handleResult (ComputationResult output Running c) =
  error $ "computer still running: " ++ show c

step :: (MonadOutput m, MonadComputer c m L1Instruction) => L1Instruction -> m ()
step (Assign r (CompRHS (Comp s1 op s2))) =
  bind2 (\s1' s2' -> nextInstWR r $ if cmp op s1' s2' then 1 else 0) (readS s1) (readS s2)
step (Assign r (MemRead (MemLoc x offset))) = do
  x' <- readReg x
  let index = x' + fromIntegral offset
  readMem "MemRead" index >>= nextInstWR r
step (Assign r (Allocate size datum)) = do
  hp <- bind2 allocate (readS size) (readS datum)
  nextInstWR r hp
step (Assign r (Print s)) = (readS s >>= print) >> nextInstWR r 1
step (Assign _ (ArrayError s1 s2)) = bind2 arrayError (readS s1) (readS s2)
step (Assign r (SRHS s)) = readS s >>= nextInstWR r
step (MathInst r op s) =
  bind2 (\v s' -> nextInstWR r $ runOp op v s') (readReg r) (readS s)
step (CJump (Comp s1 op s2) l1 l2) = do
  li <- bind2 (\s1 s2 -> findLabelIndex $ if cmp op s1 s2 then l1 else l2) (readS s1) (readS s2)
  goto li
step (MemWrite (MemLoc x offset) s) = do
  x' <- readReg x
  let index = x' + fromIntegral offset
  readS s >>= writeMem "step MemWrite" index
  nextInst
step (Goto l) = findLabelIndex l >>= goto
step (LabelDeclaration _) = nextInst
step (Call s) = do
  func <- readS s
  use ip >>= push . (1+)
  goto func
step (TailCall s) = readS s >>= goto
step Return = do
  rspVal    <- readReg rsp
  memLength <- liftM (8*) $ uses memory Vector.length
  let done = rspVal >= fromIntegral memLength
  writeReg rsp (rspVal + 8)
  if done then halt else readMem "step Return" rspVal >>= goto

readS :: (MonadOutput m, MonadComputer c m a) => L1S -> m Int64
readS (NumberL1S n) = return n
readS (RegL1S r)    = readReg r
readS (LabelL1S l)  = findLabelIndex l
