{-# LANGUAGE FlexibleContexts #-}

module L.L1.L1Interp (interpL1) where

import Control.Exception.Base
import Control.Lens hiding (set)
import Control.Monad.State
import Data.Int
import qualified Data.Vector as Vector
--import Debug.Trace
import L.Computer
import L.L1L2AST 
import L.L1L2MainAdjuster (adjustMain)
import Prelude hiding (print)

-- run the given L1 program to completion on a new computer
-- return the computer as the final result.
interpL1 :: L1 -> String
interpL1 p = concat . reverse . fst . runIdentity . runOutputT $
  runStateT (runComputerM step) (newComputer $ adjustMain p)

--`catch` \(e :: SomeException) -> something?

step :: (MonadState c m, MonadOutput m, HasComputer c L1Instruction) => m ()
step = do
  instruction <- currentInst
  case instruction of
    -- Assignment statements
    (Assign r (CompRHS (Comp s1 op s2))) -> do
      s1' <- readS s1
      s2' <- readS s2
      nextInstWR r (if cmp op s1' s2' then 1 else 0)
    (Assign r (MemRead (MemLoc x offset))) -> do
      x' <- readReg x
      let index = x' + fromIntegral offset
      memVal <- readMem "MemRead" index
      nextInstWR r memVal
    (Assign r (Allocate size datum)) -> do
      s  <- readS size
      d  <- readS datum
      hp <- allocate s d
      nextInstWR r hp
    (Assign r (Print s)) -> do
      readS s >>= print
      nextInstWR r 1
    (Assign _ (ArrayError s1 s2)) -> do
      s1' <- readS s1
      s2' <- readS s2
      arrayError s1' s2'
    (Assign r (SRHS s)) -> readS s >>= nextInstWR r
    -- Math Inst
    (MathInst r op s) -> do
      r' <- readReg r
      s' <- readS s
      nextInstWR r (runOp op r' s')
    -- CJump
    (CJump (Comp s1 op s2) l1 l2) -> do
      s1' <- readS s1
      s2' <- readS s2
      li  <- findLabelIndex (if cmp op s1' s2' then l1 else l2)
      goto li
    -- MemWrite
    (MemWrite (MemLoc x offset) s) -> do
      x' <- readReg x
      let index = x' + fromIntegral offset
      readS s >>= writeMem "step MemWrite" index
      nextInst
    -- Goto
    (Goto l) -> findLabelIndex l >>= goto
    -- LabelDec, just advance
    (LabelDeclaration _) -> nextInst
    -- Call
    (Call s) -> do
      func <- readS s
      ip'  <- use ip
      push (ip' + 1)
      goto func
    -- TailCall
    (TailCall s) -> readS s >>= goto
    -- Return
    Return -> do
      rspVal    <- readReg rsp
      memLength <- liftM (8*) $ uses memory Vector.length
      let done = rspVal >= fromIntegral memLength
      writeReg rsp (rspVal + 8)
      if done then halt else readMem "step Return" rspVal >>= goto

readS :: (MonadState c m, MonadOutput m, HasComputer c L1Instruction) => L1S -> m Int64
readS (NumberL1S n) = return n
readS (RegL1S r)    = readReg r
readS (LabelL1S l)  = findLabelIndex l
