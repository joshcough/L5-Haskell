{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}

module L.L1.L1Interp (interpL1) where

import Control.Applicative
import Control.Lens hiding (set)
import Control.Monad.State
import Control.Monad.ST
import Control.Monad.ST.Class
import Control.Monad.Trans.Error
import Data.Bits
import Data.Int
import Data.Vector.Generic.Mutable (length)
import L.Interpreter.ComputationResult
import L.Interpreter.Memory
import L.Interpreter.Output
import L.Interpreter.Runtime
import L.Interpreter.X86Computer
import L.L1.L1L2AST
import L.L1.L1L2MainAdjuster (adjustMain)
import L.Registers
import L.Util.Utils (bind2)
import Prelude hiding (length, print)

-- run the given L1 program to completion on a new computer
-- return the computer as the final result.
interpL1 :: L1 -> String
interpL1 p = runST $ show <$> runL1Computation p

runL1Computation :: (Functor m, MonadST m) => L1 -> m (ComputationResult (FrozenX86Computer L1Instruction))
runL1Computation (L1 p) = do
  c    <- newX86Computer $ adjustMain p
  (output, (haltEither, comp)) <- runOutputT $ runStateT (runErrorT $ runX86Computer step) c
  fzc  <- freezeX86Computer comp
  return $ mkComputationResult (output, (haltEither, fzc))

step :: (MonadOutput m, MonadX86Computer c m L1Instruction) => L1Instruction -> m ()
step (Assign x (CompRHS (Comp s1 op s2))) = do
  s1' <- readNum s1
  s2' <- readNum s2
  nextInstWX x $ Num (if cmp op s1' s2' then 1 else 0)
step (Assign x1 (MemRead (MemLoc x2 offset))) = do
  x'    <- readX x2 >>= expectPointer "MemRead"
  index <- runOp (Pointer x') Increment (Num $ fromIntegral offset)
  readMem "MemRead" index >>= nextInstWX x1
step (Assign x (Allocate size datum)) =
  bind2 allocate (readS size >>= encodeNum) (readS datum) >>= nextInstWX x
step (Assign x (Print s)) = readS s >>= print True >>= nextInstWX x
step (Assign _ (ArrayError s1 s2)) = bind2 arrayError (readS s1) (readS s2 >>= encodeNum)
step (Assign r (SRHS s)) = readS s >>= nextInstWX r
step (MathInst x op s) = do
  xv     <- readX x
  sv     <- readS s
  newVal <- runOp xv op sv
  nextInstWX x newVal
step (CJump (Comp s1 op s2) l1 l2) = do
  s1' <- readNum s1
  s2' <- readNum s2
  goto (FunctionPointer $ if cmp op s1' s2' then l1 else l2)
step (MemWrite (MemLoc x offset) s) = do
  x'    <- readX x
  index <- runOp x' Increment (Num $ fromIntegral offset)
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
  rspVal    <- readX rsp >>= expectPointer "Return"
  memLength <- (8*) <$> uses memory (length . _runMemory)
  newRspVal <- runOp (Pointer rspVal) Increment (Num 8)
  writeX rsp newRspVal
  let done = rspVal >= fromIntegral memLength
  if done then halt else readMem "Return" (Pointer rspVal) >>= goto

-- goto the next instruction after writing a register
nextInstWX :: MonadX86Computer c m a => Register -> Runtime -> m ()
nextInstWX r i = writeX r i >> nextInst

readS :: (MonadOutput m, MonadX86Computer c m a) => L1S -> m Runtime
readS (NumberL1S n) = return $ Num n
readS (RegL1S r)    = readX r
readS (LabelL1S l)  = return $ FunctionPointer l

readX :: MonadX86Computer c m a => Register -> m Runtime
readX = readReg

writeX :: MonadX86Computer c m a => Register -> Runtime -> m ()
writeX = writeReg

readNum :: (MonadOutput m, MonadX86Computer c m a) => L1S -> m Int64
readNum s = readS s >>= expectNum

encodeNum :: MonadX86Computer c m a => Runtime -> m Runtime
encodeNum (Num n) = return . Num $ shiftR n 1
encodeNum r = exception $ "tried to encode non-number: " ++ show r
