{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}

module L.L2.L2Interp (interpL2, runL2Computation) where

import Control.Applicative
import Control.Lens hiding (cons, set)
import Control.Lens.Operators
import Control.Monad.State
import Control.Monad.ST
import Control.Monad.ST.Class
import Control.Monad.Trans.Error
import Data.Int
import Data.List.NonEmpty hiding (length)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe
import qualified Data.Vector as Vector
import Data.Vector.Generic.Mutable (length)
import Data.Vector.Mutable (STVector)
import qualified Data.Vector.Mutable as MV
--import Debug.Trace
import L.Computer
import L.L1L2AST
import L.L1L2MainAdjuster 
import Prelude hiding (head, length, print, tail)

-- run the given L2 program to completion on a new computer
-- return the computer as the final result.
interpL2 :: L2 -> String
interpL2 p = runST $ handleResult <$> runL2Computation p

handleResult :: ComputationResult FrozenL2Computer -> String
handleResult (ComputationResult output (Halted Normal) _) =
  concat $ fmap outputText output
-- todo: maybe show output thus far for the following error cases
handleResult (ComputationResult output (Halted (Exceptional msg)) c) =
  error msg -- todo: maybe show output thus far
handleResult (ComputationResult output Running c) =
  error $ "computer still running "

-- Env, and some Env operations
type Env = Map Variable Int64
replaceHeadEnv e = do (es, c) <- get; put $ (e :| tail es, c)
addEnv e = do (es, c) <- get; put $ (cons e es, c)

type RunningL2Computer m = RunningComputer m L2Instruction
type FrozenL2Computer    = FrozenComputer L2Instruction
type CE m = (NonEmpty Env, RunningL2Computer m)

instance HasComputer r t a => HasComputer (l, r) t a where
  computer = _2.computer

runL2Computation p = do
  c   <- (newComputer $ adjustMain p)
  let ce = (Map.empty :| [], c)
  blah@(output, (haltEither, (_, comp))) <- runOutputT $ runStateT (runErrorT $ runComputer step) ce
  fzc <- freezeComputer comp
  return $ mkComputationResult (output, (haltEither, fzc))

step :: (MonadOutput m, MonadComputer (CE m) m L2Instruction) => L2Instruction -> m ()
step (Assign x (CompRHS (Comp s1 op s2))) = do
  bind2 (\s1 s2 -> nextInstWX x $ if cmp op s1 s2 then 1 else 0) (readS s1) (readS s2)
step (Assign x1 (MemRead (MemLoc x2 offset))) = do
  index <- readX x2 <&> (+ fromIntegral offset)
  readMem "step MemRead" index >>= nextInstWX x1
step (Assign x (Allocate size datum)) =
  bind2 allocate (readS size) (readS datum) >>= nextInstWX x
step (Assign x (Print s)) = (readS s >>= print) >> nextInstWX x 1
step (Assign _ (ArrayError s1 s2)) = bind2 arrayError (readS s1) (readS s2)
step (Assign x (SRHS s)) = readS s >>= nextInstWX x
step (MathInst x op s) =
  bind2 (\x' s' -> nextInstWX x $ runOp op x' s') (readX x) (readS s)
step (CJump (Comp s1 op s2) l1 l2) = do
  li <- bind2 (\s1 s2 -> findLabelIndex $ if cmp op s1 s2 then l1 else l2) (readS s1) (readS s2)
  goto li
step (MemWrite (MemLoc x offset) s) = do
  index <- readX x <&> (+ fromIntegral offset)
  readS s >>= writeMem "step MemWrite" index
  nextInst
step (Goto l) = findLabelIndex l >>= goto
step (LabelDeclaration _) = nextInst
step (Call s) = do
  func <- readS s
  use ip >>= push . (1+)
  addEnv $ Map.empty
  goto func
step (TailCall s) = do
  loc <- readS s
  replaceHeadEnv Map.empty
  goto loc
step Return = do
  rspVal <- readReg rsp
  memLength <- liftM (8*) $ uses memory (fromIntegral . length)
  (_:|es, c) <- get
  let done = rspVal >= memLength
  case (done, es) of
    -- in this case, we must be returning from main
    -- the computer thinks it's finished
    -- there are no environments left
    -- it's ok to halt the computer
    (True,  []) -> halt
    -- in this case we are trying to return
    -- an we think the computer is finished,
    -- but there are extra environments remaining
    -- there must be some programming error
    (True,  xs) -> exception $ "computer finished, but multiple environments exist" ++ show xs
    -- here we are trying to return
    -- and there are no more environments
    -- but the computer isnt in a finished state...
    -- there must be some programming error
    (False, []) -> exception $ "trying to return, but no environments remaining"
    -- we are returning, and the computer is not halted
    -- based on the last case, there must be enough environments remaining
    -- were looking at the tail, so we've already popped the top env off
    -- so just return what we have
    (False, x:xs) -> do
      put (x:|xs, c)
      writeReg rsp (rspVal + 8)
      readMem "step Return" rspVal >>= goto

-- goto the next instruction after writing an x value
nextInstWX :: MonadComputer (CE m) m a => L2X -> Int64 -> m ()
nextInstWX x i = do writeX x i; nextInst

readS :: MonadComputer (CE m) m a => L2S -> m Int64
readS (NumberL2S n) = return n
readS (XL2S x)      = readX x
readS (LabelL2S l)  = findLabelIndex l

readX :: MonadComputer (CE m) m a => L2X -> m Int64
readX (RegL2X r) = readReg r
readX (VarL2X v) = do
  e <- (head . fst) <$> get
  maybe (exception $ "unbound variable: " ++ v) return (Map.lookup v e)

writeX :: MonadComputer (CE m) m a => L2X -> Int64 -> m ()
writeX (RegL2X r) i = writeReg r i
writeX (VarL2X v) i = (head . fst) <$> get >>= replaceHeadEnv . Map.insert v i
