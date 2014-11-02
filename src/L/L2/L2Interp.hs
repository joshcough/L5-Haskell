{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}

module L.L2.L2Interp where --(interpL2, runL2Computation) where

import Control.Applicative
import Control.Lens hiding (cons, set)
import Control.Monad.State
import Control.Monad.ST
import Control.Monad.ST.Class
import Control.Monad.Trans.Error
import Data.Int
import Data.List.NonEmpty hiding (length)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe
import Data.Vector.Generic.Mutable (length)
import L.Interpreter.Computer
import L.Interpreter.Memory
import L.Interpreter.Output
import L.Interpreter.Runtime
import L.L1L2AST
import L.L1L2MainAdjuster
import Prelude hiding (head, length, print, tail)

-- run the given L2 program to completion on a new computer
-- return the computer as the final result.
interpL2 :: L2 -> String
interpL2 p = runST $ show <$> runL2Computation p

-- Env, and some Env operations
type Env = Map Variable Runtime
replaceHeadEnv :: MonadState (NonEmpty a, t) m => a -> m ()
replaceHeadEnv e = do (es, c) <- get; put (e :| tail es, c)
addEnv :: MonadState (NonEmpty a, t) m => a -> m ()
addEnv e = do (es, c) <- get; put (cons e es, c)
showEnv :: Env -> String
showEnv = show . Map.map showRuntime

type CE m = (NonEmpty Env, Computer (World m) L2Instruction)

instance HasComputer r t a => HasComputer (l, r) t a where
  computer = _2.computer

instance HasMemory r a => HasMemory (l, r) a where
  memory = _2.memory

runL2Computation :: (MonadST m, Functor m) => L2 -> m (ComputationResult (FrozenComputer L2Instruction))
runL2Computation p = do
  c   <- newComputer $ adjustMain p
  let ce = (Map.empty :| [], c)
  (output, (haltEither, (_, comp))) <- runOutputT $ runStateT (runErrorT $ runComputer step) ce
  fzc <- freezeComputer comp
  return $ mkComputationResult (output, (haltEither, fzc))

step :: (MonadOutput m, MonadComputer (CE m) m L2Instruction) => L2Instruction -> m ()
step (Assign x (CompRHS (Comp s1 op s2))) = do
  s1' <- readNum s1
  s2' <- readNum s2
  nextInstWX x $ Num (if cmp op s1' s2' then 1 else 0)
step (Assign x1 (MemRead (MemLoc x2 offset))) = do
  x'    <- readX x2 >>= flip expectPointer "MemRead"
  index <- runOp (Pointer x') Increment (Num $ fromIntegral offset)
  readMem "MemRead" index >>= nextInstWX x1
step (Assign x (Allocate size datum)) =
  bind2 allocate (readS size) (readS datum) >>= nextInstWX x
step (Assign x (Print s)) = (readS s >>= print) >> nextInstWX x (Num 1)
step (Assign _ (ArrayError s1 s2)) = bind2 arrayError (readS s1) (readS s2)
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
  addEnv Map.empty
  goto func
step (TailCall s) = do
  loc <- readS s
  replaceHeadEnv Map.empty
  goto loc
step Return = do
  rspVal     <- readX rsp >>= flip expectPointer "Return"
  memLength  <- liftM (8*) $ uses memory (length . _runMemory)
  (_:|es, c) <- get
  let done = rspVal >= fromIntegral memLength
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
    (True,  xs) -> exception $
      "computer finished, but multiple environments exist" ++ concat (fmap showEnv xs)
    -- here we are trying to return
    -- and there are no more environments
    -- but the computer isnt in a finished state...
    -- there must be some programming error
    (False, []) -> exception "trying to return, but no environments remaining"
    -- we are returning, and the computer is not halted
    -- based on the last case, there must be enough environments remaining
    -- were looking at the tail, so we've already popped the top env off
    -- so just return what we have
    (False, x:xs) -> do
      put (x:|xs, c)
      newRspVal <- runOp (Pointer rspVal) Increment (Num 8)
      writeReg rsp newRspVal
      readMem "Return" (Pointer rspVal) >>= goto

-- goto the next instruction after writing an x value
nextInstWX :: MonadComputer (CE m) m a => L2X -> Runtime -> m ()
nextInstWX x i = writeX x i >> nextInst

readS :: MonadComputer (CE m) m a => L2S -> m Runtime
readS (NumberL2S n) = return $ Num n
readS (XL2S x)      = readX x
readS (LabelL2S l)  = return $ FunctionPointer l

readX :: MonadComputer (CE m) m a => L2X -> m Runtime
readX (RegL2X r) = readReg r
readX (VarL2X v) = do
  e <- (head . fst) <$> get
  maybe (exception $ "unbound variable: " ++ v) return (Map.lookup v e)

writeX :: MonadComputer (CE m) m a => L2X -> Runtime -> m ()
writeX (RegL2X r) i = writeReg r i
writeX (VarL2X v) i = (head . fst) <$> get >>= replaceHeadEnv . Map.insert v i

readNum :: (MonadOutput m, MonadComputer (CE m) m a) => L2S -> m Int64
readNum s = readS s >>= expectNum
