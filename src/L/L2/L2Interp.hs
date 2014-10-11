{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module L.L2.L2Interp (interpL2) where

import Control.Applicative
import Control.Lens hiding (cons, set)
import Control.Monad.State
import Control.Monad.Trans.Error
import Data.Int
import Data.List.NonEmpty
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe
import qualified Data.Vector as Vector
--import Debug.Trace
import L.Computer
import L.L1L2AST
import L.L1L2MainAdjuster 
import Prelude hiding (head, print, tail)

-- run the given L2 program to completion on a new computer
-- return the computer as the final result.
interpL2 :: L2 -> String
interpL2 = concat . fmap outputText . fst . interpL2'

interpL2' :: L2 -> ([Output], (Either String ((), CE)))
interpL2' p = runIdentity $ runOutputT $ runErrorT $
  runStateT (runComputerM step) (newCE . newComputer $ adjustMain p)

data CE = CE (NonEmpty Env) (Computer L2Instruction)
newCE :: Computer L2Instruction -> CE
newCE c = CE (Map.empty :| []) c
envs :: CE -> NonEmpty Env
envs (CE es _) = es

instance HasComputer CE L2Instruction where
  computer f (CE envs c) = fmap (CE envs) (f c)

-- Env, and some Env operations
type Env = Map Variable Int64

replaceHeadEnv :: MonadState CE m => Env -> m ()
replaceHeadEnv e = do
  (CE es c) <- get
  put $ CE (e :| tail es) c

addEnv :: MonadState CE m => Env -> m ()
addEnv e = do (CE es c) <- get; put $ CE (cons e es) c

step :: (Functor m, MonadOutput m, MonadState CE m) =>  m ()
step = do
  instruction <- currentInst
  case instruction of
    -- Assignment statements
    (Assign x (CompRHS (Comp s1 op s2))) -> do
      s1' <- readS s1
      s2' <- readS s2
      nextInstWX x (if cmp op s1' s2' then 1 else 0)
    (Assign x1 (MemRead (MemLoc x2 offset))) -> do
      index <- (fromIntegral offset +) <$> readX x2
      m <- readMem "step MemRead" index
      nextInstWX x1 m
    (Assign x (Allocate size datum)) -> do
      size'  <- readS size
      datum' <- readS datum
      hp     <- allocate size' datum'
      nextInstWX x hp
    (Assign x (Print s)) -> do
      s' <- readS s
      print s'
      nextInstWX x 1
    (Assign _ (ArrayError s1 s2)) -> do
      s1' <- readS s1
      s2' <- readS s2
      arrayError s1' s2'
    (Assign x (SRHS s)) -> do
      s' <- readS s
      nextInstWX x s'
    -- Math Inst
    (MathInst x op s) -> do
      x' <- readX x
      s' <- readS s
      nextInstWX x (runOp op x' s')
    -- CJump
    (CJump (Comp s1 op s2) l1 l2) -> do
      s1' <- readS s1
      s2' <- readS s2
      li  <- findLabelIndex (if cmp op s1' s2' then l1 else l2)
      goto li
    -- MemWrite
    (MemWrite (MemLoc x offset) s) -> do
      index <- (fromIntegral offset +) <$> readX x
      s'    <- readS s
      writeMem "step MemWrite" index s'
      nextInst
    -- Goto
    (Goto l) -> findLabelIndex l >>= goto
    -- LabelDec, just advance
    (LabelDeclaration _) -> nextInst
    -- Call
    (Call s) -> do
      func <- readS s
      ip' <- use ip
      push (ip' + 1)
      addEnv $ Map.empty
      goto func
    -- TailCall
    (TailCall s) -> do
      loc <- readS s
      replaceHeadEnv Map.empty
      goto loc
    -- Return
    Return -> do
      rspVal <- readReg rsp
      m      <- use memory
      (CE (_:|es) c) <- get
      let done = rspVal >= (fromIntegral $ Vector.length m * 8) -- todo: this seems like readMem
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
        (True,  xs) -> fail $ "computer finished, but multiple environments exist" ++ show xs
        -- here we are trying to return
        -- and there are no more environments
        -- but the computer isnt in a finished state...
        -- there must be some programming error
        (False, []) -> fail $ "trying to return, but no environments remaining"
        -- we are returning, and the computer is not halted
        -- based on the last case, there must be enough environments remaining
        -- were looking at the tail, so we've already popped the top env off
        -- so just return what we have
        (False, (x:xs)) -> do
          put (CE (x:|xs) c)
          writeReg rsp (rspVal + 8)
          ip' <- readMem "step Return" rspVal
          goto ip'

-- goto the next instruction after writing an x value
nextInstWX :: (Functor m, MonadState CE m) => L2X -> Int64 -> m ()
nextInstWX x i = do writeX x i; nextInst

unbound :: Monad m => Variable -> m a
unbound v = fail $ "unbound variable: " ++ v

readS :: (Functor m, MonadState CE m) => L2S -> m Int64
readS (NumberL2S n) = return n
readS (XL2S x)      = readX x
readS (LabelL2S l)  = findLabelIndex l

readX :: (Functor m, MonadState CE m) => L2X -> m Int64
readX (RegL2X r) = readReg r
readX (VarL2X v) = do
  e <- (head . envs) <$> get
  maybe (unbound v) return (Map.lookup v e)

writeX :: (Functor m, MonadState CE m) => L2X -> Int64 -> m ()
writeX (RegL2X r) i = writeReg r i
writeX (VarL2X v) i = do e <- (head . envs) <$> get; replaceHeadEnv $ Map.insert v i e
