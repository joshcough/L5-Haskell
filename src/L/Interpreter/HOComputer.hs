{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}

-- Higher Order Computer
module L.Interpreter.HOComputer where

import Control.Applicative
import Control.Arrow (second)
import Control.Lens
import Control.Monad.Error.Class
import Control.Monad.State
import Control.Monad.ST.Class
import Control.Monad.Trans.Error
import Data.List
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe
import Data.Vector (Vector)
import qualified Data.Vector as Vector
import L.Interpreter.ComputationResult
import L.Interpreter.Memory
import L.Interpreter.Output
import L.Interpreter.Runtime
import L.Primitives (Label(..))
import L.Variable
import Prelude hiding (print)

type Env e = Map Variable (Runtime e)
type Lib func = Map Label func

data HOComputer s e a = HOComputer { _env :: Env e, _mem :: Memory s a }
makeClassy ''HOComputer

data FrozenHOComputer e a = FrozenHOComputer { _envFrozen :: Env e, _memFrozen :: Vector (Runtime a) }
makeClassy ''FrozenHOComputer

instance HasMemory (HOComputer s e a) s a where memory = mem
type MonadHOComputer c m e a = (MonadFix m, Applicative m, HasHOComputer c (World m) e a, MonadMemory c m a, MonadOutput m)

-- numbers not encoded, and not word indexed.
hoMemConfig :: MemoryConfig
hoMemConfig = MemoryConfig False False

newHOComputer :: (Functor m, MonadST m) => m (HOComputer (World m) r a)
newHOComputer = HOComputer (Map.empty) <$> newMem hoMemConfig

runComputation :: (MonadST m, Functor m) =>
  ErrorT Halt (StateT (HOComputer (World m) e a) (OutputT m)) b ->
  m (ComputationResult (FrozenHOComputer e a))
runComputation compuation = do
  c <- newHOComputer
  (output, (eHaltRuntime, finalComputer)) <- runOutputT $ flip runStateT c $ runErrorT $ compuation
  mem <- freezeMem $ finalComputer^.mem
  return $ mkComputationResult (output, (eHaltRuntime, FrozenHOComputer (finalComputer^.env) mem))

-- | like local on reader, only in my state monad.
-- | notice that putEnv is hidden here, so that it can't be used unsafely.
locally :: (HasHOComputer s s1 e a, MonadState s m) => (Env e -> Env e) -> m b -> m b
locally modifyEnv action = do
  e   <- use env
  env .= modifyEnv e
  res <- action
  env .= e
  return res

-- TODO: we have the ability to distinguish between stdout and stderr
-- TODO: this doesnt show the heap pointer!
--       we shold be able to use that
--       but forcing us into a string here makes this difficult.
instance (Show r, Eq a, Show a) => Show (ComputationResult (FrozenHOComputer r a)) where
  {-
    normally in L3, thec computer thinks it's still running.
    Halted Normal only happens on an expected arrayError.
    I could make the computer halt normally upon termination of execution,
    but unfortunately that would cause the final result to be lost
    TODO: however, i'm not yet even using the final result...should i be?
  -}
  show (ComputationResult output Running _)         = concat $ fmap outputText output
  show (ComputationResult output (Halted Normal) _) = concat $ fmap outputText output
  show (ComputationResult output rs c) =intercalate "\n" [
    "Output:      " ++ concat (fmap outputText output),
    "Run State:   " ++ show rs,
    "Memory!=0:   " ++ showMem (c^.memFrozen),
    "Environment: " ++ showEnv (c^.envFrozen) ] where
    --"Heap Ptr:   " ++ show (frozenHeapP c) ] where

showMem :: (Eq a, Show a) => Vector (Runtime a) -> String
showMem m = show $ map (second show) memList where
  memList = filter (\(_,r) -> Num 0 /= r) . zip [0..] $ Vector.toList m

showEnv :: Show e => Env e -> String
showEnv = show . Map.map show

envLookup :: (Monad m, MonadError Halt m) => Variable -> Env e -> m (Runtime e)
envLookup = forceLookup "variable" show

libLookup :: (Monad m, MonadError Halt m) => Label -> Lib f -> m f
libLookup l@(Label name) = forceLookup "function" (const name) l

forceLookup :: (Ord k, Monad m, MonadError Halt m) => String -> (k -> String) -> k -> Map k v -> m v
forceLookup kName kShow k m =
  maybe (exception $ concat ["unbound ", kName, ": ", kShow k]) return (Map.lookup k m)
