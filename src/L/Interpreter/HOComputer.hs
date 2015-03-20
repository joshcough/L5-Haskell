{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}

-- Higher Order Computer
module L.Interpreter.HOComputer where

import Control.Applicative
import Control.Arrow (second)
import Control.Lens
import Control.Monad.State
import Control.Monad.ST.Class
import Control.Monad.Trans.Error
import Data.Int
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
import L.L1L2AST hiding (Func)
import L.Variable
import Prelude hiding (print)

type Env r = Map Variable r
type Lib func = Map Label func

data HOComputer s r = HOComputer { _env :: Env r, _mem :: Memory s }
makeClassy ''HOComputer

data FrozenHOComputer r = FrozenHOComputer { _envFrozen :: Env r, _memFrozen :: Vector Runtime }
makeClassy ''FrozenHOComputer

instance HasMemory (HOComputer s r) s where memory = mem
type MonadHOComputer c m r = (Applicative m, HasHOComputer c (World m) r, MonadMemory c m, MonadOutput m)

-- numbers not encoded, and not word indexed.
hoMemConfig :: MemoryConfig
hoMemConfig = MemoryConfig False False

newHOComputer :: (Functor m, MonadST m) => m (HOComputer (World m) r)
newHOComputer = HOComputer (Map.empty) <$> newMem hoMemConfig

runComputation :: (MonadST m, Functor m) =>
  ErrorT Halt (StateT (HOComputer (World m) r) (OutputT m)) a ->
  m (ComputationResult (FrozenHOComputer r))
runComputation compuation = do
  c <- newHOComputer
  (output, (eHaltRuntime, finalComputer)) <- runOutputT $ flip runStateT c $ runErrorT $ compuation
  mem <- freezeMem $ finalComputer^.mem
  return $ mkComputationResult (output, (eHaltRuntime, FrozenHOComputer (finalComputer^.env) mem))

-- | like local on reader, only in my state monad.
-- | notice that putEnv is hidden here, so that it can't be used unsafely.
locally :: MonadHOComputer c m r => (Env r -> Env r) -> m a -> m a
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
instance Show r => Show (ComputationResult (FrozenHOComputer r)) where
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

showMem :: Vector Runtime -> String
showMem m = show $ map (second show) memList where
  memList :: [(Int, Runtime)]
  memList = filter (\(_,r) -> Num 0 /= r) . zip [0..] $ Vector.toList m

showEnv :: Show r => Map Variable r -> String
showEnv = show . Map.map show

envLookup :: MonadHOComputer c m r => Variable -> Env r -> m r
envLookup = forceLookup "variable"

libLookup :: MonadHOComputer c m r => Label -> Lib f -> m f
libLookup = forceLookup "function"

forceLookup :: (Show k, Ord k, MonadHOComputer c m r) => String -> k -> Map k v -> m v
forceLookup kName k m =
  maybe (exception $ concat ["unbound ", kName, ": ", show k]) return (Map.lookup k m)
