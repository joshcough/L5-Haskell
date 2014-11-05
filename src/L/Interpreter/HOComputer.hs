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
import Control.Monad.ST
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
import L.L3.L3AST as L3 hiding (print)
import L.Utils
import Prelude hiding (print)

type Env = Map Variable Runtime
type Lib func = Map Label func

data HOComputer s f = HOComputer {
  _env    :: Env
 ,_mem    :: Memory s
 ,_lib    :: Lib f
}
makeClassy ''HOComputer

data FrozenHOComputer f = FrozenHOComputer {
  _envFrozen    :: Env
 ,_memFrozen    :: Vector Runtime
 ,_libFrozen    :: Lib f
}
makeClassy ''FrozenHOComputer

instance HasMemory (HOComputer s f) s where memory = mem
type MonadHOComputer c m f = (Applicative m, HasHOComputer c (World m) f, MonadMemory c m, MonadOutput m)

-- | like local on reader, only in my state monad.
-- | notice that putEnv is hidden here, so that it can't be used unsafely.
locally :: MonadHOComputer c m f=> (Env -> Env) -> m a -> m a
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
instance Show (ComputationResult (FrozenHOComputer f)) where
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
showMem m = show $ map (second showRuntime) memList where
  memList :: [(Int, Runtime)]
  memList = filter (\(_,r) -> Num 0 /= r) . zip [0..] $ Vector.toList m

showEnv :: Map Variable Runtime -> String
showEnv = show . Map.map showRuntime

envLookup :: MonadHOComputer c m f=> Variable -> Env -> m Runtime
envLookup = forceLookup "variable"

libLookup :: MonadHOComputer c m f => Variable -> Lib f -> m f
libLookup = forceLookup "function"

forceLookup :: (Show k, Ord k, MonadHOComputer c m f) => String -> k -> Map k v -> m v
forceLookup kName k m =
  maybe (exception $ concat ["unbound ", kName, ": ", show k]) return (Map.lookup k m)
