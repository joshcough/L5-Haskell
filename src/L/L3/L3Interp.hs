{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}

module L.L3.L3Interp (
  interpL3
 ,envLookup, forceLookup, forceLookup
) where

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
import L.Interpreter.HOComputer
import L.Interpreter.Memory
import L.Interpreter.Output
import L.Interpreter.Runtime
import L.L1L2AST hiding (Func)
import L.L3.L3AST as L3 hiding (print)
import L.Utils
import Prelude hiding (print)

interpL3 :: L3 -> String
interpL3 p = runST $ show <$> runL3Computation p

newHOComputer :: MonadST m => L3 -> m (HOComputer (World m) L3.Func)
newHOComputer (L3 _ fs) = do
  mem <-  newMem (MemoryConfig False False) -- numbers not encoded, and not word indexed.
  return HOComputer {
    _env = Map.empty,
    _mem = mem,
    _lib = Map.fromList $ fmap (\f -> (name f, f)) fs
  }

runL3Computation :: (MonadST m, Functor m) => L3 -> m (ComputationResult (FrozenHOComputer L3.Func))
runL3Computation p@(L3 e _) = do
  c <- newHOComputer p
  (output, (eHaltRuntime, finalHOComputer)) <- runOutputT $ flip runStateT c $ runErrorT $ interpE e
  mem <- freezeMem $ finalHOComputer^.mem
  let fc = FrozenHOComputer (finalHOComputer^.env) mem (finalHOComputer^.lib)
  return $ mkComputationResult (output, (eHaltRuntime, fc))

-- | interpret an E, building a monadic operation to be run.
interpE :: MonadHOComputer c m L3.Func => E -> m Runtime
interpE (Let v d e)           = interpD d >>= \d' -> locally (Map.insert v d') (interpE e)
interpE (IfStatement v te fe) = interpV v >>= \v' -> interpE $ if v' /= lFalse then te else fe
interpE (DE d)                = interpD d

interpD :: MonadHOComputer c m L3.Func => D -> m Runtime
-- Regular L3 Level stuff
interpD (FunCall v vs)    = interpApp v vs
interpD (NewTuple vs)     = traverse interpV vs >>= newArray
interpD (MakeClosure l v) = interpD (NewTuple [LabelV l, v])
interpD (ClosureProc c)   = arrayRef c (NumV 0)
interpD (ClosureVars c)   = arrayRef c (NumV 1)
interpD (VD v)            = interpV v
-- Primitive applications (built in functions)
interpD (PrimApp b [l, r]) | isBiop b = bind2 (mathOp b) (interpV l) (interpV r)
interpD (PrimApp IsNumber [NumV _  ]) = return lTrue
interpD (PrimApp IsNumber [v@(VarV _)]) =
  do rv <- interpV v; return $ case rv of (Num _) -> lTrue; _ -> lFalse
-- labels act like arrays, it's weird, but true.
interpD (PrimApp IsNumber _         )   = return lFalse
interpD (PrimApp IsArray  [v@(VarV _)]) =
  do rv <- interpV v; return $ case rv of (Pointer _) -> lTrue; _ -> lFalse
interpD (PrimApp IsArray _)        = return lFalse
interpD (PrimApp NewArray [s, d])  = bind2 allocate (interpV s) (interpV d)
interpD (PrimApp ARef [a, loc])    = arrayRef a loc
interpD (PrimApp ASet [a, loc, v]) = arraySet a loc v
interpD (PrimApp ALen [a])         = interpV a >>= arraySize "L3-alen"
-- print should apparently return 0. this might require more work in the future.
interpD (PrimApp L3.Print [v])     = interpV v >>= print False >> return (Num 0)
interpD (PrimApp p vs)             = exception $ show p ++ " applied to wrong args: " ++ show vs

interpV :: MonadHOComputer c m f=> V -> m Runtime
interpV (VarV v)   = use env >>= envLookup v
interpV (NumV i)   = return $ Num i
interpV (LabelV l) = return $ FunctionPointer l

-- | function application (f v...)
interpApp :: MonadHOComputer c m L3.Func => V -> [V] -> m Runtime
interpApp f vs = do
  (FunctionPointer label) <- interpV f
  rs                      <- traverse interpV vs
  (Func _ args body)      <- use lib >>= libLookup label
  env                     <- use env
  locally (\_ -> Map.union (Map.fromList $ zip args rs) env) (interpE body)

-- | array reference (arr[i])
arrayRef :: MonadHOComputer c m f=>  V -> V -> m Runtime
arrayRef arr i = do
  p     <- interpV arr
  index <- interpV i >>= mathOp Add (Num 1)
  safeReadMem "L3-aref" p index

-- | sets the (arr[i] = e)
arraySet :: MonadHOComputer c m f=> V -> V -> V -> m Runtime
arraySet arr i v = do
  p        <- interpV arr
  index    <- interpV i >>= mathOp Add (Num 1)
  r        <- interpV v
  safeWriteMem "L3 array set" p index r
  return lTrue
