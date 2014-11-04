{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}

module L.L3.L3Interp (interpL3) where

import Control.Applicative
import Control.Arrow (second)
import Control.Lens
import Control.Monad.State
import Control.Monad.ST
import Control.Monad.ST.Class
import Control.Monad.Trans.Error
import Data.Array.IO (IOArray)
import qualified Data.Array.IO as IOArray
import Data.Int
import Data.List
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe
import Data.Traversable
import Debug.Trace
import Data.Vector (Vector)
import qualified Data.Vector as Vector
import L.Interpreter.Computer
import L.Interpreter.Memory
import L.Interpreter.Output
import L.Interpreter.Runtime
import L.L1L2AST hiding (Func)
import L.L3.L3AST as L3 hiding (print)
import L.Utils
import Prelude hiding (print)
import System.IO.Unsafe

type Env = Map Variable Runtime
type Lib = Map Label Func

data L3Computer s = L3Computer {
  _l3Env    :: Env
 ,_l3Mem    :: Memory s
 ,_l3Lib    :: Lib
}
makeClassy ''L3Computer

data L3FrozenComputer = L3FrozenComputer {
  _l3EnvFrozen    :: Env
 ,_l3MemFrozen    :: Vector Runtime
 ,_l3LibFrozen    :: Lib
}
makeClassy ''L3FrozenComputer

instance HasMemory (L3Computer s) s where memory = l3Mem
type MonadL3Computer c m = (Applicative m, HasL3Computer c (World m), MonadMemory c m, MonadOutput m)

-- | like local on reader, only in my state monad.
-- | notice that putEnv is hidden here, so that it can't be used unsafely.
locally :: MonadL3Computer c m => (Env -> Env) -> m a -> m a
locally modifyEnv action = do 
  e   <- use l3Env
  l3Env .= modifyEnv e
  res <- action
  l3Env .= e
  return res

lTrue, lFalse :: Runtime
lTrue  = Num 1
lFalse = Num 0

interpL3 :: L3 -> String
interpL3 p = runST $ show <$> runL3Computation p

newL3Computer :: MonadST m => L3 -> m (L3Computer (World m))
newL3Computer (L3 _ fs) = do
  mem <-  newMem (MemoryConfig False False) -- numbers not encoded, and not word indexed.
  return L3Computer {
    _l3Env = Map.empty,
    _l3Mem = mem,
    _l3Lib = Map.fromList $ fmap (\f -> (name f, f)) fs
  }

runL3Computation :: (MonadST m, Functor m) => L3 -> m (ComputationResult L3FrozenComputer)
runL3Computation p@(L3 e _) = do
  c <- newL3Computer p
  (output, (eHaltRuntime, finalComputer)) <- runOutputT $ flip runStateT c $ runErrorT $ interpE e
  mem <- freezeMem $ finalComputer^.l3Mem
  let fc = L3FrozenComputer (finalComputer^.l3Env) mem (finalComputer^.l3Lib)
  return $ mkComputationResult (output, (eHaltRuntime, fc))

-- TODO: we have the ability to distinguish between stdout and stderr
-- TODO: this doesnt show the heap pointer!
--       we shold be able to use that
--       but forcing us into a string here makes this difficult.
instance Show (ComputationResult L3FrozenComputer) where
  show (ComputationResult output Running _) = concat $ fmap outputText output
  show (ComputationResult output rs c) =intercalate "\n" [
    "Output:      " ++ concat (fmap outputText output),
    "Run State:   " ++ show rs,
    "Memory!=0:   " ++ showMem (c^.l3MemFrozen),
    "Environment: " ++ showEnv (c^.l3EnvFrozen) ] where
    --"Heap Ptr:   " ++ show (frozenHeapP c) ] where

showMem :: Vector Runtime -> String
showMem m = show $ map (second showRuntime) memList where
  memList :: [(Int, Runtime)]
  memList = filter (\(_,r) -> Num 0 /= r) . zip [0..] $ Vector.toList m

showEnv :: Map Variable Runtime -> String
showEnv = show . Map.map showRuntime

-- | interpret an E, building a monadic operation to be run.
interpE :: MonadL3Computer c m => E -> m Runtime
interpE (Let v d e)           = interpD d >>= \d' -> locally (Map.insert v d') (interpE e)
interpE (IfStatement v te fe) = interpV v >>= \v' -> interpE $ if v' /= lFalse then te else fe
interpE (DE d)                = interpD d

interpD :: MonadL3Computer c m => D -> m Runtime
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
interpD (PrimApp L3.Print [v])     = interpV v >>= print False
interpD (PrimApp p vs)             = exception $ show p ++ " applied to wrong arguments: " ++ show vs

interpV :: MonadL3Computer c m => V -> m Runtime
interpV (VarV v)   = use l3Env >>= envLookup v
interpV (NumV i)   = return $ Num i
interpV (LabelV l) = return $ FunctionPointer l

-- | function application (f v...)
interpApp :: MonadL3Computer c m => V -> [V] -> m Runtime
interpApp f vs = do
  (FunctionPointer label) <- interpV f
  rs                      <- traverse interpV vs
  (Func _ args body)      <- use l3Lib >>= libLookup label
  env                     <- use l3Env
  locally (\_ -> Map.union (Map.fromList $ zip args rs) env) (interpE body)

-- | array reference (arr[i])
arrayRef :: MonadL3Computer c m =>  V -> V -> m Runtime
arrayRef arr i = do
  p     <- interpV arr
  index <- interpV i >>= mathOp Add (Num 1)
  safeReadMem "L3-aref" p index

-- | sets the (arr[i] = e)
arraySet :: MonadL3Computer c m => V -> V -> V -> m Runtime
arraySet arr i v = do
  p        <- interpV arr
  index    <- interpV i >>= mathOp Add (Num 1)
  r        <- interpV v
  safeWriteMem "L3 array set" p index r
  return lTrue

envLookup :: MonadL3Computer c m => Variable -> Env -> m Runtime
envLookup = forceLookup "variable"

libLookup :: MonadL3Computer c m => Variable -> Lib -> m Func
libLookup = forceLookup "function"

forceLookup :: (Show k, Ord k, MonadL3Computer c m) => String -> k -> Map k v -> m v
forceLookup kName k m =
  maybe (exception $ concat ["unbound ", kName, ": ", show k]) return (Map.lookup k m)

mathOp :: MonadL3Computer c m => PrimName -> Runtime -> Runtime -> m Runtime
mathOp Add      (Num l) (Num r) = return . Num $ l + r
mathOp Sub      (Num l) (Num r) = return . Num $ l - r
mathOp Mult     (Num l) (Num r) = return . Num $ l * r
mathOp LessThan (Num l) (Num r) = return . boolToNum $ l <  r
mathOp LTorEQ   (Num l) (Num r) = return . boolToNum $ l <= r
mathOp EqualTo  (Num l) (Num r) = return . boolToNum $ l == r
mathOp b l r = exception $
  concat ["invalid arguments to ", show b, " :", showRuntime l, ", " , showRuntime r]

boolToNum :: Bool -> Runtime
boolToNum True  = Num 1
boolToNum False = Num 0
