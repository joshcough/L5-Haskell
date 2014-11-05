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
interpL3 (L3 e fs) = runST $ show <$> runComputation (interpE e) (fmap (\f -> (name f, f)) fs)

-- | interpret an E, building a monadic operation to be run.
interpE :: MonadHOComputer c m L3Func => E -> m Runtime
interpE (Let v d e)           = interpD d >>= \d' -> locally (Map.insert v d') (interpE e)
interpE (IfStatement v te fe) = interpV v >>= \v' -> interpE $ if v' /= lFalse then te else fe
interpE (DE d)                = interpD d

interpD :: MonadHOComputer c m L3Func => D -> m Runtime
-- Regular L3 Level stuff
interpD (FunCall v vs)    = interpApp v vs
interpD (NewTuple vs)     = traverse interpV vs >>= newArray
interpD (MakeClosure l v) = interpD (NewTuple [LabelV l, v])
interpD (ClosureProc c)   = do c' <- interpV c; arrayRef c' (Num 0)
interpD (ClosureVars c)   = do c' <- interpV c; arrayRef c' (Num 1)
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
interpD (PrimApp ARef [a, loc])    = bind2 arrayRef (interpV a) (interpV loc)
interpD (PrimApp ASet [a, loc, v]) = bind3 arraySet (interpV a) (interpV loc) (interpV v)
interpD (PrimApp ALen [a])         = interpV a >>= arraySize "L3-alen"
-- print should apparently return 0. this might require more work in the future.
interpD (PrimApp L3.Print [v])     = interpV v >>= print False >> return (Num 0)
interpD (PrimApp p vs)             = exception $ show p ++ " applied to wrong args: " ++ show vs

interpV :: MonadHOComputer c m f=> V -> m Runtime
interpV (VarV v)   = use env >>= envLookup v
interpV (NumV i)   = return $ Num i
interpV (LabelV l) = return $ FunctionPointer l

-- | function application (f v...)
interpApp :: MonadHOComputer c m L3Func => V -> [V] -> m Runtime
interpApp f vs = do
  (FunctionPointer label) <- interpV f
  rs                      <- traverse interpV vs
  (Func _ args body)      <- use lib >>= libLookup label
  env                     <- use env
  locally (\_ -> Map.union (Map.fromList $ zip args rs) env) (interpE body)
