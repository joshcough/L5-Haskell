{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}

module L.L4.L4Interp (interpL4) where

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
import L.L1L2AST hiding (Func, Print)
import L.L3.L3AST (PrimName(..), V(..), isBiop, Func(..))
import L.L4.L4AST as L4
import L.Utils
import Prelude hiding (print)

interpL4 :: L4 -> String
interpL4 (L4 e fs) = runST $ show <$> runComputation (interpE e) (fmap (\f -> (name f, f)) fs)

-- | interpret an E, building a monadic operation to be run.
interpE :: MonadHOComputer c m L4Func => E -> m Runtime
interpE (Let v e b)           = do e' <- interpE e; locally (Map.insert v e') (interpE b)
interpE (IfStatement e te fe) = do e' <- interpE e; interpE $ if e' /= lFalse then te else fe
interpE (FunCall e es)        = interpApp e es
interpE (NewTuple es)         = traverse interpE es >>= newArray
interpE (MakeClosure l e)     = interpE (NewTuple [VE $ LabelV l, e])
interpE (ClosureProc c)       = do c' <- interpE c; arrayRef c' (Num 0)
interpE (ClosureVars c)       = do c' <- interpE c; arrayRef c' (Num 1)
interpE (Begin e1 e2)         = interpE e1 >> interpE e2
interpE (VE v)                = interpV v

-- Primitive applications (built in functions)
interpE (PrimApp b [l, r]) | isBiop b = bind2 (mathOp b) (interpE l) (interpE r)
interpE (PrimApp IsNumber [e])        = isNumber <$> interpE e
interpE (PrimApp IsNumber _       )   = return lFalse
interpE (PrimApp IsArray  [e])        =
  do r <- interpE e; return $ case r of (Pointer _) -> lTrue; _ -> lFalse
interpE (PrimApp IsArray _)           = return lFalse
interpE (PrimApp NewArray [s, d])     = bind2 allocate (interpE s) (interpE d)
interpE (PrimApp ARef [a, loc])       = bind2 arrayRef (interpE a) (interpE loc)
interpE (PrimApp ASet [a, loc, v])    = bind3 arraySet (interpE a) (interpE loc) (interpE v)
interpE (PrimApp ALen [a])            = interpE a >>= arraySize "L4-alen"
-- print should apparently return 0. this might require more work in the future.
interpE (PrimApp Print [e])           = interpE e >>= print False >> return (Num 0)
interpE (PrimApp p es)                = exception $ show p ++ " applied to wrong args: " ++ show es

interpV :: MonadHOComputer c m f => V -> m Runtime
interpV (VarV v)   = use env >>= envLookup v
interpV (NumV i)   = return $ Num i
interpV (LabelV l) = return $ FunctionPointer l

-- | function application (f v...)
interpApp :: MonadHOComputer c m L4.Func => E -> [E] -> m Runtime
interpApp f es = do
  (FunctionPointer label) <- interpE f
  rs                      <- traverse interpE es
  (Func _ args body)      <- use lib >>= libLookup label
  env                     <- use env
  locally (\_ -> Map.union (Map.fromList $ zip args rs) env) (interpE body)
