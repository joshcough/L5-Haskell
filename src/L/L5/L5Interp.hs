{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}

module L.L5.L5Interp (interpL5) where

import Control.Applicative
import Control.Lens
import Control.Monad.State
import Control.Monad.ST
import Data.List
import qualified Data.Map as Map
import L.Interpreter.HOComputer
import L.Interpreter.Memory
import L.Interpreter.Output
import L.Interpreter.Runtime
import L.L1L2AST hiding (Func, Print)
import L.L3.L3AST (PrimName(..), isBiop)
import L.L5.L5AST
import L.Utils
import Prelude hiding (print)

interpL5 :: L5 -> String
interpL5 e = runST $ show <$> runComputation (interpEMain e)

data L5Runtime = OriginalRuntime Runtime | Closure [Variable] L5 (Env L5Runtime) deriving Eq
instance Show L5Runtime where
  show (OriginalRuntime r) = show r
  show _ = "<function>"

interpEMain :: MonadHOComputer c m L5Runtime => L5 -> m Runtime
interpEMain e = do
  r <- interpE e
  return $ case r of OriginalRuntime r -> r; Closure _ _ _ -> Num 0

-- | interpret an E, building a monadic operation to be run.
interpE :: MonadHOComputer c m L5Runtime => L5 -> m L5Runtime
interpE (Lambda vs e)       = use env >>= return . Closure vs e
interpE (Var v)             = use env >>= envLookup v
interpE (Let v e body)      = interpE $ App (Lambda [v] body) [e]
interpE (LetRec v e body)   = locally newEnv (interpE body) where
  -- todo: this is all weird now that letrec doesnt have a list of functions...
  newEnv old = new where new = Map.union (Map.fromList ([toClosure (v, e)])) old
                         toClosure (v, Lambda vs e) = (v, Closure vs e new)
                         toClosure (v, e) = error $ "non-lambda in letrec: " ++ show (v, e)
interpE (If p t f)          = do r <- interpE p; interpE $ if r == l5True then t else f
interpE (NewTuple es)       = promote <$> (traverse expectRuntime es >>= newArray)
interpE (Begin e1 e2)       = interpE e1 >> interpE e2
interpE (LitInt i)          = return . promote $ Num i
interpE (App (PrimE p) es)  = interpPrim p es
interpE (App f es)          = interpApp f es
interpE (PrimE p)           = interpE $ Lambda vs (App (PrimE p) $ Var <$> vs) where vs = primVars p

-- | interpret a Primitive function
interpPrim :: MonadHOComputer c m L5Runtime => PrimName -> [L5] -> m L5Runtime
interpPrim Print    [e]          = expectRuntime e >>= print False >> return (promote $ Num 0)
interpPrim IsNumber [e]          = l5IsNumber <$> interpE e
interpPrim IsArray  [e]          = l5IsArray  <$> interpE e
interpPrim NewArray [e1, e2]     = promote    <$> bind2 allocate (expectRuntime e1) (expectRuntime e2)
interpPrim ARef     [e1, e2]     = promote    <$> bind2 arrayRef (expectRuntime e1) (expectRuntime e2)
interpPrim ASet     [e1, e2, e3] = promote    <$> bind3 arraySet (evalPointer e1) (evalNumber e2) (expectRuntime e3)
interpPrim ALen     [e]          = promote    <$> (expectRuntime e >>= arraySize "L5-alen")
interpPrim p [e1, e2] | isBiop p = promote    <$> bind2 (mathOp p) (expectRuntime e1) (expectRuntime e2)
interpPrim p args                = exception $ "wrong arguments to prim: " ++ show p ++ mkString "," (show <$> args)

l5True :: L5Runtime
l5True  = promote lTrue
l5False :: L5Runtime
l5False = promote lFalse

promote :: Runtime -> L5Runtime
promote = OriginalRuntime

l5IsNumber :: L5Runtime -> L5Runtime
l5IsNumber (OriginalRuntime r) = promote $ isNumber r
l5IsNumber _                   = l5False

l5IsArray :: L5Runtime -> L5Runtime
l5IsArray (OriginalRuntime r) = promote $ isArray r
l5IsArray _                   = l5False

-- | function application (f e...)
interpApp :: MonadHOComputer c m L5Runtime => L5 -> [L5] -> m L5Runtime
interpApp f es = do
  (Closure vars body env) <- evalClosure f
  rs <- traverse interpE es
  locally (\_ -> Map.union (Map.fromList (zip vars rs)) env) (interpE body)

expectRuntime :: MonadHOComputer c m L5Runtime => L5 -> m Runtime
expectRuntime = expectRuntimeM return

evalNumber :: MonadHOComputer c m L5Runtime => L5 -> m Runtime
evalNumber = expectRuntimeM (\r -> Num <$> expectNum r)

evalPointer :: MonadHOComputer c m L5Runtime => L5 -> m Runtime
evalPointer = expectRuntimeM (\r -> Pointer <$> expectPointer "L5/evalPointer" r)

expectRuntimeM :: MonadHOComputer c m L5Runtime => (Runtime -> m Runtime) -> L5 -> m Runtime
expectRuntimeM f e = interpE e >>= expectRuntime' f

expectRuntime' :: MonadHOComputer c m L5Runtime => (Runtime -> m Runtime) -> L5Runtime -> m Runtime
expectRuntime' f (OriginalRuntime r) = f r
expectRuntime' _ _ = exception $ "expected Runtime, but got a Closure"

evalClosure :: MonadHOComputer c m L5Runtime => L5 -> m L5Runtime
evalClosure e = interpE e >>= evalClosure' where
  evalClosure' (OriginalRuntime r) = exception $ "expected Closure, but got " ++ show r
  evalClosure' c = return c