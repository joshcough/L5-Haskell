{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}

module L.L5.L5Interp (interpL5, interpL5String) where

import Bound
import Bound.Scope
import Control.Applicative
import Control.Lens
import Control.Monad.State
import Control.Monad.ST
import Data.List
import L.Interpreter.HOComputer
import L.Interpreter.Memory
import L.Interpreter.Output
import L.Interpreter.Runtime
import L.L1L2AST hiding (Func, Print)
import L.L3.L3AST (PrimName(..), isBiop)
import L.L5.L5AST
import L.Read
import L.Utils
import Prelude hiding (print)

interpL5String :: String -> String
interpL5String = interpL5 . either error id . fromSExpr . sread

interpL5 :: L5 -> String
interpL5 e = runST $ show <$> runComputation (interpEMain' e)

data L5Runtime a = OriginalRuntime Runtime | Closure (Scope Int E a) 
  deriving Eq

instance a ~ Variable => Show (L5Runtime a) where
  show (OriginalRuntime r) = show r
  show (Closure _) = "<function>"

interpEMain' :: MonadHOComputer c m (L5Runtime Variable) => L5 -> m Runtime
interpEMain' = interpEMain

interpEMain :: (Eq a, MonadHOComputer c m (L5Runtime a)) => E a -> m Runtime
interpEMain e = do
  r <- interpE e
  return $ case r of OriginalRuntime r -> r; Closure _ -> Num 0

-- | interpret an E, building a monadic operation to be run.
interpE :: (Eq a, MonadHOComputer c m (L5Runtime a)) => E a -> m (L5Runtime a)
interpE (Lambda _ e)        = return $ Closure e
interpE (Var _)             = fail $ "unbound var"
interpE (Let _ e body)      = interpE $ App (Lambda [] $ mapBound (const 0) body) [e]
interpE (LetRec _ e body)   = interpE (inst body) where  
  e'   = inst e
  inst = instantiate (const e')
interpE (If p t f)          = do r <- interpE p; interpE $ if r == l5True then t else f
interpE (NewTuple es)       = promote <$> (traverse expectRuntime es >>= newArray)
interpE (Begin e1 e2)       = interpE e1 >> interpE e2
interpE (LitInt i)          = return . promote $ Num i
interpE (App (PrimE p) es)  = interpPrim p es
interpE (App f es)          = interpApp f es
interpE (PrimE p)           = interpE (primlet p) where 

-- | interpret a Primitive function
interpPrim :: (Eq a, MonadHOComputer c m (L5Runtime a)) => PrimName -> [E a] -> m (L5Runtime a)
interpPrim Print    [e]          = expectRuntime e >>= print False >> return (promote $ Num 0)
interpPrim IsNumber [e]          = l5IsNumber <$> interpE e
interpPrim IsArray  [e]          = l5IsArray  <$> interpE e
interpPrim NewArray [e1, e2]     = promote    <$> bind2 allocate (expectRuntime e1) (expectRuntime e2)
interpPrim ARef     [e1, e2]     = promote    <$> bind2 arrayRef (expectRuntime e1) (expectRuntime e2)
interpPrim ASet     [e1, e2, e3] = promote    <$> bind3 arraySet (evalPointer e1) (evalNumber e2) (expectRuntime e3)
interpPrim ALen     [e]          = promote    <$> (expectRuntime e >>= arraySize "L5-alen")
interpPrim p [e1, e2] | isBiop p = promote    <$> bind2 (mathOp p) (expectRuntime e1) (expectRuntime e2)
interpPrim _ _                   = error "todo" -- exception $ "wrong arguments to prim: " ++ show p ++ mkString "," (show <$> args)

l5True  :: L5Runtime a
l5True  = promote lTrue
l5False :: L5Runtime a
l5False = promote lFalse

promote :: Runtime -> L5Runtime a
promote = OriginalRuntime

l5IsNumber :: L5Runtime a -> (L5Runtime a)
l5IsNumber (OriginalRuntime r) = promote $ isNumber r
l5IsNumber _                   = l5False

l5IsArray :: L5Runtime a -> (L5Runtime a)
l5IsArray (OriginalRuntime r) = promote $ isArray r
l5IsArray _                   = l5False

-- | function application (f e...)
--instantiate :: Monad f => (b -> f a) -> Scope b f a -> f a
interpApp :: (Eq a, MonadHOComputer c m (L5Runtime a)) => E a -> [E a] -> m (L5Runtime a)
interpApp f es = do
  (Closure body) <- evalClosure f
  interpE (instantiate (es!!) body) 

expectRuntime :: (Eq a, MonadHOComputer c m (L5Runtime a)) => E a -> m Runtime
expectRuntime = expectRuntimeM return

evalNumber :: (Eq a, MonadHOComputer c m (L5Runtime a)) => E a -> m Runtime
evalNumber = expectRuntimeM (\r -> Num <$> expectNum r)

evalPointer :: (Eq a, MonadHOComputer c m (L5Runtime a)) => E a -> m Runtime
evalPointer = expectRuntimeM (\r -> Pointer <$> expectPointer "L5/evalPointer" r)

expectRuntimeM :: (Eq a, MonadHOComputer c m (L5Runtime a)) => 
  (Runtime -> m Runtime) -> E a -> m Runtime
expectRuntimeM f e = interpE e >>= expectRuntime' f

expectRuntime' :: (Eq a, MonadHOComputer c m (L5Runtime a)) => 
  (Runtime -> m Runtime) -> (L5Runtime a) -> m Runtime
expectRuntime' f (OriginalRuntime r) = f r
expectRuntime' _ _ = exception $ "expected Runtime, but got a Closure"

evalClosure :: (Eq a, MonadHOComputer c m (L5Runtime a)) => E a -> m (L5Runtime a)
evalClosure e = interpE e >>= evalClosure' where
  evalClosure' (OriginalRuntime r) = exception $ "expected Closure, but got " ++ show r
  evalClosure' c = return c