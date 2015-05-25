{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecursiveDo #-}

module L.L5.L5Interp (interpL5) where

import Control.Applicative
import Control.Lens
import Control.Monad.State
import Control.Monad.ST
import Data.List
import qualified Data.Map as Map
import L.Primitives hiding (print)
import L.Interpreter.HOComputer
import L.Interpreter.Memory
import L.Interpreter.Output
import L.Interpreter.Runtime
import L.L5.L5AST
import L.Variable
import L.Util.Utils
import Prelude hiding (print)

type L5Monad c m = MonadHOComputer c m Closure Closure
{-
Expanded:
type L5Monad c m = (
  HasHOComputer c (World m) Closure Closure,
  HasMemory     c (World m) Closure,
  MonadST         m,
  MonadOutput     m,
  MonadError Halt m,
  MonadState    c m,
  Applicative     m,
  MonadFix        m)
-}

interpL5 :: L5 -> String
interpL5 e = runST $ show <$> runComputation (interpE e) where

data Closure = Closure [Variable] L5 (Env Closure) deriving Eq
instance Show Closure where show _ = "<function>"

-- | interpret an E, building a monadic operation to be run.
interpE :: L5Monad c m => E -> m (Runtime Closure)
interpE = ie where
  ie (Lambda vs e)      = use env >>= return . Runtime . Closure vs e
  ie (Var v)            = use env >>= envLookup v
  ie (Let v e body)     = ie $ App (Lambda [v] body) [e]
  ie (LetRec v (Lambda v' e) body) = locally newEnv (ie body) where
    newEnv :: Env Closure -> Env Closure
    newEnv old = new where new = Map.insert v (Runtime $ Closure v' e new) old
  ie (LetRec v e body) = do
    rec newEnv <- use env >>= return . Map.insert v e'
        e'     <- locally (const newEnv) (ie e)
    locally (const newEnv) (ie body)
  ie (If p t f)         = do r <- ie p; ie $ if r /= lFalse then t else f
  ie (NewTuple es)      = traverse ie es >>= newArray
  ie (Begin e1 e2)      = ie e1 >> ie e2
  ie (LitInt i)         = return $ Num i
  ie (PrimE p)          = ie $ Lambda vs (App (PrimE p) $ Var <$> vs) where vs = primVars p
  ie (App (PrimE p) es) = interpPrim p es
  ie (App f es)         = do
    Runtime (Closure vars body env) <- evalClosure f
    rs <- traverse interpE es
    locally (\_ -> Map.union (Map.fromList $ zip vars rs) env) (interpE body)

interpPrim :: L5Monad c m => PrimName -> [E] -> m (Runtime Closure)
interpPrim = ip where
  ip Print    [e]         = asRun "print" e >>= print False >> return (Num 0)
  ip IsNumber [e]         = isNumber <$> interpE e
  ip IsArray  [e]         = isArray  <$> interpE e
  ip NewArray [e1,e2]     = bind2 allocate (asRun "newArray/e1" e1) (asRun "newArray/e2" e2)
  ip ARef     [e1,e2]     = bind2 arrayRef (asRun "aRef" e1) (asRun "aRef" e2)
  ip ASet     [e1,e2,e3]  = bind3 arraySet (asPtr "aSet/e1" e1) (asNum "aSet/e2" e2) (asRun "aSet/e3" e3)
  ip ALen     [e]         = asRun "aLen" e >>= arraySize "L5-alen"
  ip p [e1,e2] | isBiop p = bind2 (mathOp p) (asRun "primop" e1) (asRun "primop" e2)
  ip p args               = exception $ "wrong arguments to prim: " ++ show p ++ " " ++ mkString "," (show <$> args)

asRun :: L5Monad c m  => String -> E -> m (Runtime Closure)
asRun = asRunM return
asNum :: L5Monad c m  => String -> E -> m (Runtime Closure)
asNum = asRunM $ \r -> Num <$> expectNum r
asPtr :: L5Monad c m  => String -> E -> m (Runtime Closure)
asPtr = asRunM $ \r -> Pointer <$> expectPointer "L5/asPtr" r
asRunM :: L5Monad c m  => (Runtime Closure -> m (Runtime Closure)) -> String -> E -> m (Runtime Closure)
asRunM f caller e = interpE e >>= f >>= g where
  g (Runtime _) = exception $ caller ++ " expected Runtime, but got a Closure."
  g r = return r

evalClosure :: L5Monad c m => L5 -> m (Runtime Closure)
evalClosure e = interpE e >>= evalClosure' where
  evalClosure' r@(Runtime _) = return r
  evalClosure' r = exception $ "expected Closure, but got " ++ show r

