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
import L.Interpreter.Runtime hiding (foldRuntime)
import L.L5.L5AST
import L.Parser.SExpr
import L.Variable
import L.Util.Utils
import Prelude hiding (print)

interpL5String :: String -> String
interpL5String = interpL5 . either error id . fromSExpr . sread

interpL5 :: L5 -> String
interpL5 e = runST $ show <$> runComputation (interpEMain' e) where
  interpEMain' :: MonadHOComputer c m L5Runtime => L5 -> m Runtime
  interpEMain' = interpEMain

data L5Runtime = RT Runtime | Closure [Variable] L5 (Env L5Runtime) deriving Eq

foldRT :: (Runtime -> b) -> b -> L5Runtime -> b
foldRT f b r = case r of { RT r -> f r; Closure _ _ _ -> b }

instance Show L5Runtime where
  show = foldRT show "<function>"

interpEMain :: MonadHOComputer c m L5Runtime => E -> m Runtime
interpEMain e = interpE e >>= return . foldRT id (Num 0)

-- | interpret an E, building a monadic operation to be run.
interpE :: MonadHOComputer c m L5Runtime => E -> m L5Runtime
interpE = ie where
  ie (Lambda vs e)      = use env >>= return . Closure vs e
  ie (Var v)            = use env >>= envLookup v
  ie (Let v e body)     = ie $ App (Lambda [v] body) [e]
  ie (LetRec v (Lambda v' e) body) = locally newEnv (ie body) where
    newEnv old = new where new = Map.insert v (Closure v' e new) old
  ie (LetRec v e body) = do
    rec newEnv <- use env >>= return . Map.insert v e'
        e'     <- locally (const newEnv) (ie e)
    locally (const newEnv) (ie body)
  ie (If p t f)         = do r <- ie p; ie $ if r /= RT lFalse then t else f

  -- new-tuple is fatally flawed, it can't store closures, but it obviously should be able to
  --ie (NewTuple es)      = RT <$> (traverse ie es >>= newArray)
  ie (NewTuple es)      = RT <$> (traverse (asRun "es") es >>= newArray)

  ie (Begin e1 e2)      = ie e1 >> ie e2
  ie (LitInt i)         = return . RT $ Num i
  ie (PrimE p)          = ie $ Lambda vs (App (PrimE p) $ Var <$> vs) where vs = primVars p
  ie (App (PrimE p) es) = interpPrim p es
  ie (App f es)         = do
    (Closure vars body env) <- evalClosure f
    rs <- traverse interpE es
    locally (\_ -> Map.union (Map.fromList (zip vars rs)) env) (interpE body)

  interpPrim :: MonadHOComputer c m L5Runtime => PrimName -> [E] -> m L5Runtime
  interpPrim = ip where
    ip Print    [e]         = (asRun "print") e >>= print False >> return (RT $ Num 0)
    ip IsNumber [e]         = foldRT (RT . isNumber) (RT lFalse) <$> interpE e
    ip IsArray  [e]         = foldRT (RT . isArray)  (RT lFalse) <$> interpE e
    ip NewArray [e1,e2]     = RT <$> bind2 allocate (asRun "newArray/e1" e1) (asRun "newArray/e2" e2)
    ip ARef     [e1,e2]     = RT <$> bind2 arrayRef (asRun "aRef" e1) (asRun "aRef" e2)
    ip ASet     [e1,e2,e3]  = RT <$> bind3 arraySet (asPtr "aSet/e1" e1) (asNum "aSet/e2" e2) (asRun "aSet/e3" e3)
    ip ALen     [e]         = RT <$> (asRun "aLen" e >>= arraySize "L5-alen")
    ip p [e1,e2] | isBiop p = RT <$> bind2 (mathOp p) (asRun "primop" e1) (asRun "primop" e2)
    ip p args               = exception $ "wrong arguments to prim: " ++ show p ++ " " ++ mkString "," (show <$> args)

  asRun :: MonadHOComputer c m L5Runtime => String -> E -> m Runtime
  asRun = asRunM return
  asNum :: MonadHOComputer c m L5Runtime => String -> E -> m Runtime
  asNum = asRunM (\r -> Num <$> expectNum r)
  asPtr :: MonadHOComputer c m L5Runtime => String -> E -> m Runtime
  asPtr = asRunM (\r -> Pointer <$> expectPointer "L5/asPtr" r)
  asRunM :: MonadHOComputer c m L5Runtime => (Runtime -> m Runtime) -> String -> E -> m Runtime
  asRunM f caller e = interpE e >>= foldRT f (exception $ caller ++ " expected Runtime, but got a Closure.")

evalClosure :: MonadHOComputer c m L5Runtime => L5 -> m L5Runtime
evalClosure e = interpE e >>= evalClosure' where
  evalClosure' (RT r) = exception $ "expected Closure, but got " ++ show r
  evalClosure' c = return c