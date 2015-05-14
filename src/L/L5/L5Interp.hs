{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}

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
  ie (LetRec v e body)  = locally newEnv (interpE body) where
     -- todo: this is all weird now that letrec doesnt have a list of functions...
     newEnv old = new where new = Map.union (Map.fromList ([toClosure (v, e)])) old
                            toClosure (v, Lambda vs e) = (v, Closure vs e new)
                            toClosure (v, e) = error $ "non-lambda in letrec: " ++ show (v, e)
  ie (If p t f)         = do r <- ie p; ie $ if r == RT lTrue then t else f
  ie (NewTuple es)      = RT <$> (traverse asRun es >>= newArray)
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
    ip Print    [e]         = asRun e >>= print False >> return (RT $ Num 0)
    ip IsNumber [e]         = foldRT (RT . isNumber) (RT lFalse) <$> interpE e
    ip IsArray  [e]         = foldRT (RT . isArray)  (RT lFalse) <$> interpE e
    ip NewArray [e1,e2]     = RT <$> bind2 allocate (asRun e1) (asRun e2)
    ip ARef     [e1,e2]     = RT <$> bind2 arrayRef (asRun e1) (asRun e2)
    ip ASet     [e1,e2,e3]  = RT <$> bind3 arraySet (asPtr e1) (asNum e2) (asRun e3)
    ip ALen     [e]         = RT <$> (asRun e >>= arraySize "L5-alen")
    ip p [e1,e2] | isBiop p = RT <$> bind2 (mathOp p) (asRun e1) (asRun e2)
    ip p args               = exception $ "wrong arguments to prim: " ++ mkString "," (show <$> args)

  asRun :: MonadHOComputer c m L5Runtime => E -> m Runtime
  asRun = asRunM return
  asNum :: MonadHOComputer c m L5Runtime => E -> m Runtime
  asNum = asRunM (\r -> Num <$> expectNum r)
  asPtr :: MonadHOComputer c m L5Runtime => E -> m Runtime
  asPtr = asRunM (\r -> Pointer <$> expectPointer "L5/asPtr" r)
  asRunM :: MonadHOComputer c m L5Runtime => (Runtime -> m Runtime) -> E -> m Runtime
  asRunM f e = interpE e >>= foldRT f (exception $ "expected Runtime, but got a Closure")

evalClosure :: MonadHOComputer c m L5Runtime => L5 -> m L5Runtime
evalClosure e = interpE e >>= evalClosure' where
  evalClosure' (RT r) = exception $ "expected Closure, but got " ++ show r
  evalClosure' c = return c