{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}

module L.L5.L5Interp (interpL5) where

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
import L.Interpreter.Runtime hiding (foldRuntime)
import L.L1L2AST hiding (Func, Print)
import L.L3.L3AST (PrimName(..), isBiop)
import L.L5.L5AST
import L.Read
import L.Utils
import Prelude hiding (print)

type L5Monad c m a = (Show a, Eq a, MonadHOComputer c m (L5Runtime a))

interpL5String :: String -> String
interpL5String = interpL5 . either error id . fromSExpr . sread

interpL5 :: L5 -> String
interpL5 e = runST $ show <$> runComputation (interpEMain' e) where
  interpEMain' :: L5Monad c m Variable => L5 -> m Runtime
  interpEMain' = interpEMain

data L5Runtime a = RT Runtime | Closure (Scope Int E a) deriving Eq

foldRT :: (Runtime -> b) -> b -> L5Runtime a -> b
foldRT f b r = case r of { RT r -> f r; Closure _ -> b }

instance a ~ Variable => Show (L5Runtime a) where
  show = foldRT show "<function>"

interpEMain :: L5Monad c m a => E a -> m Runtime
interpEMain e = interpE e >>= return . foldRT id (Num 0)

-- | interpret an E, building a monadic operation to be run.
interpE :: L5Monad c m a => E a -> m (L5Runtime a)
interpE = ie where
  ie (Lambda _ e)       = return $ Closure e
  ie (Var a)            = fail $ "unbound var: " ++ show a
  ie (Let _ e body)     = interpE $ App (Lambda [] $ mapBound (const 0) body) [e]
  ie (LetRec _ e b)     = interpE (inst b) where e' = inst e; inst = instantiate (const e')
  ie (If p t f)         = do r <- interpE p; interpE $ if r == RT lTrue then t else f
  ie (NewTuple es)      = RT <$> (traverse asRun es >>= newArray)
  ie (Begin e1 e2)      = interpE e1 >> interpE e2
  ie (LitInt i)         = return . RT $ Num i
  ie (PrimE p)          = interpE (primlet p) where 
  ie (App (PrimE p) es) = interpPrim p es
  ie (App f es)         = interpE f >>= \r -> case r of
    RT r -> exception $ "expected Closure, but got " ++ show r
    Closure body -> interpE (instantiate (es!!) body)

  interpPrim :: L5Monad c m a => PrimName -> [E a] -> m (L5Runtime a)
  interpPrim = ip where
    ip Print    [e]         = asRun e >>= print False >> return (RT $ Num 0)
    ip IsNumber [e]         = foldRT (RT . isNumber) (RT lFalse) <$> interpE e
    ip IsArray  [e]         = foldRT (RT . isArray)  (RT lFalse) <$> interpE e
    ip NewArray [e1,e2]     = RT <$> bind2 allocate (asRun e1) (asRun e2)
    ip ARef     [e1,e2]     = RT <$> bind2 arrayRef (asRun e1) (asRun e2)
    ip ASet     [e1,e2,e3]  = RT <$> bind3 arraySet (asPtr e1) (asNum e2) (asRun e3)
    ip ALen     [e]         = RT <$> (asRun e >>= arraySize "L5-alen")
    ip p [e1,e2] | isBiop p = RT <$> bind2 (mathOp p) (asRun e1) (asRun e2)
    -- todo: this is broken because of the weird show instance for E Variable
    ip p args               = exception $ "wrong arguments to prim: " ++ show p -- ++ mkString "," (show <$> args)

  asRun :: L5Monad c m a => E a -> m Runtime
  asRun = asRunM return
  asNum :: L5Monad c m a => E a -> m Runtime
  asNum = asRunM (\r -> Num <$> expectNum r)
  asPtr :: L5Monad c m a => E a -> m Runtime
  asPtr = asRunM (\r -> Pointer <$> expectPointer "L5/asPtr" r)
  asRunM :: L5Monad c m a => (Runtime -> m Runtime) -> E a -> m Runtime
  asRunM f e = interpE e >>= foldRT f (exception $ "expected Runtime, but got a Closure")
