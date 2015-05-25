{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}

module L.L4.L4Interp (interpL4) where

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
import L.L3.L3AST (V(..))
import L.L4.L4AST as L4
import L.Parser.SExpr
import L.Primitives (Label(..), PrimName(..), Func(..), name, isBiop)
import L.Util.Utils
import Prelude hiding (print)

interpL4 :: L4 -> String
interpL4 (L4 e fs) = runST $ show <$> runComputation (ie e) where

  lib = Map.fromList $ fmap (\f -> (name f, f)) fs

  interpE :: MonadHOComputer c m Label Label => E -> m (Runtime Label)
  interpE = ie

  -- | interpret an E, building a monadic operation to be run.
  ie :: MonadHOComputer c m Label Label => E -> m (Runtime Label)
  ie (Let v e b)       = do e' <- ie e; locally (Map.insert v e') (ie b)
  ie (If e te fe)      = do e' <- ie e; ie $ if e' /= lFalse then te else fe
  ie (App e es)        = interpApp e es
  ie (NewTuple es)     = traverse ie es >>= newArray
  ie (MakeClosure l e) = ie (NewTuple [VE $ LabelV l, e])
  ie (ClosureProc c)   = do c' <- ie c; arrayRef c' (Num 0)
  ie (ClosureVars c)   = do c' <- ie c; arrayRef c' (Num 1)
  ie (Begin e1 e2)     = ie e1 >> ie e2
  ie (VE v)            = interpV v
  -- Primitive applications (built in functions)
  ie (PrimApp IsNumber [e])        = isNumber <$> ie e
  ie (PrimApp IsArray  [e])        = isArray  <$> ie e
  ie (PrimApp NewArray [s, d])     = bind2 allocate (ie s) (ie d)
  ie (PrimApp ARef [a, loc])       = bind2 arrayRef (ie a) (ie loc)
  ie (PrimApp ASet [a, loc, v])    = bind3 arraySet (ie a) (ie loc) (ie v)
  ie (PrimApp ALen [a])            = ie a >>= arraySize "L4-alen"
  ie (PrimApp Print [e])           = ie e >>= print False >> return (Num 0)
  ie (PrimApp b [l, r]) | isBiop b = bind2 (mathOp b) (ie l) (ie r)
  ie (PrimApp p es)                = exception $ show p ++ " applied to wrong args: " ++ showSExpr es

  interpV :: MonadHOComputer c m Label Label => V -> m (Runtime Label)
  interpV (VarV v)   = use env >>= envLookup v
  interpV (NumV i)   = return $ Num i
  interpV (LabelV l) = return $ Runtime l

  -- | function application (f v...)
  interpApp :: MonadHOComputer c m Label Label => E -> [E] -> m (Runtime Label)
  interpApp f es = do
    (Runtime label)    <- interpE f
    rs                 <- traverse interpE es
    (Func _ args body) <- libLookup label lib
    env                <- use env
    locally (\_ -> Map.union (Map.fromList $ zip args rs) env) (interpE body)
