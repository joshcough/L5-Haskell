module L.L5.L5AST where

import Control.Applicative
import Control.Monad
import Data.Int
import Data.Traversable hiding (sequence)
import L.L1L2AST (Variable(..))
import L.L3.L3AST (Prim(..), PrimName(..), arityByName, primName, primByRawNameMaybe)
import L.Read

type L5 = E
type Arity = Int

primVars :: PrimName -> [Variable]
primVars p = Variable <$> take (arityByName p) ["x", "y", "z"]

data E =
    Lambda [Variable] E
  | Var Variable
  | Let Variable E E
  | LetRec Variable E E
  | IfStatement E E E
  | NewTuple [E]
  | Begin E E
  | App E [E]
  | LitInt Int64
  | PrimE PrimName
  deriving Eq

instance Show E where
  show = show . asSExpr

instance AsSExpr E where
  asSExpr = f where
    f (Lambda vs e)         = asSExpr (sym "lambda", fmap asSExpr vs, f e)
    f (Let v e b)           = asSExpr (sym "let", (asSExpr v, f e), f b)
    f (LetRec v e b)        = asSExpr (sym "letrec", (asSExpr v, f e), f b)
    f (IfStatement v te fe) = asSExpr (sym "if", f v, f te, f fe)
    f (NewTuple es)         = List    (sym "new-tuple" : fmap f es)
    f (Begin e1 e2)         = asSExpr (sym "begin", f e1, f e2)
    f (App (PrimE p) es)    = asSExpr (sym (show p) : fmap f es)
    f (LitInt i)            = AtomNum i
    f (PrimE p)             = sym $ show p
    f (Var v)               = asSExpr v

instance FromSExpr E where
  fromSExpr = f where
    f (List [AtomSym "lambda", List args, b]) = liftM2 Lambda (traverse fromSExpr args)  (f b)
    f (List [AtomSym "let",    List [List [arg, e]], b]) = liftM3 Let    (fromSExpr arg) (f e) (f b)
    f (List [AtomSym "letrec", List [List [arg, e]], b]) = liftM3 LetRec (fromSExpr arg) (f e) (f b)
    f (List [AtomSym "if", pe, te, fe]) = liftM3 IfStatement (f pe) (f te) (f fe)
    f (List (AtomSym "new-tuple" : es)) = liftM  NewTuple (traverse f es)
    f (List [AtomSym "begin", e1, e2])  = liftM2 Begin (f e1) (f e2)
    f (List (e : es)) = liftM2 App (f e) (traverse f es)
    f (AtomSym v) = Right $ maybe (Var $ Variable v) (PrimE . primName) (primByRawNameMaybe v)
    f (AtomNum n) = Right $ LitInt (fromIntegral n)
