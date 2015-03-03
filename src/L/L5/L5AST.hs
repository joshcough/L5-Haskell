module L.L5.L5AST where

import Control.Applicative
import Control.Monad
import Data.Int
import Data.Traversable hiding (sequence)
import L.L1L2AST (Variable(..))
import L.L3.L3AST (Prim(..), PrimName(..), arityByName, primName, primByRawNameMaybe)
import L.Read

type L5 = (E Variable)

primVars :: PrimName -> [Variable]
primVars p = Variable <$> take (arityByName p) ["x", "y", "z"]

data E a =
    Lambda [a] (E a)
  | Var Variable
  | Let Variable (E a) (E a)
  | LetRec Variable (E a) (E a)
  | IfStatement (E a) (E a) (E a)
  | NewTuple [(E a)]
  | Begin (E a) (E a)
  | App (E a) [(E a)]
  | LitInt Int64
  | PrimE PrimName
  deriving Eq

instance AsSExpr a => Show (E a) where show = showSExpr

instance AsSExpr a => AsSExpr (E a) where
  asSExpr (Lambda vs e)         = asSExpr (sym "lambda", fmap asSExpr vs, e)
  asSExpr (Let    v e b)        = asSExpr (sym "let", (asSExpr v, e), b)
  asSExpr (LetRec v e b)        = asSExpr (sym "letrec", (asSExpr v, e), b)
  asSExpr (IfStatement v te fe) = asSExpr (sym "if", v, te, fe)
  asSExpr (NewTuple es)         = List    (sym "new-tuple" : fmap asSExpr es)
  asSExpr (Begin e1 e2)         = asSExpr (sym "begin", e1, e2)
  asSExpr (App e es)            = asSExpr (asSExpr e : fmap asSExpr es)
  asSExpr (LitInt i)            = AtomNum i
  asSExpr (PrimE  p)            = sym $ show p
  asSExpr (Var    v)            = asSExpr v

instance FromSExpr a => FromSExpr (E a) where
  fromSExpr = f where
    f :: FromSExpr a => SExpr -> Either String (E a)
    f (List [AtomSym "lambda", List args, b])            = Lambda <$> traverse fromSExpr args <*> f b
    f (List [AtomSym "let",    List [List [arg, e]], b]) = Let    <$> fromSExpr arg <*> f e   <*> f b
    f (List [AtomSym "letrec", List [List [arg, e]], b]) = LetRec <$> fromSExpr arg <*> f e   <*> f b
    f (List [AtomSym "if", pe, te, fe]) = IfStatement <$> f pe <*> f te <*> f fe
    f (List (AtomSym "new-tuple" : es)) = NewTuple    <$> traverse f es
    f (List [AtomSym "begin", e1, e2])  = Begin       <$> f e1 <*> f e2
    f (List (e : es))                   = App         <$> f e  <*> traverse f es
    f (AtomSym v) = return $ maybe (Var $ Variable v) (PrimE . primName) (primByRawNameMaybe v)
    f (AtomNum n) = return $ LitInt (fromIntegral n)
    f bad = fail $ "bad L5-e: " ++ show bad
