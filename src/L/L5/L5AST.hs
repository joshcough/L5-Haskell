module L.L5.L5AST where

import Control.Applicative
import Control.Monad
import Control.Monad.State
import Data.Int
import Data.Foldable hiding (all, foldl)
import Data.List
import Data.Monoid
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Traversable hiding (sequence)
import L.Primitives
import L.Parser.SExpr
import L.Parser.Supply hiding (take)
import L.Variable hiding (freeVars)
import Prelude.Extras

type L5 = E

primVars :: PrimName -> [Variable]
primVars p = Variable <$> take (arityByName p) ["x", "y", "z"]

data E =
    Lambda [Variable] E
  | Var Variable
  | Let Variable E E
  | LetRec Variable E E
  | If E E E
  | NewTuple [E]
  | Begin E E
  | App E [E]
  | LitInt Int64
  | PrimE PrimName
  deriving Eq

instance Show E where show = showSExpr

instance AsSExpr E where
  asSExpr ex = fst $ runState (go ex) (newSupply mempty) where
    go :: E -> State Supply SExpr
    go (Var v)       = return . asSExpr $ v
    go (If v te fe)  = do
      (v', te', fe') <- (,,) <$> go v <*> go te <*> go fe
      return $ asSExpr (sym "if", v', te', fe')
    go (NewTuple es) = do
      List es' <- asSExpr <$> traverse go es
      return $ List (sym "new-tuple" : es')
    go (Begin e1 e2) = do
      (e1', e2') <- (,) <$> go e1 <*> go e2
      return $ asSExpr (sym "begin", e1', e2')
    go (App   e  es) = do
      e'       <- go e
      List es' <- asSExpr <$> traverse go es
      return $ asSExpr (e' : es')
    go (LitInt   i)  = return $ asSExpr i
    go (PrimE    p)  = return $ asSExpr p
    go (Let v e b) = do
      v' <- freshNameFor v
      e' <- go e
      b' <- go b
      return $ asSExpr (sym "let", [(v', e')], b')
    go (LetRec v e b) = do
      v' <- freshNameFor v
      e' <- go e
      b' <- go b
      return $ asSExpr (sym "letrec", [(v', e')], b')
    go (Lambda vs e) = do
      vs' <- traverse freshNameFor vs
      e'  <- go e
      return $ asSExpr (sym "lambda", vs', e')

instance FromSExpr E where
  fromSExpr (List [AtomSym "lambda", List args, b]) = 
    Lambda <$> wellFormedArgList args <*> fromSExpr b
  fromSExpr (List [AtomSym "let",    List [List [arg, e]], b]) = 
    Let    <$> wellFormedArg arg <*> fromSExpr e <*> fromSExpr b
  fromSExpr (List [AtomSym "letrec", List [List [arg, e]], b]) = 
    LetRec <$> wellFormedArg arg <*> fromSExpr e  <*> fromSExpr b
  fromSExpr (List [AtomSym "if", pe, te, fe]) = 
    If     <$> fromSExpr pe      <*> fromSExpr te <*> fromSExpr fe
  fromSExpr (List (AtomSym "new-tuple" : es)) = NewTuple <$> traverse fromSExpr es
  fromSExpr (List [AtomSym "begin", e1, e2])  = Begin    <$> fromSExpr e1 <*> fromSExpr e2
  fromSExpr (List (e : es))                   = App      <$> fromSExpr e  <*> traverse fromSExpr es
  fromSExpr (AtomNum n) = return . LitInt $ fromIntegral n
  fromSExpr (AtomSym v) = maybe 
    (Var <$> wellFormedArgString v) 
    (return . PrimE . primName) 
    (lookupPrim $ Variable v) -- TODO: can i create and used a FromSExpr PrimName?
  fromSExpr bad = fail $ "bad L5-E: " ++ show bad

{-
class BoundVars f where
  boundVars :: f a -> Set Variable

instance BoundVars E where
  boundVars (Var _)         = mempty
  boundVars (Lambda args b) = Set.fromList args <> boundVars b
  boundVars (Let   v e b)   = Set.insert v $ boundVars e <> boundVars b
  boundVars (LetRec v e b)  = Set.insert v $ boundVars e <> boundVars b
  boundVars (If p a b)      = boundVars p <> boundVars a <> boundVars b
  boundVars (NewTuple es)   = mconcat (boundVars <$> es)
  boundVars (Begin e1 e2)   = boundVars e1 <> boundVars e2
  boundVars (App e es)      = boundVars e  <> (mconcat $ boundVars <$> es)
  boundVars (LitInt _)      = mempty
  boundVars (PrimE  _)      = mempty

freeVars :: (Foldable f, Ord a) => f a -> Set a
freeVars = foldMap Set.singleton
-}