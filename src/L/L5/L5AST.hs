{-# LANGUAGE TypeFamilies #-}

module L.L5.L5AST where

import Bound
import Bound.Var
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
import L.Parser.Supply
import L.Variable hiding (freeVars)
import Prelude.Extras

type L5 = E Variable

primVars :: PrimName -> [Variable]
primVars p = Variable <$> take (arityByName p) ["x", "y", "z"]

data E a =
    Lambda   [Variable] (Scope Int E a)
  | Var      a
  | Let      Variable (E a) (Scope () E a)
  | LetRec   Variable (Scope () E a) (Scope () E a)
  | If       (E a) (E a) (E a)
  | NewTuple [E a]
  | Begin    (E a) (E a)
  | App      (E a) [E a]
  | LitInt   Int64
  | PrimE    PrimName
  deriving (Eq,Ord,Read)

lambda :: [Variable] -> E Variable -> E Variable
lambda vs e = Lambda vs (abstract (flip elemIndex vs) e)

let_ :: Variable -> E Variable -> E Variable -> E Variable
let_ v e b = Let v e (abstract (matchVar v) b)

letrec :: Variable -> E Variable -> E Variable -> E Variable
letrec v e b = LetRec v (f e) (f b) where f = abstract (matchVar v)

matchVar :: (MonadPlus m, Eq a) => a -> a -> m ()
matchVar a = guard . (a ==)

app :: SExpr -> [SExpr] -> Either String (E Variable)
app e es = case e of
  (AtomSym s) | s `Set.member` keywords -> fail $ "bad L5-E: " ++ show (List $ e : es)
  _                                     -> App <$> fromSExpr e <*> traverse fromSExpr es

keywords :: Set String
keywords = Set.fromList ["lambda", "let", "letrec", "if", "new-tuple", "begin"]

primlet :: PrimName -> E a
primlet p = l where 
  a = App (PrimE p) [Var (B 0), Var (B 1)]
  l = Lambda [Variable "x", Variable "y"] (toScope a)

instance Functor     E where fmap    = fmapDefault
instance Foldable    E where foldMap = foldMapDefault
instance Applicative E where pure = Var; (<*>) = ap

instance Monad E where
  return = Var
  Lambda vs e  >>= f = Lambda vs (e >>>= f)
  Var a        >>= f = f a
  Let    v e b >>= f = Let    v  (e >>=  f) (b >>>= f)
  LetRec v e b >>= f = LetRec v  (e >>>= f) (b >>>= f)
  If p a b     >>= f = If        (p >>=  f) (a >>=  f) (b >>= f)
  NewTuple es  >>= f = NewTuple  (map (>>= f) es)
  Begin e1 e2  >>= f = Begin     (e1 >>= f) (e2 >>= f)
  App a args   >>= f = App       (a  >>= f) (map (>>= f) args)
  LitInt n     >>= _ = LitInt    n
  PrimE  p     >>= _ = PrimE     p

instance Traversable E where
  traverse f (Var a)        = Var       <$> f a
  traverse f (Lambda vs s)  = Lambda vs <$> traverse f s
  traverse f (Let    v e b) = Let    v  <$> traverse f e <*> traverse f b
  traverse f (LetRec v e b) = LetRec v  <$> traverse f e <*> traverse f b
  traverse f (If p a b)     = If        <$> traverse f p <*> traverse f a <*> traverse f b 
  traverse f (NewTuple es)  = NewTuple  <$> traverse (traverse f) es
  traverse f (Begin e1 e2)  = Begin     <$> traverse f e1 <*> traverse f e2
  traverse f (App e es)     = App       <$> traverse f e  <*> traverse (traverse f) es
  traverse _ (LitInt i)     = pure       $ LitInt i
  traverse _ (PrimE  p)     = pure       $ PrimE p

instance Eq1   E where (==#)      = (==)
instance Ord1  E where compare1   = compare
instance Read1 E where readsPrec1 = readsPrec  
--instance Show1 E where showsPrec1 = showsPrec  

instance a ~ Variable => Show (E a) where show = showSExpr

instance a ~ Variable => AsSExpr (E a) where 
  asSExpr ex = fst $ runState (go id ex) (newSupply $ boundVars ex <> freeVars ex) where
    go :: (a -> Variable) -> E a -> State Supply SExpr
    go f (Var v)       = return . asSExpr $ f v
    go f (If v te fe)  = do
      (v', te', fe') <- (,,) <$> go f v <*> go f te <*> go f fe
      return $ asSExpr (sym "if", v', te', fe')
    go f (NewTuple es) = do 
      List es' <- asSExpr <$> traverse (go f) es
      return $ List (sym "new-tuple" : es')
    go f (Begin e1 e2) = do
      (e1', e2') <- (,) <$> go f e1 <*> go f e2
      return $ asSExpr (sym "begin", e1', e2')
    go f (App   e  es) = do 
      e'       <- go f e
      List es' <- asSExpr <$> traverse (go f) es
      return $ asSExpr (e' : es')
    go _ (LitInt   i)  = return $ asSExpr i
    go _ (PrimE    p)  = return $ asSExpr p
    go f (Let v e b) = do
      v' <- supplyNameM v
      e' <- go f e
      b' <- go (unvar (const v') f) (fromScope b)
      return $ asSExpr (sym "let", [(v', e')], b')
    go f (LetRec v e b) = do
      v' <- supplyNameM v
      e' <- go (unvar (const v') f) (fromScope e)
      b' <- go (unvar (const v') f) (fromScope b)
      return $ asSExpr (sym "letrec", [(v', e')], b')
    go f (Lambda vs e) = do
      vs' <- traverse supplyNameM vs
      e'  <- go (unvar (vs' !!) f) (fromScope e)
      return $ asSExpr (sym "lambda", vs', e')

instance a ~ Variable => FromSExpr (E a) where
  fromSExpr (List [AtomSym "lambda", List args, b]) = 
    lambda <$> wellFormedArgList args <*> fromSExpr b
  fromSExpr (List [AtomSym "let",    List [List [arg, e]], b]) = 
    let_   <$> wellFormedArg arg <*> fromSExpr e <*> fromSExpr b
  fromSExpr (List [AtomSym "letrec", List [List [arg, e]], b]) = 
    letrec <$> wellFormedArg arg <*> fromSExpr e  <*> fromSExpr b
  fromSExpr (List [AtomSym "if", pe, te, fe]) = 
    If     <$> fromSExpr pe      <*> fromSExpr te <*> fromSExpr fe
  fromSExpr (List (AtomSym "new-tuple" : es)) = NewTuple <$> traverse fromSExpr es
  fromSExpr (List [AtomSym "begin", e1, e2])  = Begin    <$> fromSExpr e1 <*> fromSExpr e2
  fromSExpr (List (e : es))                   = app e es
  fromSExpr (AtomNum n) = return . LitInt $ fromIntegral n
  fromSExpr (AtomSym v) = maybe 
    (Var <$> wellFormedArgString v) 
    (return . PrimE . primName) 
    (lookupPrim $ Variable v) -- TODO: can i create and used a FromSExpr PrimName?
  fromSExpr bad = fail $ "bad L5-E: " ++ show bad

class BoundVars f where
  boundVars :: f a -> Set Variable

instance (BoundVars f, Monad f) => BoundVars (Scope b f) where
  boundVars = boundVars . fromScope

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
