{-# LANGUAGE TypeFamilies #-}

module L.L5.L5AST where

import Bound
import Bound.Var
import Control.Applicative
import Control.Monad
import Data.Int
import Data.Foldable hiding (all)
import Data.List
import Data.Monoid
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Traversable hiding (sequence)
import L.L1L2AST (Variable(..))
import L.L3.L3AST (Prim(..), PrimName(..), PrimLookup(..), arityByName, primName)
import L.Read
import Prelude.Extras

type L5 = (E Variable)

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

instance AsSExpr a => Show (E a) where show = showSExpr

instance AsSExpr a => AsSExpr (E a) where
  asSExpr (Var    v)     = asSExpr v
{-  asSExpr (Lambda vs e)  = asSExpr (sym "lambda", fmap asSExpr vs, e)
  asSExpr (Let    v e b) = asSExpr (sym "let", (asSExpr v, e), b)
  asSExpr (LetRec v e b) = asSExpr (sym "letrec", (asSExpr v, e), b)
  asSExpr (If v te fe)   = asSExpr (sym "if", v, te, fe)
  asSExpr (NewTuple es)  = List    (sym "new-tuple" : fmap asSExpr es)
  asSExpr (Begin e1 e2)  = asSExpr (sym "begin", e1, e2)
  asSExpr (App e es)     = asSExpr (asSExpr e : fmap asSExpr es)
  asSExpr (LitInt i)     = AtomNum i
  asSExpr (PrimE  p)     = sym $ show p
-}
-- TODO: join all errors together?
wellFormedArgList :: [SExpr] -> Either String [Variable]
wellFormedArgList = traverse wellFormedArg where

wellFormedArg :: SExpr -> Either String Variable
wellFormedArg (AtomSym arg) = wellFormedArgString arg
wellFormedArg bad           = fail $ "invalid argument name: " ++ show bad  

-- TODO: test for valid variable name?
-- it might sort of be handled automatically in Read, but
-- still might be worth doing something here. consider it..
wellFormedArgString :: String -> Either String Variable
wellFormedArgString arg = return $ Variable arg

--abstract :: Monad f => (a -> Maybe b) -> f a -> Scope b f a
instance a ~ Variable => FromSExpr (E a) where
  fromSExpr (List [AtomSym "lambda", List args, b]) = 
    lambda <$> wellFormedArgList args <*> fromSExpr b
  fromSExpr (List [AtomSym "let",    List [List [arg, e]], b]) = 
    let_   <$> wellFormedArg arg <*> fromSExpr e <*> fromSExpr b
  fromSExpr (List [AtomSym "letrec", List [List [arg, e]], b]) = 
    letrec <$> wellFormedArg arg <*> fromSExpr e  <*> fromSExpr b
  fromSExpr (List [AtomSym "if", pe, te, fe]) = 
    If <$> fromSExpr pe <*> fromSExpr te <*> fromSExpr fe
  fromSExpr (List (AtomSym "new-tuple" : es)) = NewTuple <$> traverse fromSExpr es
  fromSExpr (List [AtomSym "begin", e1, e2])  = Begin    <$> fromSExpr e1 <*> fromSExpr e2
  fromSExpr (List (e : es))                   = 
    App <$> fromSExpr e  <*> traverse fromSExpr es
  fromSExpr (AtomNum n) = return $ LitInt (fromIntegral n)
  fromSExpr (AtomSym v) = maybe 
    (Var <$> wellFormedArgString v) 
    (return . PrimE . primName) 
    (lookupPrim $ Variable v)
  fromSExpr bad = fail $ "bad L5-E: " ++ show bad

class BoundVars f where
  boundVars :: f a -> Set Variable

instance (BoundVars f, Monad f) => BoundVars (Scope b f) where
  boundVars = boundVars . fromScope

instance BoundVars E where
  boundVars (Var a)         = mempty
  boundVars (Lambda args s) = Set.fromList args <> boundVars s
  boundVars (Let   v e b)   = Set.insert v $ boundVars e <> boundVars b
  boundVars (LetRec v e b)  = Set.insert v $ boundVars e <> boundVars b
  boundVars (If p a b)      = boundVars p <> boundVars a <> boundVars b 
  boundVars (NewTuple es)   = Set.unions (boundVars <$> es)
  boundVars (Begin e1 e2)   = boundVars e1 <> boundVars e2 
  boundVars (App e es)      = boundVars e  <> (Set.unions $ boundVars <$> es)
  boundVars (LitInt i)      = mempty
  boundVars (PrimE  p)      = mempty

freeVars :: (Foldable f, Ord a) => f a -> Set a   
freeVars = foldMap Set.singleton

--fromScope :: Monad f => Scope b f a -> f (Var b a)
-- Let      Variable (E a) (Scope () E a)
-- Scope () E a -> E (Var () a)
pretty :: (Variable -> Variable) -> E Variable -> String
pretty f0 es0 = go f0 names0 Set.empty es0 where
  go :: (a -> Variable) -> [Variable] -> Set Variable -> E a -> String
  go f names@(n:ns) lbs (Let v e b)
    | v `Set.member` lbs = "let " ++ show n ++ " = " ++ go f names lbs e ++ " in " 
                                  ++ go (unvar (const n) f) ns (Set.insert n lbs) (fromScope b)
    | otherwise = "let"
  bs = boundVars es0
  fs = freeVars es0
  vs = bs <> fs
  names0 = filter (not . flip Set.member vs) [Variable [v] | v <- ['a'..'z']]

{-
(a -> r)
(b -> r)
(Either b a -> r)

f (Var b (f a))
f (Either b a)
-}