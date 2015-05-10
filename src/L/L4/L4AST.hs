{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE InstanceSigs #-}

module L.L4.L4AST where

import Bound
import Bound.Var
import Control.Applicative
import Control.Monad
import Control.Monad.State
import Data.Int
import Data.Foldable hiding (all, concat, foldl)
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

type L4Func a = Func (E a)
data L4 a     = L4 (E a) [L4Func a]
data V a      = VarV a | NumV Int64 | LabelV Label deriving (Eq,Ord,Read)

data E a =
    VE       (V a)
  | Let      Variable (E a) (Scope () E a)
  | If       (E a) (E a) (E a)
  | App      (E a) [E a] -- renamed from FUNCALL
  | PrimApp PrimName [E a]
  | NewTuple [E a]
  | Begin    (E a) (E a)
  | MakeClosure Label (E a)
  | ClosureProc (E a)
  | ClosureVars (E a)
  deriving (Eq,Ord,Read)

let_ :: Variable -> E Variable -> E Variable -> E Variable
let_ v e b = Let v e (abstract (matchVar v) b)

matchVar :: (MonadPlus m, Eq a) => a -> a -> m ()
matchVar a = guard . (a ==)

app :: SExpr -> [SExpr] -> Either String (E Variable)
app e es = case e of
  (AtomSym s) | s `Set.member` keywords -> fail $ "bad L4-E: " ++ show (List $ e : es)
  _                                     -> App <$> fromSExpr e <*> traverse fromSExpr es

keywords :: Set String
keywords = Set.fromList [
  "let", "letrec", "if", "new-tuple", "begin", "make-closure", "closure-proc", "closure-vars"]

instance Functor     E where fmap    = fmapDefault
instance Foldable    E where foldMap = foldMapDefault
instance Applicative E where pure = VE . VarV; (<*>) = ap

instance Monad E where
  return = VE . VarV
  VE (VarV a)     >>= f = f a
  VE (NumV n)     >>= f = VE (NumV n)
  VE (LabelV l)   >>= f = VE (LabelV l)
  Let v e b       >>= f = Let v       (e >>=  f) (b >>>= f)
  If  p a b       >>= f = If          (p >>=  f) (a >>=  f) (b >>= f)
  App a args      >>= f = App         (a >>= f) (map (>>= f) args)
  PrimApp p args  >>= f = PrimApp     p         (map (>>= f) args)
  NewTuple es     >>= f = NewTuple    (map (>>= f) es)
  Begin e1 e2     >>= f = Begin       (e1  >>= f) (e2 >>= f)
  MakeClosure l e >>= f = MakeClosure l (e >>= f)
  ClosureProc e   >>= f = ClosureProc (e >>= f)
  ClosureVars e   >>= f = ClosureVars (e >>= f)

instance Traversable E where
  traverse :: Applicative f => (a -> f b) -> E a -> f (E b)
  traverse f (VE (VarV a))     = VE . VarV <$> f a
  traverse f (VE (NumV n))     = pure $ VE (NumV n)
  traverse f (VE (LabelV l))   = pure $ VE (LabelV l)
  traverse f (Let v e b)       = Let v         <$> traverse f e <*> traverse f b
  traverse f (If  p a b)       = If            <$> traverse f p <*> traverse f a <*> traverse f b
  traverse f (App e es)        = App           <$> traverse f e <*> traverse (traverse f) es
  traverse f (PrimApp p es)    = PrimApp p     <$> traverse (traverse f) es
  traverse f (NewTuple es)     = NewTuple      <$> traverse (traverse f) es
  traverse f (Begin e1 e2)     = Begin         <$> traverse f e1 <*> traverse f e2
  traverse f (MakeClosure l e) = MakeClosure l <$> traverse f e
  traverse f (ClosureProc e)   = ClosureProc   <$> traverse f e
  traverse f (ClosureVars e)   = ClosureVars   <$> traverse f e

instance Eq1   E where (==#)      = (==)
instance Ord1  E where compare1   = compare
instance Read1 E where readsPrec1 = readsPrec

instance a ~ Variable => Show (E a)  where show = showSExpr
instance a ~ Variable => Show (L4 a) where show = showSExpr

instance a ~ Variable => AsSExpr (E a) where
  asSExpr ex = fst $ runState (go id ex) (newSupply $ boundVars ex <> freeVars ex) where
    go :: (a -> Variable) -> E a -> State Supply SExpr
    go f (VE (VarV v))     = return . asSExpr $ f v
    go f (VE (NumV n))     = return . asSExpr $ n
    go f (VE (LabelV l))   = return . asSExpr $ l
    go f (Let v e b)       = do
      v' <- freshNameForS v
      e' <- go f e
      b' <- go (unvar (const v') f) (fromScope b)
      return $ asSExpr (sym "let", [(v', e')], b')
    go f (If v te fe)      = do
      (v', te', fe') <- (,,) <$> go f v <*> go f te <*> go f fe
      return $ asSExpr (sym "if", v', te', fe')
    go f (App   e  es)     = do
      e'       <- go f e
      List es' <- asSExpr <$> traverse (go f) es
      return $ asSExpr (e' : es')
    go f (PrimApp p es)    = do
      p'       <- return $ asSExpr p
      List es' <- asSExpr <$> traverse (go f) es
      return $ asSExpr (p' : es')
    go f (NewTuple es)     = do
      List es' <- asSExpr <$> traverse (go f) es
      return $ List (sym "new-tuple" : es')
    go f (Begin e1 e2)     = do
      (e1', e2') <- (,) <$> go f e1 <*> go f e2
      return $ asSExpr (sym "begin", e1', e2')
    go f (MakeClosure l e) = do
      e' <- go f e
      return $ asSExpr (sym "make-closure", l, e')
    go f (ClosureProc e)   = do
      e' <- go f e
      return $ asSExpr (sym "closure-proc", e')
    go f (ClosureVars e)   = do
      e' <- go f e
      return $ asSExpr (sym "closure-vars", e')

instance a ~ Variable => AsSExpr (L4 a) where
  asSExpr (L4 e fs) = List $ asSExpr e : fmap asSExpr fs

-- p ::= (e (l (x ...) e) ...)
instance a ~ Variable => FromSExpr (L4 a) where
  fromSExpr (List (main : funcs)) = L4 <$> fromSExpr main <*> traverse fromSExpr funcs
  fromSExpr bad = fail $ concat ["Bad L4 Program: ", show bad]

-- v :: = x | l | num
instance a ~ Variable => FromSExpr (V a) where
  fromSExpr (AtomSym l@(':' : _)) = Right . LabelV $ Label l
  fromSExpr (AtomSym v) = return . VarV $ Variable v
  fromSExpr (AtomNum n) = return $ NumV (fromIntegral n)
  fromSExpr bad         = fail  $ concat ["Parse Error: 'bad V' in: ", show bad]

instance a ~ Variable => AsSExpr (V a) where
  asSExpr (VarV (Variable v)) = AtomSym v
  asSExpr (NumV n)            = AtomNum n
  asSExpr (LabelV (Label l))  = AtomSym l

--class (Monad m) => FromSExpr (m a) where
--    fromSExpr :: SExpr -> m a
-- TODO: this is bad because if they give the wrong number of args to a prim,
-- TODO: it'll parse a FunCall
instance a ~ Variable => FromSExpr (E a) where
  fromSExpr = f where
    f (AtomNum n) = return . VE .  NumV $ fromIntegral n
    f (List [AtomSym "let", List [List [arg, e]], b]) =
      let_  <$> wellFormedArg arg <*> fromSExpr e <*> fromSExpr b
    f (List [AtomSym "if", pe, te, fe])     = If <$> f pe   <*> f te        <*> f fe
    f (List (AtomSym "new-tuple" : es))     = NewTuple      <$> traverse f es
    f (List [AtomSym "begin", e1, e2])      = Begin         <$> f e1        <*> f e2
    f (List [AtomSym "make-closure", l, e]) = MakeClosure   <$> fromSExpr l <*> f e
    f (List [AtomSym "closure-proc", e])    = ClosureProc   <$> f e
    f (List [AtomSym "closure-vars", e])    = ClosureVars   <$> f e
    f (List [AtomSym "new-array", s, e])    = parsePrimApp2 NewArray s e
    f (List [AtomSym "aref", a, loc])       = parsePrimApp2 ARef a loc
    f (List [AtomSym "aset", a, loc, e])    = parsePrimApp3 ASet a loc e
    f (List [AtomSym "alen", e])            = parsePrimApp1 ALen e
    f (List [AtomSym "print", e])           = parsePrimApp1 Print e
    f (List [AtomSym "+",    l, r])         = parsePrimApp2 Add l r
    f (List [AtomSym "-",    l, r])         = parsePrimApp2 Sub l r
    f (List [AtomSym "*",    l, r])         = parsePrimApp2 Mult l r
    f (List [AtomSym "<",    l, r])         = parsePrimApp2 LessThan l r
    f (List [AtomSym "<=",   l, r])         = parsePrimApp2 LTorEQ l r
    f (List [AtomSym "=",    l, r])         = parsePrimApp2 EqualTo l r
    f (List [AtomSym "number?", e])         = parsePrimApp1 IsNumber e
    f (List [AtomSym "a?",      e])         = parsePrimApp1 IsArray e

    f (List (e : es)) = App <$> f e <*> traverse f es
    f v = VE <$> fromSExpr v

    parsePrimApp1 p e = PrimApp p . return <$> fromSExpr e
    parsePrimApp2 b l r = (\v1 v2 -> PrimApp b [v1,v2]) <$> fromSExpr l <*> fromSExpr r
    parsePrimApp3 b e1 e2 e3 =
      (\v1 v2 v3 -> PrimApp b [v1,v2,v3]) <$> fromSExpr e1 <*> fromSExpr e2 <*> fromSExpr e3

{-
TODO: absolutely must resolve all of this crap
instance a ~ Variable => FromSExpr (E a) where

  fromSExpr (List (e : es))                   = app e es
  fromSExpr (AtomSym v) = maybe
    (Var <$> wellFormedArgString v)
    (return . PrimE . primName)
    (lookupPrim $ Variable v)
  fromSExpr bad = fail $ "bad L5-E: " ++ show bad
-}


class BoundVars f where
  boundVars :: f a -> Set Variable

instance (BoundVars f, Monad f) => BoundVars (Scope b f) where
  boundVars = boundVars . fromScope

instance BoundVars E where
  boundVars (VE _)            = mempty
  boundVars (Let v e b)       = Set.insert v $ boundVars e <> boundVars b
  boundVars (If p a b)        = boundVars p  <> boundVars a <> boundVars b
  boundVars (App e es)        = boundVars e  <> (mconcat $ boundVars <$> es)
  boundVars (PrimApp p es)    = mconcat $ boundVars <$> es
  boundVars (NewTuple es)     = mconcat $ boundVars <$> es
  boundVars (Begin e1 e2)     = boundVars e1 <> boundVars e2
  boundVars (MakeClosure l e) = boundVars e
  boundVars (ClosureProc e)   = boundVars e
  boundVars (ClosureVars e)   = boundVars e

freeVars :: (Foldable f, Ord a) => f a -> Set a
freeVars = foldMap Set.singleton
