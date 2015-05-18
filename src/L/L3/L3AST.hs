module L.L3.L3AST (
  L3Func
 ,L3(..)
 ,D(..)
 ,V(..)
 ,E(..)
 ,foldV
 ,module L.Primitives
) where

import Control.Applicative
import Control.Monad
import Data.Int
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (fromJust)
import Data.Monoid
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Traversable hiding (sequence)
import L.L1.L1L2AST hiding (Func, Print)
import L.Primitives
import L.Parser.SExpr
import L.Variable
import Prelude hiding (print)

type L3Func = Func E
data L3   = L3 E [L3Func]
data E    = Let Variable D E | If V E E  | DE D
data D    = App V [V]        | PrimApp PrimName [V] | VD V |
            NewTuple  [V]    | MakeClosure Label V  | ClosureProc V | ClosureVars V
data V = VarV Variable | NumV Int64 | LabelV Label

instance (AsSExpr a, Show a) => Show (Func a) where show = showSExpr
instance Show V        where show = showSExpr
instance Show D        where show = showSExpr
instance Show E        where show = showSExpr
instance Show L3       where show = showSExpr

instance AsSExpr D where
  asSExpr (App v vs)        = asSExpr (asSExpr v : fmap asSExpr vs)
  asSExpr (PrimApp p vs)    = asSExpr (asSExpr p : fmap asSExpr vs)
  asSExpr (VD v)            = asSExpr v
  asSExpr (NewTuple vs)     = asSExpr (sym "new-tuple" : fmap asSExpr vs)
  asSExpr (MakeClosure l v) = asSExpr (sym "make-closure", l, v)
  asSExpr (ClosureProc v)   = asSExpr (sym "closure-proc", v)
  asSExpr (ClosureVars v)   = asSExpr (sym "closure-vars", v)

instance AsSExpr E where
  asSExpr (Let v d e)  = asSExpr (sym "let", asSExpr [(v, d)], e)
  asSExpr (If v te fe) = asSExpr (sym "if", v, te, fe)
  asSExpr (DE d)       = asSExpr d

instance AsSExpr L3 where
  asSExpr (L3 e fs) = List $ asSExpr e : fmap asSExpr fs

-- v :: = x | l | num
instance FromSExpr V where
  fromSExpr (AtomSym l@(':' : _)) = Right . LabelV $ Label l
  fromSExpr (AtomSym v) = return . VarV $ Variable v
  fromSExpr (AtomNum n) = return $ NumV (fromIntegral n)
  fromSExpr bad         = Left  $ concat ["Parse Error: 'bad V' in: ", show bad]

instance AsSExpr V where
  asSExpr (VarV (Variable v)) = AtomSym v
  asSExpr (NumV n)            = AtomNum n
  asSExpr (LabelV (Label l))  = AtomSym l

{-
d ::= (biop v v) | (pred v) | (v v ...) | (new-array v v) | (new-tuple v ...)
      (aref v v) | (aset v v v) | (alen v)
      (print v) | (make-closure l v) | (closure-proc v) | (closure-vars v)
      v
biop ::= + | - | * | < | <= | =
pred ::= number? | a?
 -}
instance FromSExpr D where
  fromSExpr = f where
    f (List [AtomSym "make-closure", l, v]) = MakeClosure <$> fromSExpr l <*> fromSExpr v
    f (List [AtomSym "closure-proc", v])    = ClosureProc <$> fromSExpr v
    f (List [AtomSym "closure-vars", v])    = ClosureVars <$> fromSExpr v
    -- TODO: this is bad because if they give the wrong number of args to a prim,
    -- TODO: it'll parse a App
    f (List [AtomSym "+",    l, r])         = parsePrimApp2 Add      l r
    f (List [AtomSym "-",    l, r])         = parsePrimApp2 Sub      l r
    f (List [AtomSym "*",    l, r])         = parsePrimApp2 Mult     l r
    f (List [AtomSym "<",    l, r])         = parsePrimApp2 LessThan l r
    f (List [AtomSym "<=",   l, r])         = parsePrimApp2 LTorEQ   l r
    f (List [AtomSym "=",    l, r])         = parsePrimApp2 EqualTo  l r
    f (List [AtomSym "number?", v])         = parsePrimApp1 IsNumber v
    f (List [AtomSym "a?",      v])         = parsePrimApp1 IsArray  v
    f (List [AtomSym "new-array", s, v])    = parsePrimApp2 NewArray s v
    f (List (AtomSym "new-tuple" : vs))     = NewTuple <$> traverse fromSExpr vs
    f (List [AtomSym "aref", a, loc])       = parsePrimApp2 ARef     a loc
    f (List [AtomSym "aset", a, loc, v])    = parsePrimApp3 ASet     a loc v
    f (List [AtomSym "alen",  v])           = parsePrimApp1 ALen     v
    f (List [AtomSym "print", v])           = parsePrimApp1 Print    v
    f (List (v : vs))                       = App <$> fromSExpr v <*> traverse fromSExpr vs
    f v = VD <$> fromSExpr v

    parsePrimApp1 p v   = (PrimApp p . return) <$> fromSExpr v
    parsePrimApp2 b l r = (\v1 v2 -> PrimApp b [v1,v2]) <$> fromSExpr l <*> fromSExpr r
    parsePrimApp3 b e1 e2 e3 =
      (\v1 v2 v3 -> PrimApp b [v1,v2,v3]) <$> fromSExpr e1 <*> fromSExpr e2 <*> fromSExpr e3

-- e ::= (let ([x d]) e) | (if v e e) | d
instance FromSExpr E where
  fromSExpr = f where
    f (List [AtomSym "let", List [List [arg, d]], e]) = Let <$> fromSExpr arg <*> fromSExpr d <*> f e
    f (List [AtomSym "if", v, te, fe]) = If <$> fromSExpr v <*> f te <*> f fe
    f d = DE <$> fromSExpr d

-- p ::= (e (l (x ...) e) ...)
instance FromSExpr L3 where
  fromSExpr = f where
    f (List (main : funcs)) = L3 <$> fromSExpr main <*> traverse fromSExpr funcs
    f bad = parseError_ "bad L3-program" bad

instance FreeVars E where
  freeVars = f where
    f (Let x r body)         bv = f (DE r) bv <> f body (Set.insert x bv)
    f (If e a b)             bv = f (ve e) bv <> f a bv <> f b bv
    f (DE (VD (VarV v)))     bv = if Set.member v bv then mempty else Set.singleton v
    f (DE (VD _))            _  = Set.empty
    f (DE (MakeClosure _ v)) bv = f (ve v) bv
    f (DE (ClosureProc v))   bv = f (ve v) bv
    f (DE (ClosureVars v))   bv = f (ve v) bv
    f (DE (App v vs))        bv = mconcat $ f (ve v) bv : fmap (flip f bv) (ve <$> vs)
    f (DE (NewTuple vs))     bv = mconcat $ fmap (flip f bv) (ve <$> vs)
    f (DE (PrimApp _ vs))    bv = mconcat $ fmap (flip f bv) (ve <$> vs)
    ve = DE . VD

foldV :: (Variable -> a) -> (Int64 -> a) -> (Label -> a) -> V -> a
foldV f _ _ (VarV v)   = f v
foldV _ f _ (NumV v)   = f v
foldV _ _ f (LabelV v) = f v
