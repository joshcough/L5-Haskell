module L.L4.L4AST where

import Control.Applicative
import Control.Monad
import Data.Traversable hiding (sequence)
import L.Read
import L.L1L2AST (Label)
import L.L3.L3AST (PrimName(..), V, Func(..))
import L.Variable

type L4Func = Func E
data L4   = L4 E [L4Func]
data E    =
    Let Variable E E 
  | IfStatement E E E
  | FunCall E [E]
  | PrimApp PrimName [E]
  | NewTuple [E]
  | Begin E E
  | MakeClosure Label E
  | ClosureProc E
  | ClosureVars E
  | VE V

instance Show E  where show = showSExpr
instance Show L4 where show = showSExpr

instance AsSExpr E where
  asSExpr (Let v e b)           = asSExpr (sym "let", (asSExpr v, e), b)
  asSExpr (IfStatement v te fe) = asSExpr (sym "if", v, te, fe)
  asSExpr (Begin e1 e2)         = asSExpr (sym "begin", e1, e2)
  asSExpr (NewTuple es)         = List    (sym "new-tuple" : fmap asSExpr es)
  asSExpr (MakeClosure l e)     = asSExpr (sym "make-closure", asSExpr l, e)
  asSExpr (ClosureProc e)       = asSExpr (sym "closure-proc", e)
  asSExpr (ClosureVars e)       = asSExpr (sym "closure-vars", e)
  asSExpr (FunCall e es)        = asSExpr (asSExpr e : fmap asSExpr es)
  asSExpr (PrimApp p es)        = asSExpr (asSExpr p : fmap asSExpr es)
  asSExpr (VE v)                = asSExpr v

instance AsSExpr L4 where
  asSExpr (L4 e fs) = List $ asSExpr e : fmap asSExpr fs

-- p ::= (e (l (x ...) e) ...)
instance FromSExpr L4 where
  fromSExpr (List (main : funcs)) = liftM2 L4 (fromSExpr main) (traverse fromSExpr funcs)
  fromSExpr bad = Left $ concat ["Bad L4 Program: ", show bad]

instance FromSExpr E where
  fromSExpr = f where
    f (List [AtomSym "let", List [List [arg, e]], b]) = liftM3 Let (fromSExpr arg) (f e) (f b)
    f (List [AtomSym "if", pe, te, fe])     = liftM3 IfStatement (f pe) (f te) (f fe)
    f (List (AtomSym "new-tuple" : es))     = liftM  NewTuple (traverse f es)
    f (List [AtomSym "begin", e1, e2])      = liftM2 Begin (f e1) (f e2)
    f (List [AtomSym "make-closure", l, e]) = liftM2 MakeClosure (fromSExpr l) (f e)
    f (List [AtomSym "closure-proc", e])    = liftM  ClosureProc (f e)
    f (List [AtomSym "closure-vars", e])    = liftM  ClosureVars (f e)
    -- TODO: this is bad because if they give the wrong number of args to a prim,
    -- TODO: it'll parse a FunCall
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
    f (List (e : es)) = liftM2 FunCall (f e) (traverse f es)
    f v = VE <$> fromSExpr v

    parsePrimApp1 p e = liftM (PrimApp p . return) (fromSExpr e)
    parsePrimApp2 b l r = liftM2 (\v1 v2 -> PrimApp b [v1,v2]) (fromSExpr l) (fromSExpr r)
    parsePrimApp3 b e1 e2 e3 =
      liftM3 (\v1 v2 v3 -> PrimApp b [v1,v2,v3]) (fromSExpr e1) (fromSExpr e2) (fromSExpr e3)

