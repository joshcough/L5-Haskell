module L.L4.L4AST where

import Control.Applicative
import Control.Monad
import Data.Traversable hiding (sequence)
import L.Read
import L.L1L2AST (Variable, Label)
import L.L3.L3AST (PrimName(..), V, Func(..))

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

instance Show E where
  show (Let v e b)           = showAsList ["let", concat ["[", show v, " ", show e, "]"], show b]
  show (IfStatement v te fe) = showAsList ["if", show v, show te, show fe]
  show (Begin e1 e2)         = showAsList ["begin", show e1, show e2]
  show (NewTuple es)         = showAsList ("new-tuple" : fmap show es)
  show (MakeClosure l e)     = showAsList ["make-closure", show l, show e]
  show (ClosureProc e)       = showAsList ["closure-proc", show e]
  show (ClosureVars e)       = showAsList ["closure-vars", show e]
  show (FunCall e es)        = showAsList (show e : fmap show es)
  show (VE v)                = show v
  show (PrimApp p es)        = showAsList (show p : fmap show es)

instance Show L4 where show (L4 e fs) = showAsList (show e : fmap show fs)

l4ParseError :: String -> SExpr -> Either String a
l4ParseError msg exp = Left $ concat ["L4 Parse Error: '", msg, "' in: ", show exp]

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

