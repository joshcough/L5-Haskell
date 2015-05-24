module L.L4.L4AST where

import Control.Applicative
import Control.Monad
import Control.Monad.State
import Data.Monoid
import Data.Traversable hiding (sequence)
import L.L3.L3AST (V(..))
import L.Primitives
import L.Parser.SExpr
import L.Parser.Supply
import L.Variable hiding (freeVars)

type L4Func = Func E
data L4   = L4 E [L4Func]
data E    =
    Let Variable E E
  | If E E E
  | App E [E]
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
  asSExpr ex = fst $ runState (go ex) (newSupply mempty) where
    go :: E -> State Supply SExpr
    go (Let v e b)       = do
      e' <- go e
      b' <- go b
      return $ asSExpr (sym "let", [(v, e')], b')
    go (If v te fe)      = do
      (v', te', fe') <- (,,) <$> go v <*> go te <*> go fe
      return $ asSExpr (sym "if", v', te', fe')
    go (App   e  es)     = do
      e'       <- go e
      List es' <- asSExpr <$> traverse go es
      return $ asSExpr (e' : es')
    go (PrimApp p es)    = do
      List es' <- asSExpr <$> traverse go es
      return $ asSExpr (asSExpr p : es')
    go (NewTuple es)     = do
      List es' <- asSExpr <$> traverse go es
      return $ List (sym "new-tuple" : es')
    go (Begin e1 e2)     = do
      (e1', e2') <- (,) <$> go e1 <*> go e2
      return $ asSExpr (sym "begin", e1', e2')
    go (MakeClosure l e) = go e >>= \e' -> return $ asSExpr (sym "make-closure", asSExpr l, e')
    go (ClosureProc e)   = go e >>= \e' -> return $ asSExpr (sym "closure-proc", e')
    go (ClosureVars e)   = go e >>= \e' -> return $ asSExpr (sym "closure-vars", e')
    go (VE v)            = return . asSExpr $ v

instance AsSExpr L4 where
  asSExpr (L4 e fs) = List $ asSExpr e : fmap asSExpr fs

-- p ::= (e (l (x ...) e) ...)
instance FromSExpr L4 where
  fromSExpr (List (main : funcs)) = liftM2 L4 (fromSExpr main) (traverse fromSExpr funcs)
  fromSExpr bad = Left $ concat ["Bad L4 Program: ", showSExpr bad]

instance FromSExpr E where
  fromSExpr = f where
    f (List [AtomSym "let", List [List [arg, e]], b]) = liftM3 Let (fromSExpr arg) (f e) (f b)
    f (List [AtomSym "if", pe, te, fe])     = liftM3 If (f pe) (f te) (f fe)
    f (List (AtomSym "new-tuple" : es))     = liftM  NewTuple (traverse f es)
    f (List [AtomSym "begin", e1, e2])      = liftM2 Begin (f e1) (f e2)
    f (List [AtomSym "make-closure", l, e]) = liftM2 MakeClosure (fromSExpr l) (f e)
    f (List [AtomSym "closure-proc", e])    = liftM  ClosureProc (f e)
    f (List [AtomSym "closure-vars", e])    = liftM  ClosureVars (f e)
    -- TODO: this is bad because if they give the wrong number of args to a prim,
    -- TODO: it'll parse an App
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
    -- TODO: check for all keywords here
    f hmm@(List (AtomSym "let" : _))      = error $ "got a let here: " ++ showSExpr hmm
    f (List (e : es))                       = liftM2 App (f e) (traverse f es)
    f v                                     = VE <$> fromSExpr v

    parsePrimApp1 p e = liftM (PrimApp p . return) (fromSExpr e)
    parsePrimApp2 b l r = liftM2 (\v1 v2 -> PrimApp b [v1,v2]) (fromSExpr l) (fromSExpr r)
    parsePrimApp3 b e1 e2 e3 =
      liftM3 (\v1 v2 v3 -> PrimApp b [v1,v2,v3]) (fromSExpr e1) (fromSExpr e2) (fromSExpr e3)

