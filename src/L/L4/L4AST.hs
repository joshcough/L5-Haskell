module L.L4.L4AST where

import L.Read (showAsList)
import L.L1L2AST (Variable, Label)
import L.L3.L3AST (Biop, Pred, V)

data L4   = L4 E [Func]
data Func = Func { name :: Label, args :: [Variable], body :: E }
data E    =
    Let Variable E E 
  | IfStatement E E E
  | FunCall E [E]
  | NewArray E E
  | NewTuple [E]
  | ARef E E
  | ASet E E E
  | ALen E
  | Begin E E
  | Print E
  | MakeClosure Label E
  | ClosureProc E
  | ClosureVars E
  | BiopE Biop E E
  | PredE Pred E
  | VE V

instance Show E where
  show (Let v e b)           = showAsList ["let", concat ["[", v, " ", show e, "]"], show b]
  show (IfStatement v te fe) = showAsList ["if", show v, show te, show fe]
  show (FunCall e es)        = showAsList (show e : fmap show es)
  show (NewArray s e)        = showAsList ["new-array", show s, show e]
  show (NewTuple es)         = showAsList ("new-tuple" : fmap show es)
  show (ARef a loc)          = showAsList ["aref",  show a, show loc]
  show (ASet a loc e)        = showAsList ["aset",  show a, show loc, show e]
  show (ALen a)              = showAsList ["aref",  show a]
  show (Begin e1 e2)         = showAsList ["begin", show e1, show e2]
  show (Print e)             = showAsList ["print", show e]
  show (MakeClosure l e)     = showAsList ["make-closure", l, show e]
  show (ClosureProc e)       = showAsList ["closure-proc", show e]
  show (ClosureVars e)       = showAsList ["closure-ears", show e]
  show (BiopE b l r)         = showAsList [show b,  show l, show r]
  show (PredE p e)           = showAsList [show p,  show e]
  show (VE v)                = show v

instance Show Func where show (Func n a l) = showAsList [n, showAsList a, show l]
instance Show L4 where show (L4 e fs) = showAsList (show e : fmap show fs)

