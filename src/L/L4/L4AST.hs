module L.L4.L4AST where

import L.Read (showAsList)
import L.L1L2AST (Variable, Label)
import L.L3.L3AST (PrimName, V)

data L4   = L4 E [Func]
data Func = Func { name :: Label, args :: [Variable], body :: E }
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
  show (Let v e b)           = showAsList ["let", concat ["[", v, " ", show e, "]"], show b]
  show (IfStatement v te fe) = showAsList ["if", show v, show te, show fe]
  show (Begin e1 e2)         = showAsList ["begin", show e1, show e2]
  show (NewTuple es)         = showAsList ("new-tuple" : fmap show es)
  show (MakeClosure l e)     = showAsList ["make-closure", l, show e]
  show (ClosureProc e)       = showAsList ["closure-proc", show e]
  show (ClosureVars e)       = showAsList ["closure-ears", show e]
  show (FunCall e es)        = showAsList (show e : fmap show es)
  show (VE v)                = show v
  show (PrimApp p es)        = showAsList (show p : fmap show es)

instance Show Func where show (Func n a l) = showAsList [n, showAsList a, show l]
instance Show L4 where show (L4 e fs) = showAsList (show e : fmap show fs)

