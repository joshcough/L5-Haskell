module L.L4.L4AST where

import Control.Applicative
import Data.Int
import Data.List (intersperse)
import L.Read (showAsList)
import L.L1L2AST (Variable, Label)

data L4   = L4 E [Func]
data Func = Func { name :: Label, args :: [Variable], body :: E }
data V    = VarV Variable | NumV Int64 | LabelV Label
data Biop = Add E E | Sub E E | Mult E E | LessThan E E | LTorEq E E | Eq E E
data Pred = IsNum E | IsArray E
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
  | BiopE Biop
  | PredE Pred
  | VE V

instance Show V where
  show (VarV v)   = v
  show (NumV n)   = show n
  show (LabelV l) = l

instance Show Biop where
  show (Add      l r) = showAsList ["+",  show l, show r]
  show (Sub      l r) = showAsList ["-",  show l, show r]
  show (Mult     l r) = showAsList ["*",  show l, show r]
  show (LessThan l r) = showAsList ["<",  show l, show r]
  show (LTorEq   l r) = showAsList ["<=", show l, show r]
  show (Eq       l r) = showAsList ["=",  show l, show r]

instance Show Pred where
  show (IsNum   e) = showAsList ["number?", show e]
  show (IsArray e) = showAsList ["a?",      show e]

instance Show E where
  show (Let v e b)           = showAsList ["let", concat ["[", v, " ", show e, "]"], show b]
  show (IfStatement v te fe) = showAsList ["if", show v, show te, show fe]
  show (FunCall e es)        = showAsList (show e : fmap show es)
  show (NewArray s e)        = showAsList ["new-array", show s, show e]
  show (NewTuple es)         = showAsList ("new-tuple" : fmap show es)
  show (ARef a loc)          = showAsList ["aref",  show a, show loc]
  show (ASet a loc e)        = showAsList ["aset",  show a, show loc, show e]
  show (ALen a)              = showAsList ["aref",  show a]
  show (Print e)             = showAsList ["print", show e]
  show (MakeClosure l e)     = showAsList ["make-closure", l, show e]
  show (ClosureProc e)       = showAsList ["closure-proc", show e]
  show (ClosureVars e)       = showAsList ["closure-ears", show e]
  show (BiopE b)             = show b
  show (PredE p)             = show p
  show (VE v)                = show v

instance Show Func where show (Func n a l) = showAsList [n, showAsList a, show l]
instance Show L4 where show (L4 e fs) = showAsList (show e : fmap show fs)
