module L.L3.L3AST where

import Data.Int
import L.L1L2AST (Variable, Label)
import L.Read (showAsList)

data L3   = L3 E [Func]
data Func = Func { name :: Label, args :: [Variable], body :: E }
data V    = VarV Variable | NumV Int64 | LabelV Label
data E    = Let Variable D E | IfStatement V E E | DE D
data Biop = Add | Sub | Mult | LessThan | LTorEq | Eq
data Pred = IsNum | IsArray 
data D    =
    FunCall V [V]
  | BiopD Biop V V
  | PredD Pred V
  | NewArray V V
  | NewTuple [V]
  | ARef V V
  | ASet V V V
  | ALen V
  | Print V
  | MakeClosure Label V
  | ClosureProc V
  | ClosureVars V
  | VD V

instance Show V where
  show (VarV v)   = v
  show (NumV n)   = show n
  show (LabelV l) = l

instance Show Biop where
  show Add      = "+"
  show Sub      = "-"
  show Mult     = "*"
  show LessThan = "<"
  show LTorEq   = "<="
  show Eq       = "="

instance Show Pred where
  show IsNum    = "number?"
  show IsArray  = "a?"

instance Show D where
  show (BiopD b l r)     = showAsList [show b,  show l, show r]
  show (PredD p v)       = showAsList [show p,  show v]
  show (FunCall v vs)    = showAsList (show v : fmap show vs)
  show (NewArray s v)    = showAsList ["new-array", show s, show v]
  show (NewTuple vs)     = showAsList ("new-tuple" : fmap show vs)
  show (ARef a loc)      = showAsList ["aref",  show a, show loc]
  show (ASet a loc v)    = showAsList ["aset",  show a, show loc, show v]
  show (ALen a)          = showAsList ["aref",  show a]
  show (Print v)         = showAsList ["print", show v]
  show (MakeClosure l v) = showAsList ["make-closure", l, show v]
  show (ClosureProc v)   = showAsList ["closure-proc", show v]
  show (ClosureVars v)   = showAsList ["closure-vars", show v]
  show (VD v)            = show v

instance Show E where
  show (Let v d e)           = showAsList ["let", concat ["([", v, " ", show d, "])"], show e]
  show (IfStatement v te fe) = showAsList ["if", show v, show te, show fe]
  show (DE d)                = show d

instance Show Func where show (Func n a l) = showAsList [n, showAsList a, show l]
instance Show L3 where show (L3 e fs) = showAsList (show e : fmap show fs)

