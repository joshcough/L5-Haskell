module L.L3.L3AST where

import L.Read (showAsList)

type Label    = String
data Program  = Program E [Func]
type Variable = String
data V        = VarV Variable | NumV Int | LabelV Label
data Func     = Func { name :: Label, args :: [Variable], body :: E }
data E        = Let Variable D E | IfStatement V E E | DE D
data Biop     = Add V V | Sub V V | Mult V V | LessThan V V | LTorEq V V | Eq V V
data Pred     = IsNum V | IsArray V
data D        =
    BiopD Biop
  | PredD Pred
  | FunCall V [V]
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
  show (Add      l r) = showAsList ["+",  show l, show r]
  show (Sub      l r) = showAsList ["-",  show l, show r]
  show (Mult     l r) = showAsList ["*",  show l, show r]
  show (LessThan l r) = showAsList ["<",  show l, show r]
  show (LTorEq   l r) = showAsList ["<=", show l, show r]
  show (Eq       l r) = showAsList ["=",  show l, show r]

instance Show Pred where
  show (IsNum   v) = showAsList ["number?", show v]
  show (IsArray v) = showAsList ["a?",      show v]

instance Show D where
  show (BiopD b)         = show b
  show (PredD p)         = show p
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
  show (Let v d e)           = showAsList ["let", concat ["[", v, " ", show d, "]"], show e]
  show (IfStatement v te fe) = showAsList ["if", show v, show te, show fe]
  show (DE d)                = show d

instance Show Func where show (Func n a l) = showAsList [n, showAsList a, show l]
instance Show Program where show (Program e fs) = showAsList (show e : fmap show fs)

{-
  case class Num(n: Int) extends V { def *(i:Int) = Num(n*i); def +(i:Int) = Num(n+i) }
  case class Label(name: String) extends V { override def toString = "Label(\"" + name + "\")" }
-}
