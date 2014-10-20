module L.L5.L5AST where

import L.L1L2AST (Variable)

type L5 = E

data PrimFun = 
    Add      E E 
  | Sub      E E 
  | Mult     E E 
  | LessThan E E
  | LTorEQ E E
  | EqualTo  E E
  | IsNumber E 
  | IsArray  E
  | Print    E
  | NewArray E E
  | ARef     E E
  | ASet     E E E
  | ALen     E
  deriving Eq

data E = 
    Lambda [Variable] E
  | Var Variable
  | Let [(Variable, E)] E
  | LetRec [(Variable, E)] E
  | IfStatement E E E
  | NewTuple [E]
  | Begin E E
  | App E [E]
  | LitInt Int
  | PrimFunE PrimFun
  deriving Eq

instance Show E where
  show e = "todo"
