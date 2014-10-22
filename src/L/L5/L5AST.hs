module L.L5.L5AST where

import L.L1L2AST (Variable)
import L.L3.L3AST (Prim(..), PrimName(..), arityByName)

type L5 = E
type Arity = Int

primVars :: PrimName -> [Variable]
primVars p = take (arityByName p) ["x", "y", "z"]

data E =
    Lambda [Variable] E
  | Var Variable
  | Let Variable E E
  | LetRec Variable E E
  | IfStatement E E E
  | NewTuple [E]
  | Begin E E
  | App E [E]
  | LitInt Int
  | PrimE Prim
  deriving Eq

instance Show E where
  show e = "todo"
