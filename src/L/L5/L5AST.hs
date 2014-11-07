module L.L5.L5AST where

import Data.Int
import L.L1L2AST (Variable)
import L.L3.L3AST (Prim(..), PrimName(..), arityByName)
import L.Read

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
  | LitInt Int64
  | PrimE PrimName
  deriving Eq

instance Show E where
  show (Lambda vs e)         = showAsList ["lambda", showAsList vs, show e]
  show (Let v e b)           = showAsList ["let",    concat ["[", v, " ", show e, "]"], show b]
  show (LetRec v e b)        = showAsList ["letrec", concat ["[", v, " ", show e, "]"], show b]
  show (IfStatement v te fe) = showAsList ["if", show v, show te, show fe]
  show (NewTuple es)         = showAsList ("new-tuple" : fmap show es)
  show (Begin e1 e2)         = showAsList ["begin", show e1, show e2]
  show (App (PrimE p) es)    = showAsList (show p : fmap show es)
  show (App e es)            = showAsList (show e : fmap show es)
  show (LitInt i)            = show i
  show (PrimE p)             = show p
  show (Var v)               = v
