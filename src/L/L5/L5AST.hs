module L.L5.L5AST where

import Control.Applicative
import Control.Monad.State
import Control.Monad.Trans
import Data.Foldable
import Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.Sequence as Seq
import Data.Traversable

type Variable = String

data PrimFun = 
    Add      E E 
  | Sub      E E 
  | Mult     E E 
  | LessThan E E
  | LessThanOrEqualTo E E 
  | EqualTo  E E
  | IsNumber E 
  | IsArray  E
  | Print    E
  | NewArray E E
  | ARef     E E
  | ASet     E E E
  | ALen     E

data E = 
    Lambda [Variable] E
  | Var Variable
  | Let [(Variable, E)] E
  | LetRec Variable E E
  | IfStatement E E E
  | NewTuple [E]
  | Begin E E
  | App E [E]
  | LitInt Int
  | PrimFunE PrimFun
