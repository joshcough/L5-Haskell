{-# LANGUAGE OverloadedStrings #-}

module L.L5.L5Core (toCore) where

import Bound
import Bound.Var
import Bound.Scope
import Data.Text as Strict hiding (cons, foldl, length)
import Ermine.Syntax.Convention
import Ermine.Syntax.Core 
import L.L5.L5AST (E, PrimFun(..))
import qualified L.L5.L5AST as L5

type Out = Core Convention String

{-
data E =
    Lambda [Variable] E
  | Let [(Variable, E)] E
  | LetRec [(Variable, E)] E
  | IfStatement E E E
  | NewTuple [E]
  | Begin E E
  | LitInt Int
  | PrimFunE PrimFun
  deriving Eq
-}

toCore :: E -> Out
toCore   (L5.Var v)        = Var v
toCore   (L5.App f [e])    = App C (toCore f) (toCore e)
toCore a@(L5.App f (e:es)) = toCore $ flatApp a where
  -- | (f a b c d) = ((((f a) b) c) d)
  flatApp (L5.App f (e:es)) = foldl (\e f -> L5.App f [e]) (L5.App f [e]) es
toCore _ = error "todo"

primFunToCore :: PrimFun -> Out
primFunToCore (Add  e1 e2)     = mkprim "+"     [e1, e2]
primFunToCore (Sub  e1 e2)     = mkprim "-"     [e1, e2]
primFunToCore (Mult e1 e2)     = mkprim "*"     [e1, e2]
primFunToCore (LessThan e1 e2) = mkprim "<"     [e1, e2]
primFunToCore (LTorEQ e1 e2)   = mkprim "<="    [e1, e2]
primFunToCore (EqualTo e1 e2)  = mkprim "=="    [e1, e2]
primFunToCore (IsNumber e)     = mkprim "num?"  [e]
primFunToCore (IsArray  e)     = mkprim "arr?"  [e]
primFunToCore (Print    e)     = mkprim "print" [e]
primFunToCore (NewArray e1 e2) = mkprim "arr"   [e1, e2]
primFunToCore (ARef e1 e2)     = mkprim "aref"  [e1, e2]
primFunToCore (ASet e1 e2 e3)  = mkprim "aset"  [e1, e2, e3]
primFunToCore (ALen e)         = mkprim "alen"  [e]

mkprim :: Strict.Text -> [E] -> Out
mkprim name (e:es) = foldl (\e f -> App N f e) inner (fmap toCore es) where
  inner = App N (HardCore $ Foreign $ Unknown name) (toCore e)
