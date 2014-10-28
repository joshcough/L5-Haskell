module L.L3.L3AST where

import Control.Applicative
import Data.Int
import Data.Map (Map)
import Data.Maybe (fromJust)
import qualified Data.Map as Map
import L.L1L2AST (Variable, Label)
import L.Read (showAsList)
import Prelude hiding (print)

data L3   = L3 E [Func]
data Func = Func { name :: Label, args :: [Variable], body :: E }
data V    = VarV Variable | NumV Int64 | LabelV Label
data E    = Let Variable D E | IfStatement V E E | DE D
data D    = FunCall V [V] | PrimApp PrimName [V] | VD V |
            NewTuple [V] | MakeClosure Label V | ClosureProc V | ClosureVars V

data PrimName =
    Add | Sub | Mult | LessThan | LTorEQ   | EqualTo
  | IsNumber  | IsArray | Print | NewArray | ARef | ASet | ALen
  deriving (Eq,Ord)

instance Show PrimName where
  show Add      = "+"
  show Sub      = "-"
  show Mult     = "*"
  show LessThan = "<"
  show LTorEQ   = "<="
  show EqualTo  = "="
  show IsNumber = "number?"
  show IsArray  = "a?"
  show Print    = "print"
  show NewArray = "new-array"
  show ARef     = "aref"
  show ASet     = "aset"
  show ALen     = "alen"

type Arity = Int
data Prim = Prim PrimName String Arity deriving Eq
add, sub, mult, lt, eq, lteq, isNum, isArr, print, newArr, aref, aset, alen :: Prim
add    = Prim Add      "+"         2
sub    = Prim Sub      "-"         2
mult   = Prim Mult     "*"         2
lt     = Prim LessThan "<"         2
eq     = Prim LTorEQ   "<="        2
lteq   = Prim EqualTo  "="         2
isNum  = Prim IsNumber "number?"   1
isArr  = Prim IsArray  "a?"        1
print  = Prim Print    "print"     1
newArr = Prim NewArray "new-array" 2
aref   = Prim ARef     "aref"      2
aset   = Prim ASet     "aset"      3
alen   = Prim ALen     "alen"      1

prims :: [Prim]
prims = [add, sub, mult, lt, eq, lteq, isNum, isArr, print, newArr, aref, aset, alen]
primsByName :: Map PrimName Prim
primsByName = Map.fromList ((\p@(Prim n _ _) -> (n, p)) <$> prims)
primByName :: PrimName -> Prim
primByName n = fromJust $ Map.lookup n primsByName
arityByName :: PrimName -> Int
arityByName = f . primByName where f (Prim _ _ a) = a

foldV :: (Variable -> a) -> (Int64 -> a) -> (Label -> a) -> V -> a
foldV f _ _ (VarV v)   = f v
foldV _ f _ (NumV v)   = f v
foldV _ _ f (LabelV v) = f v

isBiop :: PrimName -> Bool
isBiop p = p `elem` [Add, Sub, Mult, LessThan, LTorEQ, EqualTo]

isPredicate :: PrimName -> Bool
isPredicate p = p `elem` [IsNumber, IsArray]

instance Show V where
  show (VarV v)   = v
  show (NumV n)   = show n
  show (LabelV l) = l

instance Show Prim where
  show (Prim _ name _) = name

instance Show D where
  show (FunCall v vs)    = showAsList (show v : fmap show vs)
  show (PrimApp p vs)    = showAsList (show p : fmap show vs)
  show (VD v)            = show v
  show (NewTuple vs)     = showAsList ("new-tuple" : fmap show vs)
  show (MakeClosure l v) = showAsList ["make-closure", l, show v]
  show (ClosureProc v)   = showAsList ["closure-proc", show v]
  show (ClosureVars v)   = showAsList ["closure-vars", show v]

instance Show E where
  show (Let v d e)           = showAsList ["let", concat ["([", v, " ", show d, "])"], show e]
  show (IfStatement v te fe) = showAsList ["if", show v, show te, show fe]
  show (DE d)                = show d

instance Show Func where show (Func n a l) = showAsList [n, showAsList a, show l]
instance Show L3 where show (L3 e fs) = showAsList (show e : fmap show fs)

