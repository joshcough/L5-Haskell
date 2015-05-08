module L.Primitives where

import Control.Applicative
import Control.Monad
import Data.Int
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (fromJust)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Traversable hiding (sequence)
import L.L1L2AST (Label(..))
import L.Read
import L.Variable
import Prelude hiding (print)

data Func a = Func { name :: Label, args :: [Variable], body :: a }

data V = VarV Variable | NumV Int64 | LabelV Label

foldV :: (Variable -> a) -> (Int64 -> a) -> (Label -> a) -> V -> a
foldV f _ _ (VarV v)   = f v
foldV _ f _ (NumV v)   = f v
foldV _ _ f (LabelV v) = f v

type Arity = Int
data Prim = Prim {
  primName  :: PrimName,
  primText  :: String,
  primArity :: Arity
} deriving Eq

data PrimName =
    Add | Sub | Mult | LessThan | LTorEQ   | EqualTo
  | IsNumber  | IsArray | Print | NewArray | ARef | ASet | ALen
  deriving (Eq,Ord,Read)

instance Show PrimName where show = primText . primByName

instance AsSExpr PrimName where
  asSExpr = AtomSym . primText . primByName

class PrimLookup a where
  lookupPrim :: a -> Maybe Prim

instance PrimLookup Variable where
  lookupPrim (Variable v) = primByRawNameMaybe v

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
primsByRawName :: Map String Prim
primsByRawName = Map.fromList ((\p@(Prim n _ _) -> (show n, p)) <$> prims)
primByRawNameMaybe :: String -> Maybe Prim
primByRawNameMaybe n = Map.lookup n primsByRawName
primByNameMaybe :: PrimName -> Maybe Prim
primByNameMaybe n = Map.lookup n primsByName
primByName :: PrimName -> Prim
primByName = fromJust . primByNameMaybe
arityByName :: PrimName -> Int
arityByName = f . primByName where f (Prim _ _ a) = a

isBiop :: PrimName -> Bool
isBiop p = p `elem` [Add, Sub, Mult, LessThan, LTorEQ, EqualTo]

isPredicate :: PrimName -> Bool
isPredicate p = p `elem` [IsNumber, IsArray]