module L.L3.L3AST where

import Control.Applicative
import Control.Monad
import Data.Int
import Data.Map (Map)
import Data.Maybe (fromJust)
import qualified Data.Map as Map
import Data.Traversable hiding (sequence)
import L.L1L2AST (Variable(..), Label(..))
import L.Read
import Prelude hiding (print)

data Func a = Func { name :: Label, args :: [Variable], body :: a }

type L3Func = Func E
data L3   = L3 E [L3Func]
data V    = VarV Variable | NumV Int64 | LabelV Label
data E    = Let Variable D E | IfStatement V E E | DE D
data D    = FunCall V [V] | PrimApp PrimName [V] | VD V |
            NewTuple  [V] | MakeClosure Label V  | ClosureProc V | ClosureVars V

data PrimName =
    Add | Sub | Mult | LessThan | LTorEQ   | EqualTo
  | IsNumber  | IsArray | Print | NewArray | ARef | ASet | ALen
  deriving (Eq,Ord)

instance (AsSExpr a, Show a) => Show (Func a) where show = showSExpr
instance Show V        where show = showSExpr
instance Show D        where show = showSExpr
instance Show E        where show = showSExpr
instance Show L3       where show = showSExpr
instance Show PrimName where show = primText . primByName

instance AsSExpr PrimName where
  asSExpr = AtomSym . primText . primByName

type Arity = Int
data Prim = Prim {
  primName :: PrimName,
  primText :: String,
  primArity :: Arity
} deriving Eq

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

foldV :: (Variable -> a) -> (Int64 -> a) -> (Label -> a) -> V -> a
foldV f _ _ (VarV v)   = f v
foldV _ f _ (NumV v)   = f v
foldV _ _ f (LabelV v) = f v

isBiop :: PrimName -> Bool
isBiop p = p `elem` [Add, Sub, Mult, LessThan, LTorEQ, EqualTo]

isPredicate :: PrimName -> Bool
isPredicate p = p `elem` [IsNumber, IsArray]

instance AsSExpr D where
  asSExpr (FunCall v vs)    = asSExpr (asSExpr v : fmap asSExpr vs)
  asSExpr (PrimApp p vs)    = asSExpr (asSExpr p : fmap asSExpr vs)
  asSExpr (VD v)            = asSExpr v
  asSExpr (NewTuple vs)     = asSExpr (sym "new-tuple" : fmap asSExpr vs)
  asSExpr (MakeClosure l v) = asSExpr (sym "make-closure", l, v)
  asSExpr (ClosureProc v)   = asSExpr (sym "closure-proc", v)
  asSExpr (ClosureVars v)   = asSExpr (sym "closure-vars", v)

instance AsSExpr E where
  asSExpr (Let v d e)           = asSExpr (sym "let", asSExpr [(v, d)], e)
  asSExpr (IfStatement v te fe) = asSExpr (sym "if", v, te, fe)
  asSExpr (DE d)                = asSExpr d

instance AsSExpr L3 where
  asSExpr (L3 e fs) = List $ asSExpr e : fmap asSExpr fs

-- v :: = x | l | num
instance FromSExpr V where
  fromSExpr (AtomSym l@(':' : _)) = Right . LabelV $ Label l
  fromSExpr (AtomSym v) = Right . VarV $ Variable v
  fromSExpr (AtomNum n) = Right $ NumV (fromIntegral n)
  fromSExpr bad         = Left  $ concat ["Parse Error: 'bad V' in: ", show bad]

instance AsSExpr V where
  asSExpr (VarV (Variable v)) = AtomSym v
  asSExpr (NumV n)            = AtomNum n
  asSExpr (LabelV (Label l))  = AtomSym l

instance AsSExpr a => AsSExpr (Func a) where
  asSExpr (Func n args a) = asSExpr (n, args, a)

{-
d ::= (biop v v) | (pred v) | (v v ...) | (new-array v v) | (new-tuple v ...)
      (aref v v) | (aset v v v) | (alen v)
      (print v) | (make-closure l v) | (closure-proc v) | (closure-vars v)
      v
biop ::= + | - | * | < | <= | =
pred ::= number? | a?
 -}
instance FromSExpr D where
  fromSExpr = f where
    f (List [AtomSym "make-closure", l, v]) = liftM2 MakeClosure (fromSExpr l) (fromSExpr v)
    f (List [AtomSym "closure-proc", v])    = liftM  ClosureProc (fromSExpr v)
    f (List [AtomSym "closure-vars", v])    = liftM  ClosureVars (fromSExpr v)
    -- TODO: this is bad because if they give the wrong number of args to a prim,
    -- TODO: it'll parse a FunCall
    f (List [AtomSym "+",    l, r])         = parsePrimApp2 Add      l r
    f (List [AtomSym "-",    l, r])         = parsePrimApp2 Sub      l r
    f (List [AtomSym "*",    l, r])         = parsePrimApp2 Mult     l r
    f (List [AtomSym "<",    l, r])         = parsePrimApp2 LessThan l r
    f (List [AtomSym "<=",   l, r])         = parsePrimApp2 LTorEQ   l r
    f (List [AtomSym "=",    l, r])         = parsePrimApp2 EqualTo  l r
    f (List [AtomSym "number?", v])         = parsePrimApp1 IsNumber v
    f (List [AtomSym "a?",      v])         = parsePrimApp1 IsArray  v
    f (List [AtomSym "new-array", s, v])    = parsePrimApp2 NewArray s v
    f (List (AtomSym "new-tuple" : vs))     = liftM         NewTuple (traverse fromSExpr vs)
    f (List [AtomSym "aref", a, loc])       = parsePrimApp2 ARef a loc
    f (List [AtomSym "aset", a, loc, v])    = parsePrimApp3 ASet a loc v
    f (List [AtomSym "alen", v])            = parsePrimApp1 ALen  v
    f (List [AtomSym "print", v])           = parsePrimApp1 Print v
    f (List (v : vs))                       = liftM2        FunCall (fromSExpr v) (traverse fromSExpr vs)
    f v = VD <$> fromSExpr v

    parsePrimApp1 p v   = liftM (PrimApp p . return) (fromSExpr v)
    parsePrimApp2 b l r = liftM2 (\v1 v2 -> PrimApp b [v1,v2]) (fromSExpr l) (fromSExpr r)
    parsePrimApp3 b e1 e2 e3 =
      liftM3 (\v1 v2 v3 -> PrimApp b [v1,v2,v3]) (fromSExpr e1) (fromSExpr e2) (fromSExpr e3)

-- e ::= (let ([x d]) e) | (if v e e) | d
instance FromSExpr E where
  fromSExpr = f where
    f (List [AtomSym "let", List [List [arg, d]], e]) = liftM3 Let (fromSExpr arg) (fromSExpr d) (f e)
    f (List [AtomSym "if", v, te, fe]) = liftM3 IfStatement (fromSExpr v) (f te) (f fe)
    f d = DE <$> fromSExpr d

parseError :: String -> SExpr -> Either String a
parseError msg exp = Left $ concat ["Parse Error: '", msg, "' in: ", show exp]

-- p ::= (e (l (x ...) e) ...)
instance FromSExpr L3 where
  fromSExpr = f where
    f (List (main : funcs)) = liftM2 L3 (fromSExpr main) (traverse fromSExpr funcs)
    f bad = parseError "bad L3-program" bad

-- (l (x ...) e)
instance FromSExpr a => FromSExpr (Func a) where
  fromSExpr (List [l, args, e]) = liftM3 Func (fromSExpr l) (fromSExpr args) (fromSExpr e)
  fromSExpr bad = parseError "bad function" bad
