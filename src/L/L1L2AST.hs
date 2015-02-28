{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE LambdaCase #-}

module L.L1L2AST where

import Control.Applicative
import Control.Lens
import Control.Monad
import Data.Int
import Data.Map as Map
import Data.Traversable
import Data.Tuple
import L.Read
import L.Registers
import Prelude hiding (LT, EQ)

newtype Label = Label String deriving (Eq, Ord, Show)

data MemLoc x   = MemLoc x Int64 deriving (Eq, Ord)
data CompOp     = LT | LTEQ | EQ deriving (Eq, Ord)
data Comp s     = Comp s CompOp s deriving (Eq, Ord)
data AssignRHS x s =
  CompRHS (Comp s) | Allocate s s | Print s | ArrayError s s | MemRead (MemLoc x) | SRHS s
    deriving (Eq, Ord)

data X86Op = Increment | Decrement | Multiply | LeftShift | RightShift | BitwiseAnd
  deriving (Eq, Ord)

data Instruction x s =
  Assign x (AssignRHS x s)   |
  MathInst x X86Op s         |
  MemWrite (MemLoc x) s      |
  Goto Label                 |
  CJump (Comp s) Label Label |
  LabelDeclaration Label     |
  Call s                     |
  TailCall s                 |
  Return deriving (Eq, Ord)

makePrisms ''Instruction

data Func x s = Func { body :: [Instruction x s]}
data Program x s = Program (Func x s) [Func x s]

getLabels :: [Instruction x s] -> [Label]
getLabels is = is^..traverse._LabelDeclaration

labelIndices :: [Instruction x s] -> Map Label Int
labelIndices is = m where
  l = swap <$> zip is [0..]^@..folded.swapped.ifolded._LabelDeclaration
  m = Map.fromList l

programToList :: Program x s -> [Instruction x s]
programToList (Program main fs) = Prelude.concat $ fmap body $ main : fs

increment, decrement, multiply, leftShift, rightShift, bitwiseAnd :: X86Op 
increment  = Increment
decrement  = Decrement
multiply   = Multiply
leftShift  = LeftShift
rightShift = RightShift
bitwiseAnd = BitwiseAnd

x86OpSymbol :: X86Op -> String
x86OpSymbol Increment  = "+="
x86OpSymbol Decrement  = "-="
x86OpSymbol Multiply   = "*="
x86OpSymbol LeftShift  = "<<="
x86OpSymbol RightShift = ">>="
x86OpSymbol BitwiseAnd = "&="

x86OpName :: X86Op -> String
x86OpName Increment  = "addq"
x86OpName Decrement  = "subq"
x86OpName Multiply   = "imulq"
x86OpName LeftShift  = "salq"
x86OpName RightShift = "sarq"
x86OpName BitwiseAnd = "andq"

instance (Show x, Show s) => Show (Program x s) where
  show (Program main fs) = unlines ["(", show main, fs >>= show, ")"]

instance (Show x, Show s) => Show (Func x s) where
  show (Func is) = "(" ++ (is >>= (\i -> show i ++ "\n\t")) ++ ")"

instance (Show x, Show s) => Show (Instruction x s) where
  show (Assign x rhs)       = showAsList [show x, "<-", show rhs]
  show (MathInst x op s)    = showAsList [show x, x86OpSymbol op, show s]
  show (MemWrite loc s)     = showAsList [show loc, "<-", show s]
  show (Goto l)             = showAsList ["goto", show l]
  show (CJump cmp l1 l2)    = showAsList ["cjump", show cmp, show l1, show l2]
  show (LabelDeclaration l) = show l
  show (Call s)             = showAsList ["call", show s]
  show (TailCall s)         = showAsList ["tail-call", show s]
  show Return               = "(return)"

instance (Show x, Show s) => Show (AssignRHS x s) where
  show (CompRHS c)        = show c
  show (Allocate s1 s2)   = showAsList ["allocate", show s1, show s2]
  show (Print s)          = showAsList ["print", show s]
  show (ArrayError s1 s2) = showAsList ["array-error", show s1, show s2]
  show (SRHS s)           = show s
  show (MemRead loc)      = show loc

instance (Show x) => Show (MemLoc x) where
  show (MemLoc x n) = showAsList ["mem", show x, show n]

instance (Show s) => Show (Comp s) where
  show (Comp s1 op s2) = concat [show s1, " ", show op, " ", show s2]

instance Show CompOp where
  show LT   = "<"
  show LTEQ = "<="
  show EQ   = "="

cmp :: Ord a => CompOp -> a -> a -> Bool
cmp LT   = (<)
cmp LTEQ = (<=)
cmp EQ   = (==)

compOpFromSym :: String -> Either String CompOp
compOpFromSym "<"  = Right LT
compOpFromSym "<=" = Right LTEQ
compOpFromSym "="  = Right EQ
compOpFromSym s    = Left $ "not a comparison operator" ++ s

foldOp :: a -> a -> a -> CompOp -> a
foldOp a _ _ LT   = a
foldOp _ a _ LTEQ = a
foldOp _ _ a EQ   = a

-- L1 AST (uses shared L1/L2 AST)
type L1X = Register
data L1S = NumberL1S Int64 | LabelL1S Label | RegL1S Register deriving (Eq, Ord)
type L1Instruction = Instruction L1X L1S
type L1Func = Func L1X L1S
newtype L1 = L1 (Program L1X L1S)

instance Show L1
  where show (L1 p) = show p

instance Show L1S where
  show (NumberL1S n) = show n
  show (LabelL1S l)  = show l
  show (RegL1S r)    = show r

-- L2 AST (uses shared L1/L2 AST)
-- L2 adds variables to X and S. that's the only difference between L2 and L1.
newtype Variable = Variable String deriving (Eq, Ord, Show)

instance AsSExpr Variable where
  asSExpr (Variable v) = AtomSym v

instance FromSExpr Variable where
  fromSExpr (AtomSym s) = Right $ Variable s
  fromSExpr bad = Left $ "invalid variable name: " ++ show bad

class AsVariable t where
  _Variable :: Prism' t Variable

instance AsVariable Variable where
  _Variable = id

data L2X = RegL2X Register | VarL2X Variable

instance AsRegister L2X where
  _Register = prism RegL2X $ \case RegL2X x -> Right x; r -> Left r

instance AsVariable L2X where
  _Variable = prism VarL2X $ \case VarL2X x -> Right x; r -> Left r

data L2S = XL2S L2X | NumberL2S Int64 | LabelL2S Label deriving (Eq, Ord)
type L2MemLoc = MemLoc L2X
type L2Instruction = Instruction L2X L2S
type L2Func = Func L2X L2S
newtype L2 = L2 (Program L2X L2S)

instance Show L2
  where show (L2 p) = show p

instance Show L2X where
  show (RegL2X r) = show r
  show (VarL2X v) = show v

instance Show L2S where
  show (NumberL2S n) = show n
  show (LabelL2S l)  = show l
  show (XL2S x)      = show x

instance Eq  L2X where (==) x1 x2 = show x1 == show x2
instance Ord L2X where compare x1 x2 = compare (show x1) (show x2)

orderedPair :: Ord a => a -> a -> (a, a)
orderedPair a1 a2 = if a1 < a2 then (a1, a2) else (a2, a1)

parseError :: String -> String -> Either String a
parseError  msg expr = Left $ concat ["Parse Error: '", msg, "' in: ", show expr]
parseError_ :: String -> SExpr -> Either String a
parseError_ msg expr = parseError msg (show expr)

parseLabel :: String -> ParseResult Label
parseLabel l@(':' : ':' : _) = Left $ "invalid label: " ++ l
parseLabel l@(':' : _) = Right $ Label l
parseLabel l = Left $ "invalid label: " ++ l  

instance AsSExpr Label where
  asSExpr (Label l) = AtomSym l

instance FromSExpr Label where
  fromSExpr (AtomSym l) = parseLabel l
  fromSExpr bad = Left $ "bad label" ++ show bad

instance FromSExpr X86Op where
  fromSExpr (AtomSym "+=")  = Right increment
  fromSExpr (AtomSym "-=")  = Right decrement
  fromSExpr (AtomSym "*=")  = Right multiply
  fromSExpr (AtomSym "<<=") = Right leftShift
  fromSExpr (AtomSym ">>=") = Right rightShift
  fromSExpr (AtomSym "&=")  = Right bitwiseAnd
  fromSExpr bad             = parseError_ "bad operator" bad

instance FromSExpr CompOp where
  fromSExpr (AtomSym s) = compOpFromSym s
  fromSExpr bad         = fail $ "not a comparison operator" ++ show bad

instance (FromSExpr s) => FromSExpr (Comp s) where
  fromSExpr (List [s1, cmp, s2]) = 
    liftM3 Comp (fromSExpr s1) (fromSExpr cmp) (fromSExpr s2)
  fromSExpr bad         = fail $ "not a comparison operator" ++ show bad

instance (FromSExpr x, FromSExpr s) => FromSExpr (Program x s) where
  fromSExpr (List ((List main) : funcs)) =
    liftM2 Program (parseMain main) (traverse fromSExpr funcs)
  fromSExpr bad = parseError_ "bad program" bad

parseMain :: (FromSExpr x, FromSExpr s) => [SExpr] -> ParseResult (Func x s)
parseMain exps = do
  bdy <- traverse fromSExpr exps
  return $ Func $ (LabelDeclaration $ Label ":main") : bdy

instance (FromSExpr x, FromSExpr s) => FromSExpr (Func x s) where
  fromSExpr (List ((AtomSym name) : exps)) = do
    bdy   <- traverse fromSExpr exps
    label <- parseLabel name
    return $ Func $ LabelDeclaration label : bdy
  fromSExpr bad = parseError_ "bad function" bad

instance (FromSExpr x, FromSExpr s) => FromSExpr (Instruction x s) where
  fromSExpr (AtomSym s)   = LabelDeclaration <$> parseLabel s
  fromSExpr a@(AtomNum _) = parseError_ "bad instruction" a
  fromSExpr list@(List _) = case flatten list of
    [x, AtomSym "<-", AtomSym "print", s] -> liftM2 Assign (fromSExpr x) (Print <$> fromSExpr s)
    [x, AtomSym "<-", AtomSym "allocate", s1, s2] ->
      liftM2 Assign (fromSExpr x) (liftM2 Allocate (fromSExpr s1) (fromSExpr s2))
    [x, AtomSym "<-", AtomSym "array-error", s1, s2] ->
      liftM2 Assign (fromSExpr x) (liftM2 ArrayError (fromSExpr s1) (fromSExpr s2))
    [x, AtomSym "<-", s]  -> liftM2 Assign (fromSExpr x) (SRHS <$> fromSExpr s)
    [x1, AtomSym "<-", AtomSym "mem", x2, n4] ->
      liftM2 Assign (fromSExpr x1) (MemRead <$> liftM2 MemLoc (fromSExpr x2) (parseN4 n4))
    [AtomSym "mem", x, n4, AtomSym "<-", s] ->
      liftM2 MemWrite (liftM2 MemLoc (fromSExpr x) (parseN4 n4)) (fromSExpr s)
    [x, op, s] -> liftM3 MathInst (fromSExpr x) (fromSExpr op) (fromSExpr s)
    [cx, AtomSym "<-", s1, cmp, s2] -> 
      liftM2 Assign (fromSExpr cx) (CompRHS <$> parseComp s1 cmp s2)
    [AtomSym "goto", l]      -> Goto <$> fromSExpr l
    [AtomSym "cjump", s1, cmp, s2, l1, l2] ->
      liftM3 CJump (parseComp s1 cmp s2) (fromSExpr l1) (fromSExpr l2)
    [AtomSym "call", s]      -> Call <$> fromSExpr s
    [AtomSym "tail-call", s] -> TailCall <$> fromSExpr s
    [AtomSym "return"]       -> Right Return
    _ -> parseError_ "bad instruction" list
    where
      parseComp s1 cmp s2 = fromSExpr (List [s1, cmp, s2])
      -- todo add check for divisible by 4
      parseN4 (AtomNum n) = Right n
      parseN4 bad         = parseError_ "not a number" bad

parseLabelOrRegister :: (Label -> a) -> (Register -> a) -> String -> ParseResult a
parseLabelOrRegister f _ l@(':' : _) = f <$> parseLabel l
parseLabelOrRegister _ f r           = f <$> registerFromName r

instance FromSExpr L1S where
  fromSExpr (AtomNum n) = Right $ NumberL1S (fromIntegral n)
  fromSExpr (AtomSym s) = parseLabelOrRegister LabelL1S RegL1S s
  fromSExpr bad         = parseError_ "bad L1S" bad

instance FromSExpr L1 where
  fromSExpr s = L1 <$> fromSExpr s

parseL2X v r s = Right $ either (\_ -> v s) r (registerFromName s)

instance FromSExpr L2X where
  fromSExpr (AtomSym s) = parseL2X (VarL2X . Variable) RegL2X s
  fromSExpr bad         = parseError_ "bad L2X" bad

instance FromSExpr L2S where
  fromSExpr (AtomNum n) = Right $ NumberL2S (fromIntegral n)
  fromSExpr (AtomSym s) = either 
    (\_-> parseL2X (XL2S . VarL2X . Variable) (XL2S . RegL2X) s)
    Right
    (parseLabelOrRegister LabelL2S (XL2S . RegL2X) s)
  fromSExpr bad         = parseError_ "bad L2S" bad

instance FromSExpr L2 where
  fromSExpr s = L2 <$> fromSExpr s 

parseInstructionList :: (FromSExpr x, FromSExpr s) =>  SExpr -> ParseResult [(Instruction x s)]
parseInstructionList (List exps) = traverse fromSExpr exps
parseInstructionList bad = parseError_ "not an instruction list" bad

parseL1OrDie :: SExpr -> L1
parseL1OrDie = (either error id) . fromSExpr

parseL2OrDie :: SExpr -> L2
parseL2OrDie = (either error id) . fromSExpr

