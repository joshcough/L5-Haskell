{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE LambdaCase #-}

module L.L1.L1L2AST where

import Control.Applicative
import Control.Lens
import Control.Monad
import Data.Int
import Data.Map as Map
import Data.Tuple
import L.Parser.SExpr
import L.Registers
import L.Variable
import L.Primitives (Label(..))
import Prelude hiding (LT, EQ)

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

instance (AsSExpr x, AsSExpr s) => Show (Program x s)     where show = showSExpr
instance (AsSExpr x, AsSExpr s) => Show (Func x s)        where show = showSExpr
instance (AsSExpr x, AsSExpr s) => Show (Instruction x s) where show = showSExpr
instance (AsSExpr x, AsSExpr s) => Show (AssignRHS x s)   where show = showSExpr
instance (AsSExpr x)            => Show (MemLoc x)        where show = showSExpr
instance (AsSExpr s)            => Show (Comp s)          where show = showSExpr

instance (AsSExpr x, AsSExpr s) => AsSExpr (Program x s) where
  asSExpr (Program main fs) = asSExpr [(main, fs)]

instance (AsSExpr x, AsSExpr s) => AsSExpr (Instruction x s) where
  asSExpr (Assign x (CompRHS c)) = List $ flatten $ asSExpr (x, sym "<-", c)
  asSExpr (Assign x rhs)         = asSExpr (x, sym "<-", rhs)
  asSExpr (MathInst x op s)      = asSExpr (x, sym $ x86OpSymbol op, s)
  asSExpr (MemWrite loc s)       = asSExpr (loc, sym "<-", s)
  asSExpr (Goto l)               = asSExpr (sym "goto", l)
  asSExpr (CJump cmp l1 l2)      = List $ flatten $ asSExpr (sym "cjump", cmp, l1, l2)
  asSExpr (LabelDeclaration l)   = asSExpr l
  asSExpr (Call s)               = asSExpr (sym "call", s)
  asSExpr (TailCall s)           = asSExpr (sym "tail-call", s)
  asSExpr Return                 = asSExpr [sym "return"]

instance (AsSExpr x, AsSExpr s) => AsSExpr (AssignRHS x s) where
  asSExpr (CompRHS c)        = asSExpr c
  asSExpr (Allocate s1 s2)   = asSExpr (sym "allocate", s1, s2)
  asSExpr (Print s)          = asSExpr (sym "print", s)
  asSExpr (ArrayError s1 s2) = asSExpr (sym "array-error", s1, s2)
  asSExpr (SRHS s)           = asSExpr s
  asSExpr (MemRead loc)      = asSExpr loc

instance (AsSExpr x) => AsSExpr (MemLoc x) where
  asSExpr (MemLoc x n) = asSExpr (sym "mem", x, n)

instance (AsSExpr s) => AsSExpr (Comp s) where
  asSExpr (Comp s1 op s2) = asSExpr (s1,op, s2)

instance AsSExpr CompOp where asSExpr = AtomSym . show

instance Show CompOp where
  show LT   = "<"
  show LTEQ = "<="
  show EQ   = "="

cmp :: Ord a => CompOp -> a -> a -> Bool
cmp LT   = (<)
cmp LTEQ = (<=)
cmp EQ   = (==)

compOpFromSym :: String -> Either String CompOp
compOpFromSym "<"  = return LT
compOpFromSym "<=" = return LTEQ
compOpFromSym "="  = return EQ
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

instance Show L1  where show = showSExpr
instance Show L1S where show = showSExpr

-- L2 AST (uses shared L1/L2 AST)
-- L2 adds variables to X and S. that's the only difference between L2 and L1.
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

instance Show L2  where show (L2 p) = show p
instance Show L2X where show = showSExpr
instance Show L2S where show = showSExpr

instance AsSExpr L2X where
  asSExpr (RegL2X r) = asSExpr r
  asSExpr (VarL2X v) = asSExpr v

instance AsSExpr L2S where
  asSExpr (NumberL2S n) = asSExpr n
  asSExpr (LabelL2S l)  = asSExpr l
  asSExpr (XL2S x)      = asSExpr x

instance Eq  L2X where (==) x1 x2 = show x1 == show x2
instance Ord L2X where compare x1 x2 = compare (show x1) (show x2)

orderedPair :: Ord a => a -> a -> (a, a)
orderedPair a1 a2 = if a1 < a2 then (a1, a2) else (a2, a1)

instance AsSExpr X86Op where
  asSExpr = AtomSym . x86OpSymbol 

instance FromSExpr X86Op where
  fromSExpr (AtomSym "+=")  = return increment
  fromSExpr (AtomSym "-=")  = return decrement
  fromSExpr (AtomSym "*=")  = return multiply
  fromSExpr (AtomSym "<<=") = return leftShift
  fromSExpr (AtomSym ">>=") = return rightShift
  fromSExpr (AtomSym "&=")  = return bitwiseAnd
  fromSExpr bad             = parseError_ "bad operator" bad

instance FromSExpr CompOp where
  fromSExpr (AtomSym s) = compOpFromSym s
  fromSExpr bad         = Left $ "not a comparison operator" ++ showSExpr bad

instance (FromSExpr s) => FromSExpr (Comp s) where
  fromSExpr (List [s1, cmp, s2]) = 
    Comp <$> fromSExpr s1 <*> fromSExpr cmp <*> fromSExpr s2
  fromSExpr bad         = Left $ "not a comparison operator" ++ showSExpr bad

instance (FromSExpr x, FromSExpr s) => FromSExpr (Program x s) where
  fromSExpr (List ((List main) : funcs)) = Program <$> parseMain main <*> traverse fromSExpr funcs
  fromSExpr bad = parseError_ "bad program" bad

parseMain :: (FromSExpr x, FromSExpr s) => [SExpr] -> ParseResult (Func x s)
parseMain exps = do
  bdy <- traverse fromSExpr exps
  return $ Func $ (LabelDeclaration $ Label ":main") : bdy

instance (AsSExpr x, AsSExpr s) => AsSExpr (Func x s) where
  asSExpr (Func xs) = List $ fmap asSExpr xs

instance (FromSExpr x, FromSExpr s) => FromSExpr (Func x s) where
  fromSExpr (List ((AtomSym name) : exps)) = do
    bdy   <- traverse fromSExpr exps
    label <- fromString name
    return $ Func $ LabelDeclaration label : bdy
  fromSExpr bad = parseError_ "bad function" bad

instance (FromSExpr x, FromSExpr s) => FromSExpr (Instruction x s) where
  fromSExpr (AtomSym s)   = LabelDeclaration <$> fromString s
  fromSExpr a@(AtomNum _) = parseError_ "bad instruction" a
  fromSExpr list@(List _) = case flatten list of
    [x, AtomSym "<-", AtomSym "print", s] -> Assign <$> fromSExpr x <*> (Print <$> fromSExpr s)
    [x, AtomSym "<-", AtomSym "allocate", s1, s2] ->
      Assign   <$> fromSExpr x <*> (Allocate <$> fromSExpr s1 <*> fromSExpr s2)
    [x, AtomSym "<-", AtomSym "array-error", s1, s2] ->
      Assign   <$> fromSExpr x <*> (ArrayError <$> fromSExpr s1 <*> fromSExpr s2)
    [x, AtomSym "<-", s]  -> Assign <$> fromSExpr x <*> (SRHS <$> fromSExpr s)
    [x1, AtomSym "<-", AtomSym "mem", x2, n4] ->
      Assign   <$> fromSExpr x1 <*> (MemRead <$> (MemLoc <$> fromSExpr x2 <*> parseN4 n4))
    [AtomSym "mem", x, n4, AtomSym "<-", s] ->
      MemWrite <$> (MemLoc <$> fromSExpr x <*> parseN4 n4)  <*> fromSExpr s
    [x, op, s] -> MathInst <$> fromSExpr x <*> fromSExpr op <*> fromSExpr s
    [cx, AtomSym "<-", s1, cmp, s2] -> Assign <$> fromSExpr cx <*>  (CompRHS <$> parseComp s1 cmp s2)
    [AtomSym "goto", l]      -> Goto <$> fromSExpr l
    [AtomSym "cjump", s1, cmp, s2, l1, l2] ->
      CJump <$> parseComp s1 cmp s2 <*> fromSExpr l1 <*> fromSExpr l2
    [AtomSym "call", s]      -> Call     <$> fromSExpr s
    [AtomSym "tail-call", s] -> TailCall <$> fromSExpr s
    [AtomSym "return"]       -> return Return
    _ -> parseError_ "bad instruction" list
    where
      parseComp s1 cmp s2 = fromSExpr (List [s1, cmp, s2])
      -- todo add check for divisible by 4
      parseN4 (AtomNum n) = return n
      parseN4 bad         = parseError_ "not a number" bad

parseLabelOrRegister :: (Label -> a) -> (Register -> a) -> String -> ParseResult a
parseLabelOrRegister f _ l@(':' : _) = f <$> fromString l
parseLabelOrRegister _ f r           = f <$> registerFromName r

instance AsSExpr L1S where
  asSExpr (NumberL1S n) = asSExpr n
  asSExpr (LabelL1S  l) = asSExpr l
  asSExpr (RegL1S    r) = asSExpr r

instance FromSExpr L1S where
  fromSExpr (AtomNum n) = return $ NumberL1S (fromIntegral n)
  fromSExpr (AtomSym s) = parseLabelOrRegister LabelL1S RegL1S s
  fromSExpr bad         = parseError_ "bad L1S" bad

instance AsSExpr L1 where
  asSExpr (L1 p) = asSExpr p

instance FromSExpr L1 where
  fromSExpr s = L1 <$> fromSExpr s

parseL2X v r s = return $ either (\_ -> v s) r (registerFromName s)

instance FromSExpr L2X where
  fromSExpr (AtomSym s) = parseL2X (VarL2X . Variable) RegL2X s
  fromSExpr bad         = parseError_ "bad L2X" bad

instance FromSExpr L2S where
  fromSExpr (AtomNum n) = return $ NumberL2S (fromIntegral n)
  fromSExpr (AtomSym s) = either 
    (\_-> parseL2X (XL2S . VarL2X . Variable) (XL2S . RegL2X) s)
    return
    (parseLabelOrRegister LabelL2S (XL2S . RegL2X) s)
  fromSExpr bad         = parseError_ "bad L2S" bad

instance AsSExpr L2 where
  asSExpr (L2 p) = asSExpr p

instance FromSExpr L2 where
  fromSExpr s = L2 <$> fromSExpr s 

parseInstructionList :: (FromSExpr x, FromSExpr s) =>  SExpr -> ParseResult [(Instruction x s)]
parseInstructionList (List exps) = traverse fromSExpr exps
parseInstructionList bad = parseError_ "not an instruction list" bad

parseL1OrDie :: SExpr -> L1
parseL1OrDie = (either error id) . fromSExpr

parseL2OrDie :: SExpr -> L2
parseL2OrDie = (either error id) . fromSExpr

