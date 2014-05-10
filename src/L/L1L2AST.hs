{-# LANGUAGE TypeSynonymInstances, FlexibleInstances, NoMonomorphismRestriction, DefaultSignatures #-}

module L.L1L2AST where

import Control.Lens
import Data.Bits
import Data.Int
import Data.Map (Map)
import Data.Map as M
import L.Read (showAsList)
import Prelude hiding (LT, EQ)

type Label    = String

data Register =
 Rax | Rbx | Rcx | Rdx |
 Rsi | Rdi | Rbp | Rsp |
 R8  | R9  | R10 | R11 |
 R12 | R13 | R14 | R15 deriving (Eq,Ord,Enum,Bounded)

class AsRegister t where
  _Register :: Prism' t Register

instance AsRegister Register where
  _Register = id

rsi = _Register # Rsi
rdi = _Register # Rdi
rbp = _Register # Rbp
rsp = _Register # Rsp
r8  = _Register # R8
r9  = _Register # R9
r10 = _Register # R10
r11 = _Register # R11
r12 = _Register # R12
r13 = _Register # R13
r14 = _Register # R14
r15 = _Register # R15
rax = _Register # Rax
rbx = _Register # Rbx
rcx = _Register # Rcx
rdx = _Register # Rdx

data MemLoc x   = MemLoc x Int deriving (Eq, Ord)
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
  Call Label                 |
  TailCall Label             |
  Return deriving (Eq, Ord)

data Func x s = Func { body :: [Instruction x s]}
data Program x s = Program (Func x s) [Func x s]

increment  = Increment
decrement  = Decrement
multiply   = Multiply
leftShift  = LeftShift
rightShift = RightShift
bitwiseAnd = BitwiseAnd

x86OpSymbol Increment  = "+="
x86OpSymbol Decrement  = "-="
x86OpSymbol Multiply   = "*="
x86OpSymbol LeftShift  = "<<="
x86OpSymbol RightShift = ">>="
x86OpSymbol BitwiseAnd = "&="

x86OpName Increment  = "addq"
x86OpName Decrement  = "subq"
x86OpName Multiply   = "imulq"
x86OpName LeftShift  = "salq"
x86OpName RightShift = "sarq"
x86OpName BitwiseAnd = "andq"

runOp :: (Num a, Bits a, Integral a) => X86Op -> a -> a -> a
runOp Increment  l r = l + r
runOp Decrement  l r = l - r
runOp Multiply   l r = l * r
runOp LeftShift  i amount = shiftL i (fromIntegral amount)
runOp RightShift i amount = shiftR i (fromIntegral amount)
runOp BitwiseAnd l r = l .&. r

instance (Show x, Show s) => Show (Program x s) where
  show (Program main fs) = unlines ["(", show main, fs >>= show, ")"]

instance (Show x, Show s) => Show (Func x s) where
  show (Func is) = "(" ++ (is >>= (\i -> ((show i) ++ "\n\t"))) ++ ")"

instance (Show x, Show s) => Show (Instruction x s) where
  show (Assign x rhs)       = showAsList [show x, "<-", show rhs]
  show (MathInst x op s)    = showAsList [show x, x86OpSymbol op, show s]
  show (MemWrite loc s)     = showAsList [show loc, "<-", show s]
  show (Goto l)             = showAsList ["goto", l]
  show (CJump cmp l1 l2)    = showAsList ["cjump", show cmp, l1, l2]
  show (LabelDeclaration l) = l
  show (Call s)             = showAsList ["call", s]
  show (TailCall s)         = showAsList ["tail-call", s]
  show Return               = "(return)"

instance Show Register where
  show Rax = "rax"
  show Rbx = "rbx"
  show Rcx = "rcx"
  show Rdx = "rdx"
  show Rsi = "rsi"
  show Rdi = "rdi"
  show Rbp = "rbp"
  show Rsp = "rsp"
  show R8  = "r8"
  show R9  = "r9"
  show R10 = "r10"
  show R11 = "r11"
  show R12 = "r12"
  show R13 = "r13"
  show R14 = "r14"
  show R15 = "r15"

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

registers :: [Register]
registers      = [Rax, Rbx, Rcx, Rdx, Rsi, Rdi, Rbp, Rsp, R8, R9, R10, R11, R12, R13, R14, R15]
allocatableRegisters = [rax, rbx, rcx, rdx, rdi, rsi, r8, r9, r10, r11, r12, r13, r14, r15]
registerNames :: [String]
registerNames  = fmap show registers
registerNamesMap :: Map String Register
registerNamesMap = M.fromList (zip registerNames registers)

registerFromName :: String -> Maybe Register
registerFromName s = M.lookup s registerNamesMap

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
data L1S = CharL1S Char | NumberL1S Int64 | LabelL1S Label | RegL1S Register deriving (Eq, Ord)
type L1Instruction = Instruction L1X L1S
type L1Func = Func L1X L1S
type L1 = Program L1X L1S

instance Show L1S where
  show (CharL1S c)   = show c
  show (NumberL1S n) = show n
  show (LabelL1S l)  = l
  show (RegL1S r)    = show r

-- L2 AST (uses shared L1/L2 AST)
-- L2 adds variables to X and S. that's the only difference between L2 and L1.
type Variable = String

class AsVariable t where
  _Variable :: Prism' t String

instance AsVariable String where
  _Variable = id

data L2X = RegL2X Register | VarL2X Variable

instance AsRegister L2X where
  _Register = prism RegL2X $ \r -> case r of
    RegL2X x -> Right x
    _        -> Left r

instance AsVariable L2X where
  _Variable = prism VarL2X $ \r -> case r of
    VarL2X x -> Right x
    _        -> Left r

data L2S = XL2S L2X | NumberL2S Int64 | LabelL2S Label deriving (Eq, Ord)
type L2MemLoc = MemLoc L2X
type L2Instruction = Instruction L2X L2S
type L2Func = Func L2X L2S
type L2 = Program L2X L2S

instance Show L2X where
  show (RegL2X r) = show r
  show (VarL2X v) = v

instance Show L2S where
  show (NumberL2S n)   = show n
  show (LabelL2S l)    = l
  show (XL2S x)        = show x

instance Eq  L2X where (==) x1 x2 = show x1 == show x2
instance Ord L2X where compare x1 x2 = compare (show x1) (show x2)

orderedPair :: Ord a => a -> a -> (a, a)
orderedPair a1 a2 = if (a1 < a2) then (a1, a2) else (a2, a1)
