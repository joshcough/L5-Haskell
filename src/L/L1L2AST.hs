{-# LANGUAGE TypeSynonymInstances, FlexibleInstances, NoMonomorphismRestriction, DefaultSignatures #-}

module L.L1L2AST where

import Control.Lens
import Data.Bits
import L.Read (showAsList)
import Prelude hiding (LT, EQ)


type Label      = String

data XRegister  = Esi | Rsi | Edi | Rdi | Ebp | Rbp | Esp | Rsp deriving (Eq,Ord,Enum,Bounded)

class AsXRegister t where
  _XR :: Prism' t XRegister
  default _XR :: AsRegister t => Prism' t XRegister
  _XR = _Register._XR

instance AsXRegister XRegister where
  _XR = id


data CXRegister = Eax | Rax | Ebx | Rbx | Ecx | Rcx | Edx | Rdx deriving (Eq,Ord,Enum,Bounded)

class AsCXRegister t where
  _CXR :: Prism' t CXRegister
  default _CXR :: AsRegister t => Prism' t CXRegister
  _CXR = _Register._CXR
 
instance AsCXRegister CXRegister where
  _CXR = id

data Register   = CXR CXRegister | XR XRegister  deriving (Eq,Ord)

class (AsXRegister t, AsCXRegister t) => AsRegister t where
  _Register :: Prism' t Register

instance AsRegister Register where
  _Register = id

instance AsXRegister Register where
  _XR = prism XR $ \r -> case r of
    XR x -> Right x
    _ -> Left r

instance AsCXRegister Register where
  _CXR = prism CXR $ \r -> case r of
    CXR x -> Right x
    _ -> Left r

esi = _XR # Esi
edi = _XR # Edi
ebp = _XR # Ebp
esp = _XR # Esp
rsi = _XR # Rsi
rdi = _XR # Rdi
rbp = _XR # Rbp
rsp = _XR # Rsp
eax = _CXR # Eax
ebx = _CXR # Ebx
ecx = _CXR # Ecx
edx = _CXR # Edx
rax = _CXR # Rax
rbx = _CXR # Rbx
rcx = _CXR # Rcx
rdx = _CXR # Rdx

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
  Call s                     |
  TailCall s                 |
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

x86OpName Increment  = "addl"
x86OpName Decrement  = "subl"
x86OpName Multiply   = "imull"
x86OpName LeftShift  = "sall"
x86OpName RightShift = "sarl"
x86OpName BitwiseAnd = "andl"


--data X86Op = Increment | Decrement | Multiply | LeftShift | RightShift | BitwiseAnd
runOp :: X86Op -> Int -> Int -> Int
runOp Increment  = (+)
runOp Decrement  = (-)
runOp Multiply   = (*)
runOp LeftShift  = shiftL
runOp RightShift = shiftR
runOp BitwiseAnd = (.&.)

instance (Show x, Show s) => Show (Program x s) where
  show (Program main fs) = unlines ["(", show main, fs >>= show, ")"]

instance (Show x, Show s) => Show (Func x s) where
  show (Func is) = "(" ++ (is >>= (\i -> ((show i) ++ "\n\t"))) ++ ")"

instance (Show x, Show s) => Show (Instruction x s) where
  show (Assign x rhs)       = showAsList [show x, "<-", show rhs]
  show (MathInst x op s)    = showAsList [show x, x86OpSymbol op, show s]
  show (MemWrite loc s)     = showAsList [show loc, "<-", show s]
  show (Goto l)             = showAsList ["goto", ":" ++ l]
  show (CJump cmp l1 l2)    = showAsList ["cjump", show cmp, ":" ++ l1, ":" ++ l2]
  show (LabelDeclaration l) = ":" ++ l
  show (Call s)             = showAsList ["call", show s]
  show (TailCall s)         = showAsList ["tail-call", show s]
  show Return               = "(return)"

instance Show XRegister where
  show Esi = "esi"
  show Edi = "edi"
  show Ebp = "ebp"
  show Esp = "esp"
  show Rsi = "rsi"
  show Rdi = "rdi"
  show Rbp = "rbp"
  show Rsp = "rsp"
instance Show CXRegister where
  show Eax = "eax"
  show Ebx = "ebx"
  show Ecx = "ecx"
  show Edx = "edx"
  show Rax = "rax"
  show Rbx = "rbx"
  show Rcx = "rcx"
  show Rdx = "rdx"

instance Show Register where
  show (CXR cxr) = show cxr
  show (XR xr)   = show xr

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

xRegisters      = [Esi, Edi, Ebp, Esp]
cxRegisters     = [Eax, Ebx, Ecx, Edx]
xRegisterNames  = map show xRegisters
cxRegisterNames = map show cxRegisters

xRegisterFromName :: String -> Maybe XRegister
xRegisterFromName "esi" = Just Esi
xRegisterFromName "edi" = Just Edi
xRegisterFromName "ebp" = Just Ebp
xRegisterFromName "esp" = Just Esp
xRegisterFromName _     = Nothing

cxRegisterFromName :: String -> Maybe CXRegister
cxRegisterFromName "eax" = Just Eax
cxRegisterFromName "ebx" = Just Ebx
cxRegisterFromName "ecx" = Just Ecx
cxRegisterFromName "edx" = Just Edx
cxRegisterFromName _     = Nothing

registerFromName :: String -> Maybe Register
registerFromName s = maybe (fmap CXR (cxRegisterFromName s)) (Just . XR) (xRegisterFromName s)

is32Bit :: Register -> Bool
is32Bit (XR Esi)  = True
is32Bit (XR Edi)  = True
is32Bit (XR Ebp)  = True
is32Bit (XR Esp)  = True
is32Bit (CXR Eax) = True
is32Bit (CXR Ebx) = True
is32Bit (CXR Ecx) = True
is32Bit (CXR Edx) = True
is32Bit _ = False

is64Bit :: Register -> Bool
is64Bit = (not . is32Bit)

instance Show CompOp where
  show LT   = "<"
  show LTEQ = "<="
  show EQ   = "="

cmp :: CompOp -> Int -> Int -> Bool
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
data L1S = NumberL1S Int | LabelL1S Label | RegL1S Register deriving (Eq, Ord)
type L1Instruction = Instruction L1X L1S
type L1Func = Func L1X L1S
type L1 = Program L1X L1S

instance Show L1S where
  show (NumberL1S n) = show n
  show (LabelL1S l)  = ":" ++ l
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

instance AsXRegister L2X
instance AsCXRegister L2X

data L2S = XL2S L2X | NumberL2S Int | LabelL2S Label deriving (Eq, Ord)
type L2MemLoc = MemLoc L2X
type L2Instruction = Instruction L2X L2S
type L2Func = Func L2X L2S
type L2 = Program L2X L2S

instance Show L2X where
  show (RegL2X r) = show r
  show (VarL2X v) = v

instance Show L2S where
  show (NumberL2S n)   = show n
  show (LabelL2S l)    = ":" ++ l
  show (XL2S x)        = show x

instance Eq  L2X where (==) x1 x2 = show x1 == show x2
instance Ord L2X where compare x1 x2 = compare (show x1) (show x2)

orderedPair :: Ord a => a -> a -> (a, a)
orderedPair a1 a2 = if (a1 < a2) then (a1, a2) else (a2, a1)
