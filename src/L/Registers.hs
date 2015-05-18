{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE LambdaCase #-}

module L.Registers where

import Control.Lens
import Data.Map as Map
import L.Parser.SExpr
import Prelude hiding (LT, EQ)

data Register =
 Rax | Rbx | Rcx | Rdx |
 Rsi | Rdi | Rbp | Rsp |
 R8  | R9  | R10 | R11 |
 R12 | R13 | R14 | R15 deriving (Eq,Ord,Enum,Bounded)

instance Show Register where
  show Rax = "rax"; show Rbx = "rbx"; show Rcx = "rcx"; show Rdx = "rdx"
  show Rsi = "rsi"; show Rdi = "rdi"; show Rbp = "rbp"; show Rsp = "rsp"
  show R8  = "r8";  show R9  = "r9";  show R10 = "r10"; show R11 = "r11"
  show R12 = "r12"; show R13 = "r13"; show R14 = "r14"; show R15 = "r15"

class AsRegister t where
  _Register :: Prism' t Register

instance AsRegister Register where
  _Register = id

rsi, rdi, rbp, rsp, rax, rbx, rcx, rdx, r8, r9, r10, r11, r12, r13, r14, r15 :: AsRegister t => t
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

registersList :: [Register]
registersList   = [Rax, Rbx, Rcx, Rdx, Rsi, Rdi, Rbp, Rsp, R8, R9, R10, R11, R12, R13, R14, R15]
-- saving r15 for storing labels into memory
allocatableRegisters :: AsRegister t => [t]
allocatableRegisters = [rax, rbx, rcx, rdx, rdi, rsi, r8, r9, r10, r11, r12, r13, r14]
registerNames :: [String]
registerNames  = fmap show registersList
registerNamesMap :: Map String Register
registerNamesMap = Map.fromList (zip registerNames registersList)

registerFromName :: String -> Either String Register
registerFromName s = maybe (badRegister s) return (Map.lookup s registerNamesMap)

badRegister :: String -> Either String Register
badRegister s = Left $ "invalid register: " ++ s

instance AsSExpr Register where
  asSExpr = AtomSym . show 

instance FromSExpr Register where
  fromSExpr (AtomSym s) = registerFromName s
  fromSExpr bad = badRegister $ show bad

low8 :: Register -> String
low8 Rax = "al"
low8 Rbx = "bl"
low8 Rcx = "cl"
low8 Rdx = "dl"
low8 Rsi = "sil"
low8 Rdi = "dil"
low8 Rbp = "bpl"
low8 Rsp = "spl"
low8 R8  = "r8b"
low8 R9  = "r9b"
low8 R10 = "r10b"
low8 R11 = "r11b"
low8 R12 = "r12b"
low8 R13 = "r13b"
low8 R14 = "r14b"
low8 R15 = "r15b"

low16 :: Register -> String
low16 Rax = "ax"
low16 Rbx = "bx"
low16 Rcx = "cx"
low16 Rdx = "dx"
low16 Rsi = "si"
low16 Rdi = "di"
low16 Rbp = "bp"
low16 Rsp = "sp"
low16 R8  = "r8w"
low16 R9  = "r9w"
low16 R10 = "r10w"
low16 R11 = "r11w"
low16 R12 = "r12w"
low16 R13 = "r13w"
low16 R14 = "r14w"
low16 R15 = "r15w"

low32 :: Register -> String
low32 Rax = "eax"
low32 Rbx = "ebx"
low32 Rcx = "ecx"
low32 Rdx = "edx"
low32 Rsi = "esi"
low32 Rdi = "edi"
low32 Rbp = "ebp"
low32 Rsp = "esp"
low32 R8  = "r8d"
low32 R9  = "r9d"
low32 R10 = "r10d"
low32 R11 = "r11d"
low32 R12 = "r12d"
low32 R13 = "r13d"
low32 R14 = "r14d"
low32 R15 = "r15d"
