module L.L1.L1 (l1Language) where

import L.Compiler
import L.L1L2AST
import L.L1L2Parser
import L.L1.L1Interp
import L.L1.MainAdjuster
import L.L1.L1X86

l1Language :: Language L1 String
l1Language  = Language parseL1 compileL1 interpL1 "L1" Nothing

compileL1 :: L1 -> Either String String
compileL1 = genX86Code . adjustMain
