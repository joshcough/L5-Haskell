module L.L1.L1 (l1Language) where

import Control.Lens
import L.Compiler
import L.L1.L1L2AST
import L.Parser.SExpr
import L.L1.L1Interp
import L.L1.L1L2MainAdjuster
import L.L1.L1X86

l1Language :: Language L1 String
l1Language  = Language fromSExpr compileL1 interpL1 "L1" Nothing

compileL1 :: CompilationOptions -> ProgramName -> L1 -> Either String String
compileL1 opts name (L1 p) = genX86Code name (opts^.os) . L1 $ adjustMain p
