{-# LANGUAGE FlexibleContexts #-}

module L.L1.L1 (l1Compiler, l1Language) where

import Control.Lens
import Control.Monad.Error.Class
import L.Compiler
import L.LCompiler
import L.L1L2AST
import L.L1L2Parser
import L.L1.L1Interp
import L.L1L2MainAdjuster
import L.L1.L1X86

l1Language :: Language L1 String
l1Language  = Language parseL1 compileL1 interpL1 "L1" Nothing

l1Compiler :: CompilerMonad m => LCompiler m L1 String
l1Compiler = LCompiler $ \opts name -> either exception return . compileL1 opts name

compileL1 :: CompilationOptions -> ProgramName -> L1 -> Either String String
compileL1 opts name = genX86Code name (opts^.os) . adjustMain
