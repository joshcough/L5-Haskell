{-# LANGUAGE TypeOperators #-}

module L.L1.L1 (
   l1Language
 , l1Language1
 , l1Compiler1
 , l1Compiler
 , l1Interpreter
 , x86Compiler1
 , x86Compiler
 , x86Language
 , x86Language1
) where

import Control.Lens
import L.Compiler
import L.L1.L1L2AST
import L.L1.L1Interp
import L.L1.L1L2MainAdjuster
import L.L1.L1X86

l1Compiler1 :: Compiler1 L1 X86
l1Compiler1 = Compiler1 compileL1 "L1"

l1Compiler :: Compiler L1 X86
l1Compiler = Cons (Constrained l1Compiler1) x86Compiler

l1Interpreter :: Interpreter L1
l1Interpreter = interpL1

l1Language1 :: Language1 L1 X86
l1Language1  = Language1 l1Compiler1 l1Interpreter

l1Language :: Language L1 X86
l1Language = Cons (Constrained l1Language1) x86Language

x86Compiler1 :: Compiler1 X86 X86
x86Compiler1 = Compiler1 (\_ _ x86 -> return x86) ".S"

x86Compiler :: Compiler X86 X86
x86Compiler = Nil (Constrained x86Compiler1)

x86Language1 :: Language1 X86 X86
x86Language1 = Language1 x86Compiler1 (error "todo: x86 interpreter?")

x86Language :: Language X86 X86
x86Language = Nil (Constrained x86Language1)

compileL1 :: CompilationOptions -> ProgramName -> L1 -> Either String X86
compileL1 opts name (L1 p) = genX86Code name (opts^.os) . L1 $ adjustMain p
