{-# LANGUAGE TypeOperators #-}

module L.L3.L3 (
   l3Language
 , l3Language1
 , l3LanguageShowable
 , l3Compiler
 , l3Compiler1
 , l3CompilerShowable
 , l3Interpreter
) where

import L.Compiler
import L.L1.L1L2AST
import L.L2.L2 
import L.L3.L3AST
import L.L3.L3Interp
import L.L3.Linearize

l3Compiler1 :: Compiler1 L3 L2
l3Compiler1 = Compiler1 (\_ _ -> return . compileL3ToL2) "L3"

l3Compiler :: Compiler L3 X86
l3Compiler = unconstrain l3CompilerShowable

l3CompilerShowable :: Thrist (Show :=> Compiler1) L3 X86
l3CompilerShowable = Cons (Constrained l3Compiler1) l2CompilerShowable

l3Interpreter :: Interpreter L3
l3Interpreter = interpL3

l3Language1 :: Language1 L3 L2
l3Language1  = Language1 l3Compiler1 interpL3

l3Language :: Language L3 X86
l3Language = Cons l3Language1 l2Language

l3LanguageShowable :: Thrist (Show :=> Language1) L3 X86
l3LanguageShowable = Cons (Constrained l3Language1) l2LanguageShowable

compileL3ToL2 :: L3 -> L2
compileL3ToL2 = linearize
