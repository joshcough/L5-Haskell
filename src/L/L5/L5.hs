{-# LANGUAGE TypeOperators #-}

module L.L5.L5 (
   l5Language
 , l5Language1
 , l5LanguageShowable
 , l5Compiler
 , l5Compiler1
 , l5CompilerShowable
 , l5Interpreter
) where

import L.Compiler
import L.Primitives (X86)
import L.L4.L4
import L.L4.L4AST
import L.L5.L5AST
import L.L5.L5Interp
import L.L5.LambdaLifter

l5Compiler1 :: Compiler1 L5 L4
l5Compiler1 = Compiler1 (\_ _ -> return . compileL5ToL4) "L5"

l5Compiler :: Compiler L5 X86
l5Compiler = unconstrain l5CompilerShowable

l5CompilerShowable :: Thrist (Show :=> Compiler1) L5 X86
l5CompilerShowable = Cons (Constrained l5Compiler1) l4CompilerShowable

l5Interpreter :: Interpreter L5
l5Interpreter = interpL5

l5Language1 :: Language1 L5 L4
l5Language1  = Language1 l5Compiler1 interpL5

l5Language :: Language L5 X86
l5Language = Cons l5Language1 l4Language

l5LanguageShowable :: Thrist (Show :=> Language1) L5 X86
l5LanguageShowable = Cons (Constrained l5Language1) l4LanguageShowable

compileL5ToL4 :: L5 -> L4
compileL5ToL4 = lambdaLift
