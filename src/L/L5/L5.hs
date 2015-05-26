{-# LANGUAGE TypeOperators #-}

module L.L5.L5 (
   l5Language
 , l5Language1
 , l5Compiler
 , l5Compiler1
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
l5Compiler = Cons (Constrained l5Compiler1) l4Compiler

l5Interpreter :: Interpreter L5
l5Interpreter = interpL5

l5Language1 :: Language1 L5 L4
l5Language1  = Language1 l5Compiler1 l5Interpreter

l5Language :: Language L5 X86
l5Language = Cons (Constrained l5Language1) l4Language

compileL5ToL4 :: L5 -> L4
compileL5ToL4 = lambdaLift
