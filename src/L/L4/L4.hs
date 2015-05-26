{-# LANGUAGE TypeOperators #-}

module L.L4.L4 (
   l4Language
 , l4Language1
 , l4Compiler
 , l4Compiler1
 , l4Interpreter
) where

import L.Compiler
import L.L3.L3AST
import L.L3.L3
import L.L4.ANormalize
import L.L4.L4AST
import L.L4.L4Interp

l4Compiler1 :: Compiler1 L4 L3
l4Compiler1 = Compiler1 (\_ _ -> return . compileL4ToL3) "L4"

l4Compiler :: Compiler L4 X86
l4Compiler = Cons (Constrained l4Compiler1) l3Compiler

l4Interpreter :: Interpreter L4
l4Interpreter = interpL4

l4Language1 :: Language1 L4 L3
l4Language1  = Language1 l4Compiler1 l4Interpreter

l4Language :: Language L4 X86
l4Language = Cons (Constrained l4Language1) l3Language

compileL4ToL3 :: L4 -> L3
compileL4ToL3 = aNormalize
