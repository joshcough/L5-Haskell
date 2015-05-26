{-# LANGUAGE TypeOperators #-}

module L.L2.L2 (
   l2Language
 , l2Language1
 , l2LanguageShowable
 , l2Compiler
 , l2Compiler1
 , l2CompilerShowable
 , l2Interpreter
) where

import Control.Applicative
import L.Compiler
import L.L1.L1L2AST
import L.Primitives (X86)
import L.L1.L1
import L.L2.L2Interp
import L.L2.Allocation

-- L2 introduces variables on top of L1.
-- the L2 compiler is really just a register allocator.
-- for each function in the program, it tries to see if it can allocate it as is.
-- if so, great, assign variables to registers.
-- if it is unable to, it spills a variable, and tries again.
-- after that, it continuously tries to allocate the function.
-- it does this until either a) it works, or b) it is out of variables to spill.
-- the last case results in error.

l2Compiler1 :: Compiler1 L2 L1
l2Compiler1 = Compiler1 (\_ _ -> return . compileL2ToL1) "L2"

l2Compiler :: Compiler L2 X86
l2Compiler = unconstrain l2CompilerShowable

l2CompilerShowable :: Thrist (Show :=> Compiler1) L2 X86
l2CompilerShowable = Cons (Constrained l2Compiler1) l1CompilerShowable

l2Interpreter :: Interpreter L2
l2Interpreter = interpL2

l2Language1 :: Language1 L2 L1
l2Language1  = Language1 l2Compiler1 l2Interpreter

l2Language :: Language L2 X86
l2Language = Cons l2Language1 l1Language

l2LanguageShowable :: Thrist (Show :=> Language1) L2 X86
l2LanguageShowable = Cons (Constrained l2Language1) l1LanguageShowable

-- this is the main function, the rest are just various helpers
compileL2ToL1 :: L2 -> L1
compileL2ToL1 (L2 (Program main fs)) =
  L1 $ Program (allocate mainWithRet) $ (allocate <$> fs) where 
  mainWithRet = Func (body main ++ [Return])

