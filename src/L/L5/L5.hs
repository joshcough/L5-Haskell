module L.L5.L5 (l5Language) where

import L.Compiler
import L.L4.L4
import L.L4.L4AST
import L.L5.L5AST
import L.L5.L5Interp
import L.L5.L5Parser
import L.L5.LambdaLifter

l5Language :: Language L5 L4
l5Language  = Language
  parseL5
  (\_ _ -> Right . compileL5ToL4)
  interpL5
  "L5"
  (Just l4Language)

compileL5ToL4 :: L5 -> L4
compileL5ToL4 = lambdaLift
