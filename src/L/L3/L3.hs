module L.L3.L3 (l3Language, interpL3) where

import L.Compiler
import L.L1L2AST
import L.L2.L2 
import L.L3.L3AST
import L.L3.Linearize
import L.L3.L3Parser

l3Language :: Language L3 L2
l3Language  = Language
  parseL3
  (\_ _ -> Right . compileL3ToL2)
  interpL3
  "L3"
  (Just l2Language)

compileL3ToL2 :: L3 -> L2
compileL3ToL2 = linearize

interpL3 :: L3 -> String
interpL3 = interpL2 . compileL3ToL2
