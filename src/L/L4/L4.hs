module L.L4.L4 (l4Language) where

import L.Compiler
import L.L3.L3AST
import L.L3.L3 
import L.L4.L4AST
import L.L4.L4Parser

l4Language :: Language L4 L3
l4Language  = Language
  parseL4
  (Right . compileL4ToL3)
  (interpL3 . compileL4ToL3)
  "L4"
  (Just l3Language)

compileL4ToL3 :: L4 -> L3
compileL4ToL3 = undefined --aNormalize
