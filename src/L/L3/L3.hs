module L.L3.L3 (l3Language) where

import L.Compiler
import L.IOHelpers
import L.L1L2AST
import L.L2.L2 
import L.L3.L3AST
import L.L3.Linearize
import L.L3.L3Parser

l3Language :: Language L3 L2
l3Language  = Language
  parseL3
  (Right . compileL3ToL2)
  (interpL2 . compileL3ToL2)
  runL2Native

runL2Native :: String -> FilePath -> L2 -> IO (Val String)
runL2Native name outputDir l2 = do
  _ <- writeFile l2File (show l2)
  compileAndRunNative l2Language name outputDir l2 where
  l2File = changeDir outputDir (name ++ ".L2")

compileL3ToL2 :: L3 -> L2
compileL3ToL2 = linearize
