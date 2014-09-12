module Main where

import L.Compiler
import L.IOHelpers
import L.L1.L1
import L.L1.L1Interp
import L.L2.L2
import L.L3.L3

main = withFileArg $ \file -> g (getExtension file) file where
  go lang file = 
    interpretFile lang file >>= putStrLn . showComputerOutput . runVal
  g "L1" = go l1Language
  g "L2" = go l2Language
  g "L3" = go l3Language
