module Main where

import L.Compiler
import L.IOHelpers
import L.L1.L1
import L.L2.L2
import L.L3.L3

import System.Info as Info

main = withFileArg $ \file -> g (getExtension file) (getDir file) file where
  go lang dir file = compileAndRunNativeFile lang opts dir file >>= putStrLn . runVal
  g "L1" = go l1Language
  g "L2" = go l2Language
  g "L3" = go l3Language
  opts = CompilationOptions { L.Compiler.os = Info.os }
