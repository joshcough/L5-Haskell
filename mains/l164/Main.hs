module Main (main) where

import L.Compiler
import L.IOHelpers
import L.L1.L1

main = compileL1File

compileL1File :: IO ()
compileL1File =
  readMapAndWriteFileArg (runVal . compileString l1Language) "S"
