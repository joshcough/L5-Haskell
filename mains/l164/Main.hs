module Main (main) where

import L.L1.L1 (compileL1OrDie)
import L.CompilationUnit

main = compileL1File

compileL1File :: IO ()
compileL1File = compile (compileL1OrDie True) "S"