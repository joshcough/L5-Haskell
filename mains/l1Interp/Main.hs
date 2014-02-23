module Main (main) where

import L.CompilationUnit (compile_)
import L.L1.L1Interp (interpL1String)

main = interpL1File

interpL1File =
  do s <- compile_ $ (either error id) . interpL1String
     putStrLn (snd s)