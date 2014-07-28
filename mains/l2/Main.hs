module Main (main) where

import L.Compiler
import L.IOHelpers
import L.L2.L2 

main = withFileArg $ \f -> 
  compileFileAndWriteResult l2Language (getDir f) f >> return ()
