module Main (main) where

import Data.List
import Data.Traversable
import L.Read
import L.L1L2AST
import L.L1L2Parser
import L.IOHelpers
import L.L2.L2 (compileL2)
import System.Environment 
import System.IO

-- just read the first file here. i suppose later on i could compile many files...
main = fmap (!! 0) getArgs >>= compileToFile where
  compileToFile inputFile =
    let outputFile = changeExtension inputFile "L1" in
    fmap ((either error id) . compileL2) (readFile inputFile) >>= (writeFile outputFile . show)
