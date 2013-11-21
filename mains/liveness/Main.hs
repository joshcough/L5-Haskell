module Main (main) where

import Data.List
import Data.Traversable
import L.Read
import L.L1L2AST
import L.L1L2Parser
import L.IOHelpers
import L.L2.L2
import L.L2.Liveness
import System.Environment 
import System.IO

main = fmap (!! 0) getArgs >>= runLiveness where
  runLiveness inputFile =
    fmap ((either error id) . parseL2 . sread) (readFile inputFile) >>= putStrLn . show

--parseL2 :: SExpr -> Either String L2
--parseL2 (sread code)
