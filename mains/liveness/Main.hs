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

parseIList = parseInstructionList l2Parser

main = fmap (!! 0) getArgs >>= runLiveness where
  runLiveness inputFile =
    fmap (showLiveness . liveness . extract . parseIList . sread) (readFile inputFile) >>= putStrLn . show

