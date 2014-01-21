module Main (main) where

import L.L1.L1 (runNative)
import L.Utils
import System.Environment

main = do
  inputFile <- fmap (!! 0) getArgs
  res <- runNative inputFile "tmp"
  putStrLn res
