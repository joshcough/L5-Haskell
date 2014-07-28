module Main (main) where

import L.IOHelpers
import L.NativeRunner
import L.Utils
import System.Environment

main = do
  inputFile <- fmap (!! 0) getArgs
  res <- runSCodeNative (getFileName inputFile) (getDir inputFile) "tmp"
  putStrLn res
