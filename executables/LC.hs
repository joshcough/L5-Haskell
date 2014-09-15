module Main where

import L.CommandLine
import L.Compiler
import L.IOHelpers
import L.L1.L1
import L.L2.L2
import L.L3.L3
import Options.Applicative

main :: IO ()
main = execParser commandLineParser >>= main'

main' :: (CompilationOptions, FilePath) -> IO ()
main' (opts, file) = g (getExtension file) (getDir file) where
  go lang dir = compileFileAndWriteResult lang opts dir file >> return ()
  g "L1" = go l1Language
  g "L2" = go l2Language
  g "L3" = go l3Language

