module L.IOHelpers
  (
    mapFileContents
   ,withFileArg
  ) where

import System.Environment

withFileArg :: (FilePath -> IO a) -> IO a
withFileArg f = fmap (!! 0) getArgs >>= f

-- read the given input file, and compile it
mapFileContents :: (String -> a) -> FilePath -> IO a
mapFileContents f inputFile =
  do s <- readFile inputFile
     length s `seq` return (f s)
