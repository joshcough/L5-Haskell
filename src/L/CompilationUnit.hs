module L.CompilationUnit
  (
    compile
   ,compile1
  ) where

import Control.Applicative
import L.IOHelpers
import System.Environment

-- just read the first file here. i suppose later on i could compile many files...
-- compile that file, and write it's result out
compile :: Show a => (String -> a) -> String -> IO ()
compile compileFunction newExt = do
  (f, a) <- compile_ compileFunction
  writeFile (changeExtension f newExt) (show a)

-- read input file from fist command line arg, and compile it 
compile_ :: (String -> a) -> IO (FilePath, a)
compile_ compileFunction = do 
  inputFile <- fmap (!! 0) getArgs
  a <- compile1 compileFunction inputFile
  return (inputFile, a)

-- read the given input file, and compile it
compile1 :: (String -> a) -> FilePath -> IO a
compile1 compileFunction inputFile = 
  do f <- readFile inputFile
     length f `seq` return (compileFunction f)

