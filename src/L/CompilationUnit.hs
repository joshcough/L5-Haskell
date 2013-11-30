module L.CompilationUnit
  (
    CompilationUnit(..)
   ,compile
   ,compile_
   ,compile1
   ,fromOutputFile
   ,readOutputFile
   ,writeOutputFile
  ) where

import Control.Applicative
import L.IOHelpers
import System.Environment

data CompilationUnit = CompilationUnit {
  inputFile :: FilePath, 
  outputExt :: String, 
  outputContents :: String
} deriving (Eq, Show)

-- build a CompilationUnit post compilation
fromOutputFile :: FilePath -> String -> IO CompilationUnit
fromOutputFile input ext = 
  CompilationUnit <$> return input <*> return ext <*> readFile (changeExtension ext input)

outputFile :: CompilationUnit -> String
outputFile c = changeExtension (inputFile c) (outputExt c)

readOutputFile :: CompilationUnit -> IO String
readOutputFile = readFile . outputFile

writeOutputFile :: CompilationUnit -> IO ()
writeOutputFile c = writeFile (outputFile c) (outputContents c)

-- just read the first file here. i suppose later on i could compile many files...
-- compile that file, and write it's result out
compile :: (String -> Either String String) -> String -> IO ()
compile compileFunction newExt = compile_ compileFunction newExt >>= writeOutputFile

-- read input file from fist command line arg, and compile it to a CompilationUnit
compile_ :: (String -> Either String String) -> String -> IO CompilationUnit
compile_ compileFunction ext = fmap (!! 0) getArgs >>= compile1 compileFunction ext where

-- read the given input file, and compile it to a CompilationUnit
compile1 :: (String -> Either String String) -> String -> FilePath -> IO CompilationUnit
compile1 compileFunction ext inputFile =
  let result = fmap ((either error id) . compileFunction) (readFile inputFile)
  in CompilationUnit <$> return inputFile <*> return ext <*> result

