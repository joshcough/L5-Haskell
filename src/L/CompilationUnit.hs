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

data CompilationUnit a = CompilationUnit {
  inputFile :: FilePath, 
  outputExt :: String, 
  result    :: a
} deriving (Eq, Show)

instance Functor CompilationUnit where
  fmap f c = CompilationUnit (inputFile c) (outputExt c) $ f (result c)

-- build a CompilationUnit post compilation
fromOutputFile :: FilePath -> String -> IO (CompilationUnit String)
fromOutputFile input ext = 
  CompilationUnit <$> return input <*> return ext <*> readFile (changeExtension ext input)

outputFile :: CompilationUnit a -> String
outputFile c = changeExtension (inputFile c) (outputExt c)

readOutputFile :: CompilationUnit a -> IO String
readOutputFile = readFile . outputFile

writeOutputFile :: (Show a) => CompilationUnit a -> IO ()
writeOutputFile c = writeFile (outputFile c) (show $ result c)

-- just read the first file here. i suppose later on i could compile many files...
-- compile that file, and write it's result out
compile :: Show a => (String -> a) -> String -> IO ()
compile compileFunction newExt = compile_ compileFunction newExt >>= writeOutputFile

-- read input file from fist command line arg, and compile it to a CompilationUnit
compile_ :: (String -> a) -> String -> IO (CompilationUnit a)
compile_ compileFunction ext = fmap (!! 0) getArgs >>= compile1 compileFunction ext where

-- read the given input file, and compile it to a CompilationUnit
compile1 :: (String -> a) -> String -> FilePath -> IO (CompilationUnit a)
compile1 compileFunction ext inputFile =
  let result = fmap compileFunction (readFile inputFile)
  in CompilationUnit <$> return inputFile <*> return ext <*> result

