module L1Tests (tests) where

import Control.Applicative
import Control.Monad hiding (sequence)
import Data.List
import Data.Traversable
import Distribution.TestSuite
import Prelude hiding (sequence)
import System.Directory (doesFileExist, removeFile)
import L.CompilationUnit
import L.IOHelpers
import L.L1.L1
import L.TestHelpers

tests :: IO [Test]
tests = mkTests compileL1Files 

testDir = "./test/test-fest/"
endsWith :: String -> String -> Bool
endsWith = isSuffixOf
isL1File = endsWith ".L1"
l1Files :: IO [FilePath]
l1Files = getRecursiveContents testDir >>= (return . filter isL1File) 

compileL1Files :: IO [TestInstance]
compileL1Files = fmap (fmap mkL1Test) l1Files where
  mkL1Test :: FilePath -> TestInstance
  mkL1Test file = mkTest file $ go file
  go :: FilePath -> IO Progress
  go file = do
    actual   <- compileL1File_ file
    expected <- (readOutputFile actual)
    return $ finish (outputContents actual == expected) file

