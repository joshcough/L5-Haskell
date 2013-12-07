module LTests (tests) where

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
import L.Read
import L.TestHelpers
import L.L2.Liveness

tests :: IO [Test]
tests = mkTests [compileL1Files, livenessTests]

endsWith :: String -> String -> Bool
endsWith = isSuffixOf
testDir = "./test/test-fest/"
livenessDir = testDir ++ "liveness-test"
getTestFiles dir ext = 
  getRecursiveContents dir >>= (return . filter (endsWith ext))
l1Files :: IO [FilePath]
l1Files = getTestFiles testDir ".L1" 
livenessFiles = getTestFiles livenessDir ".L2f"

compileL1Files :: IO [TestInstance]
compileL1Files = fmap (fmap mkL1Test) l1Files where
  mkL1Test :: FilePath -> TestInstance
  mkL1Test file = mkTest file $ go file
  go :: FilePath -> IO Progress
  go file = do
    actual   <- compileL1File_ file
    expected <- readOutputFile actual
    return $ assertEqual (result actual) expected

livenessTests :: IO [TestInstance]
livenessTests = fmap (fmap mkLivenessTest) livenessFiles where
  mkLivenessTest :: FilePath -> TestInstance
  mkLivenessTest file = mkTest file $ go file
  go :: FilePath -> IO Progress
  go file = do
    live     <- livenessMain_ file
    expected <- fmap sread $ readOutputFile live
    return $ assertEqual ((sread . showLiveness . result) live) expected
