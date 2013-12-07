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
import L.Utils
import L.TestHelpers
import L.L2.Interference
import L.L2.Liveness

tests :: IO [Test]
tests = mkTests [compileL1Files, livenessTests, interferenceTests]

testDir = "./test/test-fest/"

l1Files = getRecursiveContentsByExt testDir ".L1" 
compileL1Files = testsFromFiles l1Files $ \file -> do
  actual   <- compileL1File_ file
  expected <- readOutputFile actual
  return $ assertEqual (result actual) expected

livenessDir   = testDir ++ "liveness-test"
livenessFiles = getRecursiveContentsByExt livenessDir ".L2f"
livenessTests = testsFromFiles livenessFiles $ \file -> do
  live     <- livenessMain_ file
  expected <- fmap sread $ readOutputFile live
  return $ assertEqual ((sread . showLiveness . result) live) expected

interferenceDir   = testDir ++ "graph-test"
interferenceFiles = getRecursiveContentsByExt interferenceDir ".L2f"
interferenceTests = testsFromFiles interferenceFiles $ \file -> do
  graph    <- runInterferenceMain file
  expected <- fmap sread $ readOutputFile graph
  return $ assertEqual ((sread . show . result) graph) expected
