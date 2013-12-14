{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import Test.Tasty
import Test.Tasty.HUnit

import Control.Applicative
import Control.Exception
import Data.IORef
import Data.List
import Data.Ord
import System.Environment
import System.IO
import System.IO.Unsafe
import System.Directory

import L.CompilationUnit
import L.IOHelpers
import L.L1.L1
import L.L2.Interference
import L.L2.Liveness
import L.L2.Spill
import L.Read
import L.Utils

testDir = "./test/test-fest/"

main = defaultMain $ testGroup "Main" $ (unsafePerformIO . fmap concat . sequence) allTests 

allTests = [l1Tests, livenessTests, interferenceTests, spillTests]

testsFromFiles :: [FilePath] -> (FilePath -> Assertion) -> [TestTree]
testsFromFiles files f = fmap (\file -> testCase file $ f file) files

l1Files = getRecursiveContentsByExt testDir ".L1" 
l1Tests = compile <$> l1Files where
  compile fs = testsFromFiles fs $ \file -> do
    actual   <- compileL1File_ file
    expected <- readOutputFile actual
    (result actual) @?= expected

livenessDir   = testDir ++ "liveness-test"
livenessFiles = getRecursiveContentsByExt livenessDir ".L2f"
livenessTests = compile <$> livenessFiles where
  compile fs = testsFromFiles fs $ \file -> do
    live     <- livenessMain_ file
    expected <- fmap sread $ readOutputFile live
    ((sread . showLiveness . result) live) @?= expected

interferenceDir   = testDir ++ "graph-test"
interferenceFiles = getRecursiveContentsByExt interferenceDir ".L2f"
interferenceTests = compile <$> interferenceFiles where
  compile fs = testsFromFiles fs $ \file -> do
    graph    <- runInterferenceMain_ file
    expected <- fmap sread $ readOutputFile graph
    ((sread . show . result) graph) @?= expected

spillDir   = testDir ++ "spill-test"
spillFiles = getRecursiveContentsByExt spillDir ".L2f"
spillTests = spill <$> spillFiles where
  spill fs = testsFromFiles fs $ \file -> do
    s        <- runSpillMain_ file `catch` \(e :: SomeException) -> return $ CompilationUnit "hello there" "wat"      (show e)
    expected <- fmap sread $ readOutputFile s
    ((sread . result) s) @?= expected
