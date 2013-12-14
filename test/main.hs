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

main = defaultMain $ testGroup "Main" $ (unsafePerformIO . sequence) allTests 

allTests = [l1Tests, livenessTests, interferenceTests, spillTests]

testDir = "./test/test-fest/"

data TestDef a = TestDef {
  name :: String,
  dir  :: FilePath,
  inputFileExt  :: String,
  outputFileExt :: String,
  computeExpected :: String -> a,
  computeResult   :: String -> a
}

l1Tests = tree $ TestDef { 
  name = "L1", 
  dir  = testDir,
  inputFileExt = "L1",
  outputFileExt = "S",
  computeResult = compileL1OrDie,
  computeExpected = id
}
livenessTests = tree $ TestDef { 
  name = "Liveness", 
  dir  = testDir ++ "liveness-test", 
  inputFileExt  = "L2f", 
  outputFileExt = "lres",
  computeResult = sread . showLiveness . runLiveness, 
  computeExpected = sread
}
interferenceTests = tree $ TestDef { 
  name = "Interference", 
  dir  = testDir ++ "graph-test", 
  inputFileExt  = "L2f", 
  outputFileExt = "gres",
  computeResult = sread . show . runInterference, 
  computeExpected = sread
}
spillTests = tree $ TestDef { 
  name = "Spill", 
  dir  = testDir ++ "spill-test", 
  inputFileExt  = "L2f", 
  outputFileExt = "sres",
  computeResult = sread . spillTest, 
  computeExpected = sread
}

tree :: (Eq a, Show a) => TestDef a -> IO TestTree
tree def = 
  let testsFromFiles :: [FilePath] -> (FilePath -> Assertion) -> [TestTree]
      testsFromFiles files f = fmap (\file -> testCase file $ f file) files
      files = getRecursiveContentsByExt (dir def) (inputFileExt def)
      mkTests fs = testsFromFiles fs $ \file -> do
        res <- fmap (computeResult   def) $ readFile file
        exp <- fmap (computeExpected def) $ readFile (changeExtension file (outputFileExt def))
        res @?= exp
  in testGroup (name def) . mkTests <$> files

-- s <- runSpillMain_ file `catch` \(e :: SomeException) -> return $ CompilationUnit "hello there" "wat" (show e)
