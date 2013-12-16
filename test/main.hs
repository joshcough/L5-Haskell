{-# LANGUAGE GADTs #-}

module Main where

import Test.Tasty
import Test.Tasty.HUnit

import Control.Applicative
import Control.Exception
import Data.List
import Data.Traversable
import System.IO.Unsafe

import L.IOHelpers
import L.L1.L1
import L.L2.Interference
import L.L2.Liveness
import L.L2.Spill
import L.Read

main = do
  tests <- traverse tree allTests
  defaultMain $ testGroup "Main" tests

allTests = [l1Tests, livenessTests, interferenceTests, spillTests]

testDir = "./test/test-fest/"

data TestDef = TestDef 
  { name :: String
  , dir  :: FilePath
  , inputFileExt  :: String
  , outputFileExt :: String
  , compute :: String -> String -> Assertion
  }

l1Tests = TestDef { 
  name = "L1", 
  dir  = testDir,
  inputFileExt = "L1",
  outputFileExt = "S",
  compute = \r e -> compileL1OrDie r @?= e
}
livenessTests = TestDef { 
  name = "Liveness", 
  dir  = testDir ++ "liveness-test", 
  inputFileExt  = "L2f", 
  outputFileExt = "lres",
  compute = \r e -> sread (showLiveness (runLiveness r)) @?= sread e
}
interferenceTests = TestDef { 
  name = "Interference", 
  dir  = testDir ++ "graph-test", 
  inputFileExt  = "L2f", 
  outputFileExt = "gres",
  compute = \r e -> sread (show (runInterference r)) @?= sread e
}
spillTests = TestDef { 
  name = "Spill", 
  dir  = testDir ++ "spill-test", 
  inputFileExt  = "L2f", 
  outputFileExt = "sres",
  compute = \r e -> sread (spillTest r) @?= sread e
}

tree :: TestDef -> IO TestTree
tree def = testGroup (name def) . fmap mkTest <$> testFiles where
  testFiles = getRecursiveContentsByExt (dir def) (inputFileExt def)
  mkTest file = testCase file $ do
    res <- readFile file
    exp <- readFile (changeExtension file $ outputFileExt def)
    compute def res exp

-- s <- runSpillMain_ file `catch` \(e :: SomeException) -> return $ CompilationUnit "hello there" "wat" (show e)
