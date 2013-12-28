{-# LANGUAGE GADTs #-}

module LUnitTests (tests) where

import Test.Framework.Runners.Console
import Test.Framework.Providers.API
import Test.Framework.Providers.HUnit
import Test.HUnit

import Control.Applicative
import Control.Exception
import Data.List
import Data.String.Utils
import Data.Traversable
import System.IO.Unsafe

import L.IOHelpers
import L.L1.L1
import L.L1.L1Interp
import L.L2.Interference
import L.L2.Liveness
import L.L2.Spill
import L.Read

tests = do
  ts <- traverse tree tests_
  return $ testGroup "Main" ts

--tests_ = [l1Tests, livenessTests, interferenceTests, spillTests]
tests_ = [l1InterpreterTests]

testDir = "./test/test-fest/"

data TestDef = TestDef 
  { name :: String
  , dir  :: FilePath
  , inputFileExt  :: String
  , outputFileExt :: String
  , compute :: String -> String -> Assertion
  }

l1Tests = TestDef { 
  name = "L1" 
 ,dir  = testDir
 ,inputFileExt = "L1"
 ,outputFileExt = "S"
 ,compute = \r e -> strip (compileL1OrDie r) @?= (strip e)
}
l1InterpreterTests = TestDef {
  name = "L1 Interpreter"
-- ,dir = testDir ++ "1-test"
-- ,dir = testDir ++ "1-test/robby"
-- jin (2) robby (2)
 ,dir = "./L1TestFailures"
 ,inputFileExt = "L1"
 ,outputFileExt = "res"
 ,compute = \r e -> strip (interpL1OrDie r) @?= strip e
}
livenessTests = TestDef { 
  name = "Liveness"
 ,dir  = testDir ++ "liveness-test"
 ,inputFileExt  = "L2f"
 ,outputFileExt = "lres"
 ,compute = \r e -> sread (showLiveness $ runLiveness r) @?= sread e
}
interferenceTests = TestDef { 
  name = "Interference"
 ,dir  = testDir ++ "graph-test"
 ,inputFileExt  = "L2f" 
 ,outputFileExt = "gres"
 ,compute = \r e -> sread (show $ runInterference r) @?= sread e
}
spillTests = TestDef { 
  name = "Spill" 
 ,dir  = testDir ++ "spill-test"
 ,inputFileExt  = "L2f"
 ,outputFileExt = "sres"
 ,compute = \r e -> sread (spillTest r) @?= sread e
}

tree def = testGroup (name def) . fmap mkTest <$> testFiles where
  testFiles = getRecursiveContentsByExt (dir def) (inputFileExt def)
  mkTest file = testCase file $ do
    res <- readFile file
    exp <- readFile (changeExtension file $ outputFileExt def)
    compute def res exp

-- s <- runSpillMain_ file `catch` \(e :: SomeException) -> return $ CompilationUnit "hello there" "wat" (show e)
