{-# LANGUAGE GADTs #-}

module LUnitTests (tests) where

import Test.Framework.Runners.Console
import Test.Framework.Providers.API
import Test.Framework.Providers.HUnit
import Test.HUnit

import Control.Applicative
import Control.Exception
import Data.List
import Data.Traversable
import System.IO.Unsafe

import L.IOHelpers
import L.L1.L1
import L.L1.L1Interp
import L.L2.L2
import L.L2.Interference
import L.L2.Liveness
import L.L2.Spill
import L.Read
import L.Utils

tests = do
  ts <- traverse tree tests_
  return $ testGroup "Main" ts

tests_ = [
  spillTests
 ,l1InterpreterTests
 ,l164Tests
--  livenessTests
-- ,interferenceTests
 ,l2Tests ]

testDir = "./test/test-fest/"

data TestDef = TestDef 
  { name :: String
  , dir  :: FilePath
  , inputFileExt  :: String
  , outputFileExt :: String
  , compute :: FilePath -> String -> String -> Assertion
  }

l1InterpreterTests = TestDef {
  name = "L1 Interpreter"
 ,dir = "test/x86-64-tests"
 ,inputFileExt = "L1"
 ,outputFileExt = "res"
 ,compute = \_ r e -> strip (interpL1StringOrDie r) @?= strip e
}
l164Tests = TestDef { 
  name = "L1" 
 ,dir  = "test/x86-64-tests"
 ,inputFileExt = "L1"
 ,outputFileExt = "res"
 ,compute = \r _ e -> do 
   res <- compileL1FileAndRunNative r "tmp"
   strip res @?= strip e
}
livenessTests = TestDef { 
  name = "Liveness"
 ,dir  = testDir ++ "liveness-test"
 ,inputFileExt  = "L2f"
 ,outputFileExt = "lres"
 ,compute = \_ r e -> sread (showLiveness $ runLiveness r) @?= sread e
}
interferenceTests = TestDef { 
  name = "Interference"
 ,dir  = testDir ++ "graph-test"
 ,inputFileExt  = "L2f" 
 ,outputFileExt = "gres"
 ,compute = \_ r e -> sread (show $ runInterference r) @?= sread e
}
spillTests = TestDef { 
  name = "Spill" 
 ,dir  = testDir ++ "spill-test"
 ,inputFileExt  = "L2f"
 ,outputFileExt = "sres"
 ,compute = \_ r e -> sread (spillTest r) @?= sread e
}
l2Tests = TestDef {
  name = "L2"
 ,dir  = testDir ++ "2-test"
 ,inputFileExt = "L2"
 ,outputFileExt = "L2" -- this isn't actually used
 ,compute = \l2f l2 _->
   let l1 = compileL2OrDie l2
       interpRes = interpL1OrDie l1
   in do
        _      <- writeFile (changeExtension l2f "L1") (show l1)
        x86res <- compileL1AndRunNative l1 (Just l2f) "tmp"
        strip x86res @?= strip interpRes
}

tree def = testGroup (name def) . fmap mkTest <$> testFiles where
  testFiles = getRecursiveContentsByExt (dir def) (inputFileExt def)
  mkTest file = testCase file $ do
    res <- readFile file
    exp <- readFile (changeExtension file $ outputFileExt def)
    compute def file res exp

-- s <- runSpillMain_ file `catch` \(e :: SomeException) -> return $ CompilationUnit "hello there" "wat" (show e)
