module LUnitTests (tests) where

import Test.Framework.Runners.Console
import Test.Framework.Providers.API
import Test.Framework.Providers.HUnit
import Test.HUnit

import Control.Applicative
import Control.Exception
import Data.List
import Data.Traversable
import Debug.Trace
import System.IO.Unsafe

import L.Compiler
import L.IOHelpers
import L.L1.L1
import L.L1.L1Interp
import L.L2.L2
import L.L2.Interference
import L.L2.Liveness
import L.L2.Spill
import L.L3.L3
import L.Read
import L.Utils

tests = do
  ts <- traverse tree tests_
  return $ testGroup "Main" ts

tests_ = [
  spillTests
 ,l1InterpreterTests
 ,l164Tests
 ,l2Tests
 ,l3Tests ]

testDir = "./test/test-fest/"

runInterp lang file = showComputerOutput . runVal <$> interpretFile lang file

data TestDef = TestDef 
  { name :: String
  , dir  :: FilePath
  , inputFileExt  :: String
  , outputFileExt :: String
  , compute :: (FilePath, String) -> (FilePath, String) -> Assertion
  }

l1InterpreterTests = TestDef {
  name = "L1 Interpreter"
 ,dir = "test/x86-64-tests"
 ,inputFileExt  = "L1"
 ,outputFileExt = "res"
 ,compute = \(l1f,_) (_,e) -> do
   interpRes <- runInterp l1Language l1f
   strip interpRes @?= strip e
}
l164Tests = TestDef { 
  name = "L1" 
 ,dir  = "test/x86-64-tests"
 ,inputFileExt  = "L1"
 ,outputFileExt = "res"
 ,compute = \(r,_) (_,e) -> do 
   res <- runVal <$> compileAndRunNativeFile l1Language "tmp" r
   strip res @?= strip e
}
-- Liveness tests are somewhat useless after moving to x86-64
-- since they lack all the registers.
-- TODO: update at least my tests
livenessTests = TestDef { 
  name = "Liveness"
 ,dir  = testDir ++ "liveness-test/cough"
 ,inputFileExt  = "L2f"
 ,outputFileExt = "lres"
 ,compute = \(_,r) (_,e) -> sread (showLiveness $ runLiveness r) @?= sread e
}
-- TODO: Interference suffer the same problem as liveness tests.
interferenceTests = TestDef { 
  name = "Interference"
 ,dir  = testDir ++ "graph-test/cough"
 ,inputFileExt  = "L2f" 
 ,outputFileExt = "gres"
 ,compute = \(_,r) (_,e) -> sread (show $ runInterference r) @?= sread e
}
-- TODO: most spill tests are not currently running :(
-- robby's tests make cabal never return.
spillTests = TestDef { 
  name = "Spill" 
 ,dir  = testDir ++ "spill-test/cough"
 ,inputFileExt  = "L2f"
 ,outputFileExt = "sres"
 ,compute = \(_,r) (_,e) -> sread (spillTest r) @?= sread e
}
l2Tests = TestDef {
  name = "L2"
 ,dir  = testDir ++ "2-test"
 ,inputFileExt  = "L2"
 ,outputFileExt = "L2" -- this isn't actually used
 ,compute = \(l2f,l2) _ -> do
    nativeRes <- runVal <$> compileAndRunNativeFile l2Language "tmp" l2f
    interpRes <- runInterp l2Language l2f
    strip nativeRes @?= strip interpRes
}
l3Tests = TestDef {
  name = "L3"
 --,dir  = testDir ++ "3-test/cough"
 ,dir  = testDir ++ "3-test"
 ,inputFileExt  = "L3"
 ,outputFileExt = "L3" -- this isn't actually used
 ,compute = \(l3f,l3) _ -> do
    nativeRes <- runVal <$> compileAndRunNativeFile l3Language "tmp" l3f
    interpRes <- runInterp l3Language l3f
    strip nativeRes @?= strip interpRes
}

tree def = testGroup (name def) . fmap mkTest <$> testFiles where
  testFiles = getRecursiveContentsByExt (dir def) (inputFileExt def)
  mkTest inputFile = testCase inputFile $ do
    inputContents  <- readFile inputFile
    outputContents <- readFile outputFile
    compute def (inputFile,inputContents) (outputFile,outputContents) where
    outputFile = changeExtension inputFile $ outputFileExt def

-- s <- runSpillMain_ file `catch` \(e :: SomeException) -> return $ CompilationUnit "hello there" "wat" (show e)
