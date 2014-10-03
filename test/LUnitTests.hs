module LUnitTests (tests) where

import Test.Framework.Runners.Console
import Test.Framework.Providers.API
import Test.Framework.Providers.HUnit
import Test.HUnit

import Control.Applicative
import Control.Exception
import Data.Traversable
import Debug.Trace
import System.FilePath.Find
import System.IO.Unsafe
import System.Info as Info

import L.Compiler
import L.IOHelpers
import L.L1.L1
import L.L1.L1Interp
import L.L2.L2
import L.L2.Interference
import L.L2.Liveness
import L.L2.Spill
import L.L3.L3
import L.L4.L4
import L.OS as OS
import L.Read
import L.Utils

tests = do
  ts <- traverse tree allTests
  return $ testGroup "Main" ts

allTests = [
  spillTests
 ,lParsingTests
 ,l1InterpreterTests
 ,l164Tests
 ,l2Tests 
 ,l3Tests ]
 --,l4Tests ]

opts = CompilationOptions { L.Compiler.os = OS.osFromString Info.os }

testDir = "./test/test-fest/"

runInterp lang file = runVal <$> interpretFile lang file

data TestDef = TestDef 
  { name :: String
  , dir  :: FilePath
  , inputFileSearch :: String
  , outputFileExt   :: Maybe String
  , compute :: FilePath -> Maybe FilePath -> Assertion
 }

lParsingTests = TestDef {
  name = "L Parsing Tests"
 ,dir  = "test"
 ,inputFileSearch = "*.L[0-4]"
 ,outputFileExt   = Nothing
 ,compute = \lFile _ -> do 
   lCode <- readFile lFile
   sread lCode @?= (sread . show $ sread lCode)
}
l1InterpreterTests = TestDef {
  name = "L1 Interpreter"
 ,dir = "test/x86-64-tests"
 ,inputFileSearch = "*.L1"
 ,outputFileExt   = Just "res"
 ,compute = \l1f (Just resFile) -> do
   actual   <- runInterp l1Language l1f
   expected <- readFile resFile
   strip actual @?= strip expected
}
l164Tests = TestDef { 
  name = "L1" 
 ,dir  = "test/x86-64-tests"
 ,inputFileSearch = "*.L1"
 ,outputFileExt   = Just "res"
 ,compute = \l1File (Just resFile) -> do 
   actual   <- runVal <$> compileAndRunNativeFile l1Language opts "tmp" l1File
   expected <- readFile resFile
   strip actual @?= strip expected
}
-- Liveness tests are somewhat useless after moving to x86-64
-- since they lack all the registers.
-- TODO: update at least my tests
livenessTests = TestDef { 
  name = "Liveness"
 ,dir  = testDir ++ "liveness-test/cough"
 ,inputFileSearch = "*.L2f"
 ,outputFileExt   = Just "lres"
 ,compute = \livenessFile (Just resFile) -> do
   l        <- readFile livenessFile
   expected <- readFile resFile
   sread (showLiveness $ runLiveness l) @?= sread expected
}
-- TODO: Interference suffer the same problem as liveness tests.
interferenceTests = TestDef { 
  name = "Interference"
 ,dir  = testDir ++ "graph-test/cough"
 ,inputFileSearch = "*.L2f" 
 ,outputFileExt   = Just "gres"
 ,compute = \interferenceFile (Just resFile) -> do
   i        <- readFile interferenceFile
   expected <- readFile resFile
   sread (show $ runInterference i) @?= sread expected
}
spillTests = TestDef {
  name = "Spill" 
 ,dir  = testDir ++ "spill-test"
 ,inputFileSearch = "*.L2f"
 ,outputFileExt   = Just "sres"
 ,compute = \spillFile (Just resFile) -> do
    s        <- readFile spillFile
    expected <- readFile resFile
    sread (spillTest s) @?= sread expected
}
l2Tests = TestDef {
  name = "L2"
 ,dir  = testDir ++ "2-test"
 ,inputFileSearch = "*.L2"
 ,outputFileExt   = Nothing
 ,compute = \l2f _ -> do
    l2        <- readFile l2f
    nativeRes <- runVal <$> compileAndRunNativeFile l2Language opts "tmp" l2f
    interpRes <- runInterp l2Language l2f
    strip nativeRes @?= strip interpRes
}
l3Tests = TestDef {
  name = "L3"
 ,dir  = testDir ++ "3-test"
 ,inputFileSearch = "*.L3"
 ,outputFileExt   = Nothing
 ,compute = \l3f _ -> do
    l3        <- readFile l3f
    nativeRes <- runVal <$> compileAndRunNativeFile l3Language opts "tmp" l3f
    interpRes <- runInterp l3Language l3f
    strip nativeRes @?= strip interpRes
}
l4Tests = TestDef {
  name = "L4"
 --,dir  = testDir ++ "L4-tests-from-2010/kleinfindler"
 ,dir  = testDir ++ "4-test"
 ,inputFileSearch = "*.L4"
 ,outputFileExt   = Nothing
 ,compute = \l4f _ -> do
    l4        <- readFile l4f
    nativeRes <- runVal <$> compileAndRunNativeFile l4Language opts "tmp" l4f
    interpRes <- runInterp l4Language l4f
    strip nativeRes @?= strip interpRes
}

tree def = testGroup (name def) . fmap mkTest <$> testFiles (inputFileSearch def) where
  testFiles :: String -> IO [FilePath]
  testFiles rgx = find always (fileType ==? RegularFile &&? fileName ~~? rgx) (dir def)
  mkTest inputFile = testCase inputFile $ do
    compute def inputFile (changeExt inputFile <$> outputFileExt def)

-- s <- runSpillMain_ file `catch` \(e :: SomeException) -> return $ CompilationUnit "hello there" "wat" (show e)
