module LUnitTests (tests) where

import Test.Framework.Runners.Console
import Test.Framework.Providers.API
import Test.Framework.Providers.HUnit
import Test.HUnit

import Control.Applicative
import Control.Exception
import Data.Traversable
import Debug.Trace
import System.FilePath.Find hiding (extension)
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
  lParsingTests
 ,l1InterpreterTests
 ,l164Tests
 ,l2Tests
 ,l2TurtlesTests
 ,spillTests
 ,l3Tests
 ,l3TurtlesTests
 ,l4Tests
 ,l4TurtlesTests ]

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

l2Tests        = oneLevelTestDef l2Language (testDir ++ "2-test")
l2TurtlesTests = turtlesTestDef  l2Language (testDir ++ "2-test")

l3Tests        = oneLevelTestDef l3Language (testDir ++ "3-test")
l3TurtlesTests = turtlesTestDef  l3Language (testDir ++ "3-test")

l4Tests        = oneLevelTestDef l4Language (testDir ++ "4-test") -- testDir ++ "L4-tests-from-2010/kleinfindler"
l4TurtlesTests = turtlesTestDef  l4Language (testDir ++ "4-test")


oneLevelTestDef :: Language i o -> FilePath -> TestDef
oneLevelTestDef lang dir' = langTestDef lang "One Level" dir' runAndCompareInterpVsNative

turtlesTestDef :: Language i o -> FilePath -> TestDef
turtlesTestDef lang dir' = langTestDef lang "Turtles" dir' runAndCompareInterpTurtlesVsNative

langTestDef :: Language i o -> String -> FilePath -> (Language i o -> FilePath -> IO ()) -> TestDef
langTestDef lang name dir' runFunction = TestDef {
  name = extension lang ++ " " ++ name
 ,dir  = dir'
 ,inputFileSearch = "*." ++ (extension lang)
 ,outputFileExt   = Nothing
 ,compute = \file _ -> runFunction lang file
}

runAndCompareInterpVsNative :: Language i o -> FilePath -> IO ()
runAndCompareInterpVsNative lang inputFile = do
  nativeRes <- strip . runVal <$> compileAndRunNativeFile lang opts "tmp" inputFile
  interpRes <- strip  <$> runInterp lang inputFile
  nativeRes @?= interpRes

runAndCompareInterpTurtlesVsNative :: Language i o -> FilePath -> IO ()
runAndCompareInterpTurtlesVsNative lang inputFile = do
  nativeRes     <- Right . strip . runVal <$> compileAndRunNativeFile lang opts "tmp" inputFile
  interpResList <- (fmap (fmap strip)) <$> (interpretTurtlesFile lang opts inputFile)
  assertList $ nativeRes : interpResList

tree def = testGroup (name def) . fmap mkTest <$> testFiles (inputFileSearch def) where
  testFiles :: String -> IO [FilePath]
  testFiles rgx = find always (fileType ==? RegularFile &&? fileName ~~? rgx) (dir def)
  mkTest inputFile = testCase (name def ++ " " ++ inputFile) $ do
    compute def inputFile (changeExt inputFile <$> outputFileExt def)

assertList :: (Eq a, Show a) => [a] -> IO ()
assertList (x1:x2:xs) = (x1 @?= x2) >> assertList xs
assertList _          = return ()

{-
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
-}

-- s <- runSpillMain_ file `catch` \(e :: SomeException) -> return $ CompilationUnit "hello there" "wat" (show e)
