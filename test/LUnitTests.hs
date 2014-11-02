module LUnitTests (tests) where

import Test.Framework.Runners.Console
import Test.Framework.Providers.API
import Test.Framework.Providers.HUnit
import Test.HUnit

import Control.Applicative
import Control.Exception
import Control.Lens
import Data.Default
import Data.Traversable
import Debug.Trace
import Prelude hiding (sequence)
import System.Directory
import System.FilePath.Find hiding (extension)
import System.IO.Unsafe
import System.Info as Info
import System.FilePath.Lens

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
import L.L5.L5
import L.OS as OS
import L.Read
import L.Utils

tests = do
  ts <- traverse tree [l1Tests]
  return $ testGroup "Main" ts

quickTests = [l1Tests, l2Tests, l3Tests, l4Tests, l5Tests]

allTests = [
  lParsingTests,
  l1Tests,
  l2Tests,
  l2TurtlesTests,
  spillTests,
  l3Tests,
  l3TurtlesTests,
  l4Tests,
  l4TurtlesTests,
  l5Tests,
  l5TurtlesTests ]

opts :: CompilationOptions
opts = def & outputDir .~ Just "./tmp"

testDir = "./test/test-fest/"

runInterp lang file = runVal <$> interpretFile lang file

data TestDef = TestDef 
  { name :: String
  , dirs :: [FilePath]
  , inputFileSearch :: String
  , outputFileExt   :: Maybe String
  , compute :: FilePath -> Maybe FilePath -> Assertion
 }

lParsingTests = TestDef {
  name = "L Parsing Tests"
 ,dirs = ["test"]
 ,inputFileSearch = "*.L[0-5]"
 ,outputFileExt   = Nothing
 ,compute = \lFile _ -> do
   lCode <- readFile lFile
   sread lCode @?= (sread . show $ sread lCode)
}
spillTests = TestDef {
  name = "Spill"
 ,dirs = [testDir ++ "spill-test"]
 ,inputFileSearch = "*.L2f"
 ,outputFileExt   = Just "sres"
 ,compute = \spillFile (Just resFile) -> do
    s        <- readFile spillFile
    expected <- readFile resFile
    sread (spillTest s) @?= sread expected
}

l1Tests        = oneLevelTestDef l1Language ["test/x86-64-tests"]

l2Tests        = oneLevelTestDef l2Language [testDir ++ "2-test"]
l2TurtlesTests = turtlesTestDef  l2Language [testDir ++ "2-test"]

l3Tests        = oneLevelTestDef l3Language [testDir ++ "3-test"]
l3TurtlesTests = turtlesTestDef  l3Language [testDir ++ "3-test"]

l4_2010_Dir sub = testDir ++ "L4-tests-from-2010/" ++ sub
l4Dirs = [testDir ++ "4-test",
          l4_2010_Dir "mcglynn",
          l4_2010_Dir "shawger",
          --l4_2010_Dir "hartglass", -- 100% cpu
          l4_2010_Dir "kleinfindler",
          l4_2010_Dir "burgener"]
l4Tests        = oneLevelTestDef l4Language l4Dirs
l4TurtlesTests = turtlesTestDef  l4Language l4Dirs

l5_2010_Dir sub = testDir ++ "L5-tests-from-2010/" ++ sub
l5Dirs = [--l5_2010_Dir "burgener",
          --l5_2010_Dir "mcglynn", -- 100% cpu
          --l5_2010_Dir "hartglass",
          l5_2010_Dir "kleinfindler"]--,
          --l5_2010_Dir "shawger"]
l5Tests        = oneLevelTestDef l5Language l5Dirs
l5TurtlesTests = turtlesTestDef  l5Language l5Dirs

oneLevelTestDef :: Language i o -> [FilePath] -> TestDef
oneLevelTestDef lang dirs' = langTestDef lang "One Level" dirs' runAndCompareInterpVsNativeVsRes

turtlesTestDef :: Language i o -> [FilePath] -> TestDef
turtlesTestDef lang dirs' = langTestDef lang "Turtles" dirs' runAndCompareInterpTurtlesVsNative

langTestDef :: Language i o  ->
               String        ->
               [FilePath]    ->
               (Language i o -> FilePath -> FilePath -> IO ()) ->
               TestDef
langTestDef lang name dirs' runFunction = TestDef {
  name = ext lang ++ " " ++ name
 ,dirs  = dirs'
 ,inputFileSearch = "*." ++ ext lang
 ,outputFileExt   = Just "res"
 ,compute = \inputFile (Just resFile) -> runFunction lang inputFile resFile
}

runAndCompareInterpVsNativeVsRes :: Language i o -> FilePath -> FilePath -> IO ()
runAndCompareInterpVsNativeVsRes lang inputFile resFile = do
  nativeRes     <- runVal <$> compileFileAndRunNative lang opts inputFile
  interpRes     <- runInterp lang inputFile
  resFileExists <- doesFileExist resFile
  expectedRes   <- if resFileExists then strip <$> readFile resFile else return "nope"
  let resultList = fmap strip $ [nativeRes, interpRes] ++ if resFileExists then [expectedRes] else []
  assertList resultList

runAndCompareInterpTurtlesVsNative :: Language i o -> FilePath -> FilePath -> IO ()
runAndCompareInterpTurtlesVsNative lang inputFile resFile = do
  nativeRes     <- Right . strip . runVal <$> compileFileAndRunNative lang opts inputFile
  interpResList <- (fmap $ fmap strip) <$> (interpretTurtlesFile lang opts inputFile)
  resFileExists <- doesFileExist resFile
  expectedRes   <- if resFileExists then strip <$> readFile resFile else return "nope"
  let resultList = nativeRes : interpResList ++ if resFileExists then [Right expectedRes] else []
  assertList $ resultList

tree def = testGroup (name def) . fmap mkTest <$> testFiles where
  findFiles :: FilePath -> IO [FilePath]
  findFiles dir =
    find always (fileType ==? RegularFile &&? fileName ~~? (inputFileSearch def)) dir
  testFiles :: IO [FilePath]
  testFiles = concat <$> sequence (findFiles <$> dirs def)
  mkTest inputFile = testCase (name def ++ " " ++ inputFile) $ do
    compute def inputFile (extension (const (outputFileExt def)) inputFile)

assertList :: (Eq a, Show a) => [a] -> IO ()
assertList (x1:x2:xs) = (x1 @?= x2) >> assertList (x2:xs)
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
