{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import Test.Tasty
import Test.Tasty.HUnit

import Control.Applicative
import Control.Exception
import Data.List
import System.IO.Unsafe

import L.IOHelpers
import L.L1.L1
import L.L2.Interference
import L.L2.Liveness
import L.L2.Spill
import L.Read

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
tree def = testGroup (name def) . fmap mkTest <$> testFiles where
  testFiles = getRecursiveContentsByExt (dir def) (inputFileExt def)
  mkTest file = testCase file $ do
    res <- fmap (computeResult   def) $ readFile file
    exp <- fmap (computeExpected def) $ readFile (changeExtension file $ outputFileExt def)
    res @?= exp

-- s <- runSpillMain_ file `catch` \(e :: SomeException) -> return $ CompilationUnit "hello there" "wat" (show e)
