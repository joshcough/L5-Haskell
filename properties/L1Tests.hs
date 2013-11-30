{-# LANGUAGE TemplateHaskell #-}

module L1Tests where

import Control.Applicative
import Control.Monad hiding (sequence)
import Data.List (isSuffixOf)
import Data.Traversable
import Prelude hiding (sequence)
import Prelude.Extras
import Test.QuickCheck
import Test.QuickCheck.Function
import Test.QuickCheck.Instances
import Test.QuickCheck.Monadic (assert, monadicIO, pick, pre, run)
import Test.Framework.TH
import Test.Framework.Providers.QuickCheck2
import System.Directory (removeFile)
import System.IO (hGetContents, hPutStr, hSeek, openBinaryTempFile, SeekMode (..))
import L.CompilationUnit
import L.IOHelpers
import L.L1.L1

--http://stackoverflow.com/questions/2259926/testing-io-actions-with-monadic-quickcheck

testDir = "./test/test-fest/"

endsWith :: String -> String -> Bool
endsWith = isSuffixOf

isL1File = endsWith ".L1"

l1Files :: IO [FilePath]
l1Files = getRecursiveContents testDir >>= (return . take 20 . filter isL1File)

compileL1Files :: [FilePath] -> IO [CompilationUnit]
compileL1Files fs = sequence $ fmap compileL1File_ fs

compileL1Files_ :: IO [CompilationUnit]
compileL1Files_ = l1Files >>= compileL1Files

readPrecompiledFiles :: [CompilationUnit] -> IO [String]
readPrecompiledFiles cs = sequence $ fmap (\c -> readOutputFile c) cs

outputsEq :: [CompilationUnit] -> [String] -> [(CompilationUnit, Bool)]
outputsEq cs outputs = fmap outputEq $ zip cs outputs where
  outputEq :: (CompilationUnit, String) -> (CompilationUnit, Bool)
  outputEq (c, s) = (c, outputContents c == s)

allEq :: IO [(CompilationUnit, Bool)]
allEq = do
  actual   <- compileL1Files_
  expected <- readPrecompiledFiles actual
  return $ outputsEq actual expected

prop_fst :: Property
prop_fst = monadicIO $ go where
  go = do _ <- run (putStrLn "yes!")
          assert True

prop_listFiles :: Property
prop_listFiles = monadicIO $ go where
  go = do bs <- run (allEq >>= return . fmap snd >>= return . all id)
          assert bs

tests = $testGroupGenerator

-- problems:
-- too many open files
-- i want to know individually which cases failed, but i'm not sure how to do that yet
-- i only want to run these tests ones for all the L1 files I want to compile, not N times each. 
