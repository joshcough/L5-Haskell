{-# LANGUAGE TemplateHaskell #-}

--http://stackoverflow.com/questions/2259926/testing-io-actions-with-monadic-quickcheck
module L1Tests where

import Control.Applicative
import Control.Exception hiding (assert)
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
import System.Directory (doesFileExist, removeFile)
import System.IO (hGetContents, hPutStr, hSeek, openBinaryTempFile, SeekMode (..))
import System.IO.Unsafe (unsafePerformIO)
import L.CompilationUnit
import L.IOHelpers
import L.L1.L1

{--
testDir = "./test/test-fest/"

endsWith :: String -> String -> Bool
endsWith = isSuffixOf

isL1File = endsWith ".L1"

l1Files :: IO [FilePath]
l1Files = getRecursiveContents testDir >>= (return . take 100 . filter isL1File)
--l1Files = getRecursiveContents testDir >>= (return . filter isL1File) 

compileL1Files :: [FilePath] -> IO [CompilationUnit]
--compileL1Files fs = (sequence $ fmap compileL1File_ fs) >>= evaluate
compileL1Files fs = sequence $ fmap compileL1File_ fs

compileL1Files_ :: IO [CompilationUnit]
compileL1Files_ = l1Files >>= compileL1Files

readExpectedResultFiles :: [CompilationUnit] -> IO [String]
readExpectedResultFiles cs = sequence $ fmap (\c -> readOutputFile c) cs

flag          = ".flag.L1"
mkFlag        = putStrLn "touching flag" >> touch flag
doesFlagExist = doesFileExist flag
killFlag      = do 
  b <- doesFlagExist
  if b then putStrLn "no flag" else putStrLn "killing flag" >> removeFile flag

allEq :: IO Bool
allEq = allEq_ >>= return . fmap snd >>= return . all id where
  allEq_ :: IO [(CompilationUnit, Bool)]
  allEq_ = do
    actual   <- compileL1Files_
    expected <- readExpectedResultFiles actual
    return $ outputsEq actual expected
  outputsEq :: [CompilationUnit] -> [String] -> [(CompilationUnit, Bool)]
  outputsEq cs outputs = fmap outputEq $ zip cs outputs
  outputEq :: (CompilationUnit, String) -> (CompilationUnit, Bool)
  outputEq (c, s) = unsafePerformIO $ evaluate (c, outputContents c == s)

--prop_del_flag :: Property
--prop_del_flag = monadicIO $ go where
--  go = do _ <- run killFlag
--          assert True

--prop_compileL1Files :: Property
--prop_compileL1Files = monadicIO $ go where
--  go = do flg <- run $ doesFlagExist
--          bs  <- run $ if flg then putStrLn "skipping" >> return True else mkFlag >> allEq
--          assert bs
--}
tests = $testGroupGenerator

-- problems:
--   too many open files
--     check here: http://stackoverflow.com/questions/8716728/resource-exhausted-too-many-open-files
--   i want to know individually which cases failed, but i'm not sure how to do that yet
--   i only want to run these tests ones for all the L1 files I want to compile, not N times each. 
