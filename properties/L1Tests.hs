{-# LANGUAGE TemplateHaskell #-}

module L1Tests where

import Control.Applicative
import Control.Monad
import Data.List (isSuffixOf)
import Prelude.Extras
import Test.QuickCheck
import Test.QuickCheck.Function
import Test.QuickCheck.Instances
import Test.QuickCheck.Monadic (assert, monadicIO, pick, pre, run)
import Test.Framework.TH
import Test.Framework.Providers.QuickCheck2
import System.Directory (removeFile)
import System.IO (hGetContents, hPutStr, hSeek, openBinaryTempFile, SeekMode (..))
import L.IOHelpers

--http://stackoverflow.com/questions/2259926/testing-io-actions-with-monadic-quickcheck

testDir = "../scala/src/test/compilers/test-fest/"

--prop_writeThenRead :: Property
--prop_writeThenRead = monadicIO $ do writtenData <- pick arbitrary
--                                    pre $ not (null writtenData)
--                                    readData <- run $ writeThenRead writtenData
--                                    assert $ writtenData == readData

--prop_writeThenRead2 :: [Char] -> Property
--prop_writeThenRead2 writtenData = not (null writtenData) ==> monadicIO test
--    where test = do readData <- run $ writeThenRead writtenData
--                    assert $ writtenData == readData

--getRecursiveContents :: FilePath -> IO [FilePath]

writeThenRead :: String -> IO String
writeThenRead output = do (path, h) <- openBinaryTempFile "/tmp" "quickcheck.tmp"
                          removeFile path
                          hPutStr h output
                          hSeek h AbsoluteSeek 0
                          hGetContents h

endsWith :: String -> String -> Bool
endsWith = isSuffixOf

isL1File = endsWith ".L1"

prop_listFiles :: Property
prop_listFiles = monadicIO $ go where
  go = do _ <- run (getRecursiveContents testDir >>= putList . filter isL1File)
          assert True

tests = $testGroupGenerator
