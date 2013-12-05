module L.TestHelpers where

import Control.Applicative
import Control.Monad hiding (sequence)
import Data.List
import Data.Traversable
import Distribution.TestSuite
import Prelude hiding (sequence)
import System.Directory (doesFileExist, removeFile)
import L.CompilationUnit
import L.IOHelpers
import L.L1.L1

mkTests_ :: IO [TestInstance] -> IO [Test]
mkTests_ ts = fmap (fmap Test) ts

mkTests :: [IO [TestInstance]] -> IO [Test]
mkTests = mkTests_ . fmap join . sequence

mkTest :: String -> IO Progress -> TestInstance
mkTest testName runF = TestInstance
  { run       = runF
  , name      = testName
  , tags      = []
  , options   = []
  , setOption = \_ _ -> Right $ mkTest testName runF
  }

finish :: Bool -> String -> Progress 
finish b failureMsg = if b then (Finished Pass) else failWith failureMsg
failWith :: String -> Progress
failWith msg = Finished $ Fail msg

