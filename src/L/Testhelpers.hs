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

mkTests :: IO [TestInstance] -> IO [Test]
mkTests ts = fmap (fmap Test) ts

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

