module L.L1.L1 
  (
    compileL1
   ,compileL1OrDie
   ,compileL1File
   ,compileL1File_
  ) where

import Control.Monad.State
import Control.Monad.Error
import Data.List
import Data.Traversable
import L.CompilationUnit
import L.L1L2AST
import L.L1L2Parser
import L.IOHelpers
import L.Read
import L.Utils
import L.L1.L132
import L.L1.L164
import System.Environment 
import System.IO

compileL1 :: String -> Either String String
compileL1 code = parseL1 (sread code) >>= genX8632Code

compileL1OrDie :: String -> String
compileL1OrDie = (either error id) . compileL1

-- reads first command line argument, loads that file
-- compiles it, writes the result to same file location
-- except with .S as the extension instead of .L1
compileL1File :: IO ()
compileL1File = compile compileL1OrDie "S"
compileL1File_ :: FilePath -> IO String
compileL1File_ = compile1 compileL1OrDie
