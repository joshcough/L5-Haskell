module L.L1.L1 
  (
    compileL132
   ,compileL132OrDie
   ,compileL132File
   ,compileL132File_
   ,compileL164
   ,compileL164OrDie
   ,compileL164File
   ,compileL164File_
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

compileL132 :: String -> Either String String
compileL132 code = parseL132 (sread code) >>= genX8632Code

compileL132OrDie :: String -> String
compileL132OrDie = (either error id) . compileL132

-- reads first command line argument, loads that file
-- compiles it, writes the result to same file location
-- except with .S as the extension instead of .L1
compileL132File :: IO ()
compileL132File = compile compileL132OrDie "S"
compileL132File_ :: FilePath -> IO String
compileL132File_ = compile1 compileL132OrDie


compileL164 :: String -> Either String String
compileL164 code = parseL164 (sread code) >>= genX8664Code

compileL164OrDie :: String -> String
compileL164OrDie = (either error id) . compileL164

compileL164File :: IO ()
compileL164File = compile compileL164OrDie "S"
compileL164File_ :: FilePath -> IO String
compileL164File_ = compile1 compileL164OrDie
