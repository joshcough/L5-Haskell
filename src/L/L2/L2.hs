{-# LANGUAGE TupleSections #-}
module L.L2.L2
  (
    compileL2
   ,compileL2OrDie
   ,compileL2AndRunNative
   ,compileL2FileAndRunNative
   ,compileL2ToL1
   ,interpL2
   ,interpL2File
  )
where

import Control.Applicative
import Data.List
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Traversable
import Debug.Trace
import L.CompilationUnit
import L.IOHelpers
import L.L1L2AST
import L.L1L2Parser
import L.Read
import L.Utils
import L.L1.L1 (compileL1AndRunNative)
import L.L1.L1X86
import L.L1.L1Interp
import L.L2.Allocation

-- L2 introduces variables on top of L1.
-- the L2 compiler is really just a register allocator.
-- for each function in the program, it tries to see if it can allocate it as is.
-- if so, great, assign variables to registers.
-- if it is unable to, it spills a variable, and tries again.
-- after that, it continuously tries to allocate the function.
-- it does this until either a) it works, or b) it is out of variables to spill.
-- the last case results in error.

-- this is the main function, the rest are just various helpers
compileL2ToL1 :: L2 -> L1
compileL2ToL1 (Program main fs) =
  Program (allocate mainWithRet) $ (allocate <$> fs) where 
  mainWithRet = Func (body main ++ [Return])

compileL2 :: String -> Either String L1
compileL2 code = compileL2ToL1 <$> parseL2 (sread code)

compileL2OrDie :: String -> L1
compileL2OrDie = (either error id) . compileL2

compileL2ToX86 :: String -> String
compileL2ToX86 code = either error id $ compileL2 code >>= genX86Code

compileL2File_ :: FilePath -> IO L1
compileL2File_ = compile1 compileL2OrDie

-- the second argument is represents where the original code came from
-- maybe it came from an L5-L3 file. 
compileL2AndRunNative :: L2 -> FilePath -> FilePath -> IO String
compileL2AndRunNative l2 inputFile outputDir =
  let l1 = compileL2ToL1 l2 in do
  _  <- writeFile l1File (show l1)
  compileL1AndRunNative l1 inputFile outputDir where
  l1File = changeDir (changeExtension inputFile "L1") outputDir

compileL2FileAndRunNative :: FilePath -> FilePath -> IO String
compileL2FileAndRunNative l2File outputDir = do
  l2 <- parseL2OrDie . sread <$> readFile l2File
  compileL2AndRunNative l2 l2File outputDir

interpL2 :: L2 -> Computer
interpL2 l2 = interpL1 $ compileL2ToL1 l2

interpL2String :: String -> Either String String
interpL2String code = showOutput . interpL1 <$> compileL2 code

interpL2OrDie :: String -> String
interpL2OrDie = (either error id) . interpL2String

interpL2File =
  do s <- compile_ $ (either error id) . interpL2String
     putStrLn (snd s)
