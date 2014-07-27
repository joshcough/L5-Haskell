{-# LANGUAGE TupleSections #-}
module L.L2.L2 (l2Language, interpL2) where

import Control.Applicative
import Data.List
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Traversable
import Debug.Trace
import L.Compiler
import L.IOHelpers
import L.L1L2AST
import L.L1L2Parser
import L.Read
import L.Utils
import L.L1.L1 
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

l2Language :: Language L2 L1
l2Language  = Language
  parseL2
  (Right . compileL2ToL1)
  (interpL1 . compileL2ToL1)
  runL1Native

interpL2 = interpL1 . compileL2ToL1

runL1Native :: String -> FilePath -> L1 -> IO (Val String)
runL1Native name outputDir l1 = do
  _ <- writeFile l1File (show l1)
  compileAndRunNative l1Language name outputDir l1 where
  l1File = changeDir outputDir (name ++ ".L1")

-- this is the main function, the rest are just various helpers
compileL2ToL1 :: L2 -> L1
compileL2ToL1 (Program main fs) =
  Program (allocate mainWithRet) $ (allocate <$> fs) where 
  mainWithRet = Func (body main ++ [Return])

