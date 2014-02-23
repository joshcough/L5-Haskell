{-# LANGUAGE TupleSections #-}
module L.L2.L2
  (
    compileL2
   ,compileL2OrDie
   ,compileL2FileAndRunNative
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

compileL2 :: String -> Either String L1
compileL2 code = genL1Code <$> parseL2 (sread code) where
  --genL1Code :: L2 -> L1
  genL1Code (Program main fs) =
    let mainWithRet = Func (body main ++ [Return])
    in Program (allocate mainWithRet) $ (allocate <$> fs)

compileL2OrDie :: String -> L1
compileL2OrDie = (either error id) . compileL2

compileL2ToX86 :: String -> String
compileL2ToX86 code = either error id $ compileL2 code >>= genX86Code

--compileL2File :: IO ()
--compileL2File = compile compileL2OrDie "S"
compileL2File_ :: FilePath -> IO L1
compileL2File_ = compile1 compileL2OrDie

compileL2FileAndRunNative :: FilePath -> FilePath -> IO String
compileL2FileAndRunNative l2File outputDir = do
  l1 <- compileL2File_ l2File
  _  <- writeFile l1File (show l1)
  compileL1AndRunNative l1 (Just l2File) outputDir where
  l1File = changeDir (changeExtension l2File "L1") outputDir

interpL2String :: String -> Either String String
interpL2String code = showOutput . interpL1 <$> compileL2 code

interpL2OrDie :: String -> String
interpL2OrDie = (either error id) . interpL2String

interpL2File =
  do s <- compile_ $ (either error id) . interpL2String
     putStrLn (snd s)
