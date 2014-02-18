module L.L1.L1 
  (
    compileL1AndRunNative
   ,compileL1FileAndRunNative
   ,compileL1
   ,compileL1OrDie
   ,compileL1File
   ,compileL1File_
   ,runNative
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
import L.L1.L1X86
import System.Cmd
import System.Environment 
import System.IO

compileL1 :: String -> Either String String
compileL1 code = parseL1 (sread code) >>= genX86Code

compileL1OrDie :: String -> String
compileL1OrDie = (either error id) . compileL1

compileL1File :: IO ()
compileL1File = compile compileL1OrDie "S"
compileL1File_ :: FilePath -> IO String
compileL1File_ = compile1 compileL1OrDie

compileL1FileAndRunNative :: FilePath -> FilePath -> IO String
compileL1FileAndRunNative l1File outputDir = do
  s <- compileL1File_ l1File
  _   <- writeFile sFile s
  runNative sFile outputDir where 
  sFile = changeDir (changeExtension l1File "S") outputDir

-- the second argument is represents where the original code came from
-- maybe it came from an L5-L2 file. 
-- or, maybe it didn't come from a file at all
compileL1AndRunNative :: L1 -> Maybe FilePath -> FilePath -> IO String
compileL1AndRunNative l1 inputFile outputDir = do
  _   <- writeFile sFile s
  runNative sFile outputDir where 
  s = either error id $ genX86Code l1
  sFile = case inputFile of
    Just f  -> changeDir (changeExtension f "S") outputDir
    Nothing -> outputDir ++ "tmp.S"

runNative :: FilePath -> FilePath -> IO String
runNative sFile outputDir = 
  let f newExt = changeDir (changeExtension sFile newExt) outputDir
      oFile   = f "S.o"
      outFile = f "S.out"
      resFile = f "S.res"
  in do
    _ <- rawSystem "as"  ["-o", oFile,   sFile]
    _ <- rawSystem "gcc" ["-o", outFile, oFile, "bin/runtime.o"]
    _ <- rawSystem "bin/run.sh" [outFile, resFile]
    readFile resFile
