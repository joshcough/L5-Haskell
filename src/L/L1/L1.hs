module L.L1.L1 (l1Language) where

import Control.Applicative
import Data.Traversable
import L.Compiler
import L.L1L2AST
import L.L1L2Parser
import L.L1.L1Interp
import L.L1.MainAdjuster
import L.IOHelpers
import L.Read
import L.Utils
import L.L1.L1X86
import System.Cmd
import System.Environment 
import System.IO

l1Language :: Language L1 String
l1Language  = Language 
  parseL1 
  compileL1
  interpL1
  (\name dir i -> Right <$> runSCodeNative name dir i)

compileL1 :: L1 -> Either String String
compileL1 = genX86Code . adjustMain

runSCodeNative :: String -> FilePath -> String -> IO String
runSCodeNative name outputDir sCode = do
  _ <- writeFile sFile sCode
  runSFileNative sFile outputDir where
  sFile = changeDir outputDir (name ++ ".S")

runSFileNative :: FilePath -> FilePath -> IO String
runSFileNative sFile outputDir = 
  let f newExt = changeDir (changeExtension sFile newExt) outputDir
      oFile   = f "S.o"
      outFile = f "S.out"
      resFile = f "S.res"
  in do
    _ <- rawSystem "as"  ["-o", oFile,   sFile]
    _ <- rawSystem "gcc" ["-o", outFile, oFile, "bin/runtime.o"]
    _ <- rawSystem "bin/run.sh" [outFile, resFile]
    readFile resFile

