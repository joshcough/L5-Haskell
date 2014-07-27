module L.NativeRunner where

import L.IOHelpers
import L.Utils
import System.Cmd
import System.Environment 
import System.IO

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

