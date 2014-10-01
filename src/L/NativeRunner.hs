module L.NativeRunner where

import L.IOHelpers
import System.Process

runSCodeNative :: String -> FilePath -> String -> IO String
runSCodeNative name outputDir sCode = do
  _ <- writeFile sFile sCode
  runSFileNative sFile outputDir where
  sFile = changeDir (name ++ ".S") outputDir

runSFileNative :: FilePath -> FilePath -> IO String
runSFileNative sFile outputDir = 
  let f newExt = changeDir (changeExt sFile newExt) outputDir
      oFile   = f "S.o"
      outFile = f "S.out"
      resFile = f "S.res"
  in do
    _ <- rawSystem "as"  ["-o", oFile,   sFile]
    _ <- rawSystem "gcc" ["-o", outFile, oFile, "bin/runtime.o"]
    _ <- rawSystem "bin/run.sh" [outFile, resFile]
    readFile resFile

