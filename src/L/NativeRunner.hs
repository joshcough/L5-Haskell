module L.NativeRunner where

import Control.Lens
import System.Process
import System.FilePath.Lens

runSFileNative :: FilePath -> FilePath -> IO String
runSFileNative sFile outputDir = 
  let f newExt = sFile & extension .~ newExt & directory .~ outputDir
      oFile   = f "o"
      outFile = f "out"
      resFile = f "res"
  in do
    _ <- rawSystem "as"  ["-o", oFile,   sFile]
    _ <- rawSystem "gcc" ["-o", outFile, oFile, "bin/runtime.o"]
    _ <- rawSystem "bin/run.sh" [outFile, resFile]
    readFile resFile

