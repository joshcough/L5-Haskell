module L.L3.L3 
  (
    compileL3
   --,compileL3OrDie
   --,compileL3FileAndRunNative
   --,interpL3File
  )
where

import Control.Applicative
import L.CompilationUnit
import L.IOHelpers
import L.L1L2AST
import L.L1L2Parser
import L.L1.L1Interp (Computer, showOutput)
import L.Read
import L.Utils
import L.L2.L2 (compileL2ToL1, compileL2AndRunNative, interpL2)
import L.L3.L3AST
import L.L3.Linearize
import L.L3.L3Parser

compileL3ToL2 :: L3 -> L2
compileL3ToL2 = linearize

compileL3 :: String -> Either String L2
compileL3 code = compileL3ToL2 <$> parseL3 (sread code)

compileL3OrDie :: String -> L2
compileL3OrDie = (either error id) . compileL3

compileL3File_ :: FilePath -> IO L2
compileL3File_ = compile1 compileL3OrDie

compileL3FileAndRunNative :: FilePath -> FilePath -> IO String
compileL3FileAndRunNative l3File outputDir = do
  l2 <- compileL3File_ l3File
  _  <- writeFile l2File (show l2)
  compileL2AndRunNative l2 l3File outputDir where
  l2File = changeDir (changeExtension l3File "L2") outputDir

interpL3 :: L3 -> Computer
interpL3 l3 = interpL2 $ compileL3ToL2 l3

interpL3String :: String -> Either String String
interpL3String code = showOutput . interpL2 <$> compileL3 code

interpL3OrDie :: String -> String
interpL3OrDie = (either error id) . interpL3String

interpL3File =
  do s <- compile_ $ (either error id) . interpL3String
     putStrLn (snd s)
