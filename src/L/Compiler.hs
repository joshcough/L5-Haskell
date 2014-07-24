module L.Compiler
  (
   --,compileL3OrDie
   --,compileL3FileAndRunNative
   --,interpL3File
  )
where

import Control.Applicative
import L.IOHelpers
import L.L1.L1Interp (Computer, showOutput)
import L.Read
import L.Utils

type CompilationResult o = Either String o

data Compiler i o = Compiler {
  parse   :: (SExpr -> ParseResult i), 
  compile :: (i -> CompilationResult o)
}

type Interpreter i = i -> Computer

compileString :: Compiler i o -> String -> CompilationResult o
compileString compiler code = do
  i <- parse compiler (sread code)
  compile compiler i

compileStringOrDie :: Compiler i o -> String -> o
compileStringOrDie c code = (either error id) (compileString c code)

compileFile :: Compiler i o -> FilePath -> IO (CompilationResult o)
compileFile c f = do { code <- readFile f; return $ compileString c code }

compileAndInterpret :: Compiler i o -> Interpreter o -> i -> Either String Computer
compileAndInterpret compiler interpreter input = do
  o <- compile compiler input
  return $ interpreter o

{-
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
-}
