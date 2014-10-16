{-# LANGUAGE RankNTypes #-}

module Main where

import Control.Lens
import L.CommandLine
import L.Compiler
import L.IOHelpers
import L.NativeRunner
import L.L1.L1
import L.L2.L2
import L.L3.L3
import L.L4.L4
import Options.Applicative
import System.FilePath.Lens

main :: IO ()
main = execParser commandLineParser >>= main'

commandLineParser :: ParserInfo (Bool, Bool, CompilationOptions, FilePath)
commandLineParser = addInfo "The L Compiler" $
  (,,,) <$> execMode <*> turtlesMode <*> compileOptionsParser <*> lStarFileParser

execMode = switch
  (short 'x' <> long "exec" <>
   help "Run the native code after executing it." )

turtlesMode = switch
  (short 't' <> long "turtles" <>
   help "Write all intermediate languages" )

main' :: (Bool, Bool, CompilationOptions, FilePath) -> IO ()
main' (exec, turtles, opts, file) = case (file^.extension) of
  "S" -> runSFileNative (file^.filename) (file^.directory) >>= putStrLn
  ext -> g ext where
    go lang = c exec lang opts file
    g "L1" = go l1Language
    g "L2" = go l2Language
    g "L3" = go l3Language
    g "L4" = go l4Language
    g bad  = error $ "LC: bad input file: " ++ file

c :: Show o => Bool -> Language i o -> CompilationOptions -> FilePath -> IO ()
c False lang opts file = do
  res <- compileFileAndWriteResult lang (opts & outputDir %~ (<|> (Just $ file^.directory))) file
  either error (const $ return ()) res
c True lang opts file = do
  res <- compileFileAndRunNative   lang (opts & outputDir %~ (<|> (Just $ file^.directory))) file
  either error putStrLn res
