{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeOperators #-}

module Main where

import Control.Lens
import L.CommandLine
import L.Compiler
import L.Parser.SExpr
import L.Primitives (X86)
import L.Util.IOHelpers
import L.Util.NativeRunner
import L.L1.L1
import L.L2.L2
import L.L3.L3
import L.L4.L4
import L.L5.L5
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
    g ".L1" = go l1CompilerShowable
    g ".L2" = go l2CompilerShowable
    g ".L3" = go l3CompilerShowable
    g ".L4" = go l4CompilerShowable
    g ".L5" = go l5CompilerShowable
    g bad  = error $ "LC: bad input file: " ++ file

c :: FromSExpr i => Bool -> Thrist (Show :=> Compiler1) i X86 -> CompilationOptions -> FilePath -> IO ()
c False comp opts file = do
  res <- compileFileAndWriteResult (unconstrain comp) (opts & outputDir %~ (<|> (Just $ file^.directory))) file
  either error (const $ return ()) res
c True comp opts file = do
  res <- compileFileAndRunNative   (unconstrain comp) (opts & outputDir %~ (<|> (Just $ file^.directory))) file
  either error putStrLn res
