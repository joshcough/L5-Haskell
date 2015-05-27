{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeOperators #-}

module Main where

import Control.Lens
import L.CommandLine
import L.Compiler
import L.Parser.SExpr
import L.Runners
import L.Util.IOHelpers
import L.L1.L1Interp
import L.L1.L1
import L.L2.L2
import L.L3.L3
import L.L4.L4
import L.L5.L5
import Options.Applicative
import System.FilePath.Lens

main :: IO ()
main = execParser commandLineParser >>= main'

commandLineParser :: ParserInfo (Bool, CompilationOptions, FilePath)
commandLineParser = addInfo "The L Interpreter" $ 
  (,,) <$> turtlesMode <*> compileOptionsParser <*> lStarFileParser

turtlesMode = switch
  (short 't' <> long "turtles" <> 
   help "Interpret all intermediate languages" )

main' :: (Bool, CompilationOptions, FilePath) -> IO ()
main' (turtles, opts, file) =
  if turtles
    then withLanguage (file^.extension) tn
    else withInterp   (file^.extension) t1  where
    tn :: Language i o -> IO ()
    tn lang = interpretTurtlesFile lang opts file >>= putStrLn . show
    t1 :: FromSExpr i => Interpreter i -> IO ()
    t1 i = interpretFile i file >>= putStrLn . runVal
