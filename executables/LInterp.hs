module Main where

import Control.Lens
import L.CommandLine
import L.Compiler
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
main' (turtles, opts, file) = g (file^.extension) where
  go lang = 
    if turtles
    then interpretTurtlesFile lang opts file >>= putStrLn . show
    else interpretFile lang file >>= putStrLn . runVal
  g ".L1" = go l1Language
  g ".L2" = go l2Language
  g ".L3" = go l3Language
  g ".L4" = go l4Language
  g ".L5" = go l5Language
  g bad  = error $ "LInterp: bad input file: " ++ file

