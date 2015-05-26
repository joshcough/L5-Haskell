{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeOperators #-}

module Main where

import Control.Lens
import L.CommandLine
import L.Compiler
import L.Parser.SExpr
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
    then withLang   tn (file^.extension)
    else withInterp t1 (file^.extension) where
    tn :: Thrist (Show :=> Language1) i o -> IO ()
    tn lang = interpretTurtlesFile (unconstrain lang) opts file >>= putStrLn . show
    t1 :: Interpreter i -> IO ()
    t1 i = interpretFile i file >>= putStrLn . runVal

{- g (file^.extension) where
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
-}

withLang :: (forall i o. Thrist (Show :=> Language1) i o -> a) -> Extension -> a
withLang go ".L1" = go l1LanguageShowable
withLang go ".L2" = go l2LanguageShowable
withLang go ".L3" = go l3LanguageShowable
withLang go ".L4" = go l4LanguageShowable
withLang go ".L5" = go l5LanguageShowable

withLang1 :: (forall i o. Language1 i o -> a) -> Extension -> a
withLang1 go ".L1" = go l1Language1
withLang1 go ".L2" = go l2Language1
withLang1 go ".L3" = go l3Language1
withLang1 go ".L4" = go l4Language1
withLang1 go ".L5" = go l5Language1

withInterp :: (forall i. Interpreter i -> a) -> Extension -> a
withInterp go ".L1" = go l1Interpreter
withInterp go ".L2" = go l1Interpreter
withInterp go ".L3" = go l1Interpreter
withInterp go ".L4" = go l1Interpreter
withInterp go ".L5" = go l1Interpreter