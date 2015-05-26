module L.CommandLine where

import L.Compiler hiding (Parser)
import Options.Applicative

addInfo :: String -> Parser a -> ParserInfo a
addInfo desc p = 
  info (helper  <*> p)
       (fullDesc <> progDesc desc <> 
       (header "The L Programming Language"))

osParser :: Parser String
osParser = strOption (
  long "os" <>
  metavar "operating system"
  <> help "darwin|linux")

outputDirParser :: Parser String
outputDirParser = strOption (
  long "outputDir" <>
  short 'o' <>
  metavar "outputDir" <>
  help "the directory to write output files")

compileOptionsParser :: Parser CompilationOptions
compileOptionsParser = compOpts <$> optional osParser <*> optional outputDirParser

-- TODO: for many files later, use:
-- some (argument str (metavar "..."))
lStarFileParser :: Parser FilePath
lStarFileParser = argument str $ metavar "'.L* file'"
