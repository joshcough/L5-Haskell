module L.CommandLine where

import L.Compiler
import Options.Applicative

addInfo :: String -> Parser a -> ParserInfo a
addInfo desc p = 
  info (helper  <*> p)
       (fullDesc <> progDesc desc <> 
       (header "The L Programming Language"))

compileOptionsParser :: Parser CompilationOptions
compileOptionsParser = compOpts <$> (optional $ strOption
  (long "os" <> short 'o' <> metavar "operating system" <> help "darwin|linux"))

-- TODO: for many files later, use:
-- some (argument str (metavar "..."))
lStarFileParser :: Parser FilePath
lStarFileParser = argument str $ metavar "'.L* file'"
