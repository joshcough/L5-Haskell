module L.CommandLine where

import L.Compiler
import Options.Applicative

commandLineParserWithoutInfo :: Parser (CompilationOptions, FilePath)
commandLineParserWithoutInfo = (,) <$> optsParser <*> mainFileArgParser

commandLineParser = 
  info (helper <*> commandLineParserWithoutInfo)
       ( fullDesc
          <> progDesc "The L Compiler"
          <> header "not sure what to put here yet" )

optsParser :: Parser CompilationOptions
optsParser = compOpts <$> (optional $ strOption
  ( long "os" <> short 'o' <> metavar "operating system" <> help "darwin|linux" ))

mainFileArgParser :: Parser FilePath
mainFileArgParser = argument str $ metavar "'L file'"
