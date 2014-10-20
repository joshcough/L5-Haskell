{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}

module L.LCompiler where

import Control.Applicative
import Control.Category
import Control.Lens
import Control.Monad.Error.Class
import Data.Default
import Data.Maybe
import L.OS
import Prelude hiding ((.),id)
import System.FilePath.Lens

type ProgramName = String

type CompilerMonad m = (MonadError CompileError m)

data CompileError = CompileError [String]
exception :: MonadError CompileError m => String -> m a
exception s = throwError (strMsg s)

instance Error CompileError where
  noMsg    = CompileError []
  strMsg s = CompileError [s]


data LCompiler i o = LCompiler {
   _runLCompiler :: CompilationOptions -> ProgramName -> i -> o
}

data CompilationOptions = CompilationOptions {
  _os :: OS,
  _outputDir :: Maybe FilePath
} deriving (Show, Eq)
makeLenses ''CompilationOptions

compOpts :: Maybe String -> Maybe FilePath -> CompilationOptions
compOpts mOs mOutputDir =
  CompilationOptions (osFromMaybeString mOs) mOutputDir

-- this sets it only if it isnt there
getOutputDirOrElse :: CompilationOptions -> FilePath -> FilePath
getOutputDirOrElse opts inputFile = fromJust $ setIfAbsent^.outputDir where
  setIfAbsent :: CompilationOptions
  setIfAbsent = opts & outputDir %~ (<|> (Just $ inputFile^.directory))

instance Default CompilationOptions where
  def = CompilationOptions systemOS Nothing
