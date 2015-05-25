{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}

module L.Interpreter.Output where

import Control.Applicative
import Control.Monad.Error.Class
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Trans.Error hiding (throwError)
import Control.Monad.Trans.Identity
import Control.Monad.Writer
import Data.Bifunctor
import Data.Monoid()
import System.IO

class Monad m => MonadOutput m where
  stdOut :: String -> m ()
  stdErr :: String -> m ()
  default stdOut :: (MonadTrans t, MonadOutput n, m ~ t n) => String -> m ()
  stdOut = lift . stdOut
  default stdErr :: (MonadTrans t, MonadOutput n, m ~ t n) => String -> m ()
  stdErr = lift . stdErr

instance MonadOutput IO where
  stdOut = hPutStr stdout
  stdErr = hPutStr stderr

instance Monad m => MonadOutput (OutputT m) where
  stdOut s = tell [StdOut s]
  stdErr s = tell [StdErr s]

instance MonadOutput m => MonadOutput (IdentityT m)
instance MonadOutput m => MonadOutput (StateT s m)
instance MonadOutput m => MonadOutput (ReaderT s m)
instance (Error s, MonadOutput m)  => MonadOutput (ErrorT s m)

data Output = StdOut String | StdErr String
outputText :: Output -> String
outputText (StdOut s) = s
outputText (StdErr s) = s

type OutputT = WriterT [Output]

data Halt = Normal | Exceptional String deriving Show
halt :: MonadError Halt m => m a
halt = throwError Normal
exception :: MonadError Halt m => String -> m a
exception s = throwError (Exceptional s)
data RunState = Running | Halted Halt deriving Show

instance Error Halt where
  noMsg  = Normal
  strMsg = Exceptional
