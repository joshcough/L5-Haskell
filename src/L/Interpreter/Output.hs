{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveFunctor #-}
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
  stdOut s = OutputT $ return ([StdOut s], ())
  stdErr s = OutputT $ return ([StdErr s], ())

instance MonadOutput m => MonadOutput (IdentityT m)
instance MonadOutput m => MonadOutput (StateT s m)
instance MonadOutput m => MonadOutput (ReaderT s m)
instance (Error s, MonadOutput m)  => MonadOutput (ErrorT s m)
instance (Monoid w, MonadOutput m) => MonadOutput (WriterT w m)

data Output = StdOut String | StdErr String
outputText :: Output -> String
outputText (StdOut s) = s
outputText (StdErr s) = s

newtype OutputT m a = OutputT { runOutputT :: m ([Output], a) }

instance Functor f => Functor (OutputT f) where
  fmap f (OutputT m) = OutputT $ fmap (second f) m

instance Applicative f => Applicative (OutputT f) where
  pure a = OutputT $ pure ([], a)
  OutputT f <*> OutputT a = OutputT $
    (,) <$> ((++) <$> fmap fst f <*> fmap fst a) <*> (fmap snd f <*> fmap snd a)

instance Monad m => Monad (OutputT m) where
  return a = OutputT $ return ([], a)
  OutputT m >>= f = OutputT $ do
    (xs, a) <- m
    (ys, b) <- runOutputT $ f a
    return (xs ++ ys, b)

instance MonadTrans OutputT where
  lift m = OutputT $ do a <- m; return ([], a)

data Halt = Normal | Exceptional String deriving Show
halt :: MonadError Halt m => m a
halt = throwError Normal
exception :: MonadError Halt m => String -> m a
exception s = throwError (Exceptional s)
data RunState = Running | Halted Halt deriving Show

instance Error Halt where
  noMsg  = Normal
  strMsg = Exceptional
