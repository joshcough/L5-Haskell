{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}

module L.LCompiler where

import Control.Applicative
import Control.Arrow
import Control.Category
import Control.Lens
import Control.Monad (liftM)
import Control.Monad.Fix
import Control.Monad.Error.Class
import Data.Default
import Data.Profunctor
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

data LCompiler m a b = LCompiler {
   _runLCompiler :: CompilationOptions -> ProgramName -> a -> m b
} deriving Functor

-- instances we can build if we have
-- (Either (Compiler, Interpreter, Ext) (Compiler, Interpreter)) -- adding in parser loses Profunctor stuff
-- False, True
instance Applicative m => Applicative (LCompiler m i) where
  pure a = LCompiler $ \_ _ _ -> pure a
  LCompiler f <*> LCompiler a = LCompiler $ \opts name i ->
    f opts name i <*> a opts name i

-- False, False
instance Monad m => Monad (LCompiler m i) where
  return a = LCompiler $ \_ _ _ -> return a
  (>>=) :: LCompiler m i a -> (a -> LCompiler m i b) -> LCompiler m i b
  m >>= k = LCompiler $ \opts name i -> do
    a <- _runLCompiler m opts name i
    _runLCompiler (k a) opts name i

-- False, True
instance Monad m => Category (LCompiler m) where
  id :: LCompiler m a a
  id = LCompiler $ \_ _ -> return
  (.) :: LCompiler m b c -> LCompiler m a b -> LCompiler m a c
  LCompiler fbc . LCompiler fab = LCompiler $ \opts name a -> do
    b <- fab opts name a
    fbc opts name b

-- True, True
instance Functor f => Profunctor (LCompiler f) where
  dimap :: (a -> b) -> (c -> d) -> LCompiler f b c -> LCompiler f a d
  dimap ab cd (LCompiler bmc) = LCompiler $ \opts name a -> cd <$> bmc opts name (ab a)

-- True, True
instance Functor f => Strong (LCompiler f) where
 first' :: LCompiler f a b -> LCompiler f (a, c) (b, c)
 first' (LCompiler amb) = LCompiler $ \opts name (a, c) ->
   (\b -> (b,c)) <$> amb opts name a
 second' :: LCompiler f a b -> LCompiler f (c, a) (c, b)
 second' (LCompiler amb) = LCompiler $ \opts name (c, a) ->
   (\b -> (c,b)) <$> amb opts name a

-- False, True
instance Monad m => Arrow (LCompiler m) where
 arr :: (a -> b) -> LCompiler m a b
 arr ab = LCompiler $ \_ _ a -> return $ ab a
 first :: LCompiler m a b -> LCompiler m (a, c) (b, c)
 first (LCompiler amb) = LCompiler $ \opts name (a, c) -> liftM (\b -> (b,c)) $ amb opts name a

-- False, not sure
instance Monad m => ArrowChoice (LCompiler m) where
  left :: LCompiler m a b -> LCompiler m (Either a c) (Either b c)
  left (LCompiler amb) = LCompiler $ \opts name eac -> case eac of
    Left  a -> do b <- amb opts name a; return $ Left b
    Right c -> return $ Right c

-- False, False
instance Monad m => ArrowApply (LCompiler m) where
  app :: LCompiler m (LCompiler m b c, b) c
  app = LCompiler $ \name opts (LCompiler f, b) -> f name opts b

-- False, False
instance MonadFix m => ArrowLoop (LCompiler m) where
    loop (LCompiler f) = LCompiler $ \name opts a ->
      let f' x y = f name opts (x, snd y)
      in liftM fst $ mfix $ f' a

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
