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

module L.Memory where

import Control.Applicative
import Control.Lens hiding (set)
import Control.Monad.Error.Class
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.ST.Class
import Control.Monad.Trans.Error hiding (throwError)
import Control.Monad.Trans.Identity
import Control.Monad.Writer
import Data.Bifunctor
import Data.Bits
import Data.Int
import Data.Monoid()
import Data.Vector.Fusion.Stream hiding ((++))
import Data.Vector.Generic.Mutable (update)
import Data.Vector.Mutable (STVector, read, write)
import qualified Data.Vector.Mutable as MV
import Prelude hiding (read)
import System.IO
import L.Utils

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

data Halt = Normal | Exceptional String
halt :: MonadError Halt m => m a
halt = throwError Normal
exception :: MonadError Halt m => String -> m a
exception s = throwError (Exceptional s)
data RunState = Running | Halted Halt

instance Error Halt where
  noMsg  = Normal
  strMsg = Exceptional

data Memory s = Memory {
  _runMemory :: STVector s Int64
 ,_heapP     :: Int64 -- pointer to top of heap  -- this is good across all languages!
}
makeClassy ''Memory

type MonadMemory mem m a = (Functor m, MonadST m, MonadError Halt m, MonadState mem m, HasMemory mem (World m))

oneMeg, twoMeg, memSize :: Int
oneMeg = 1048576
twoMeg = oneMeg * 2
memSize = twoMeg
zero :: Int64
zero = 0

newMem :: MonadST m => m (Memory (World m))
newMem = liftST $ Memory <$> (MV.replicate memSize zero) <*> pure 0

-- write an int into memory at the given address
writeMem :: MonadMemory mem m a => String -> Int64 -> Int64 -> m ()
writeMem caller addr value = go where
  index = fromIntegral addr `div` 8
  go | index < memSize = do m <- use memory; liftST $ write (_runMemory m) index value
     | otherwise = exception $ caller ++ "tried to write out of bounds memory index: " ++ show index

-- read a single int from memory
readMem :: MonadMemory mem m a => String -> Int64 -> m Int64
readMem caller addr = go where
  index = fromIntegral addr `div` 8
  go | index < memSize = do m <- use memory; liftST $ read (_runMemory m) index
     | otherwise       = exception $ caller ++ "tried to access out of bounds memory index: " ++ show index

-- read an array from memory
readArray :: MonadMemory mem m a => Int64 -> m (STVector (World m) Int64)
readArray addr = do
  s <- fromIntegral `liftM` readMem "readArray" addr
  let startIndex = fromIntegral addr `div` 8 + 1
  if startIndex + s < memSize
    then uses memory (MV.slice startIndex s . _runMemory)
    else exception $ "readArray tried to access out of bounds memory index: " ++ show startIndex

encodeNum :: Int64 -> Int64
encodeNum n = shiftR n 1

-- TODO: test if heap runs into stack, and vice-versa
allocate :: (MonadState mem m, MonadMemory mem m a) => Int64 -> Int64 -> m Int64
allocate size n = do
  hp <- use heapP
  let size'   = encodeNum size
      ns      = Prelude.replicate (fromIntegral size') n
      indices :: [Int]
      indices = [(fromIntegral $ hp `div` 8)..]
      nsWithIndices = Prelude.zip indices $ size' : ns
  do m <- use memory; liftST $ update (_runMemory m) (fromList nsWithIndices)
  heapP <<+= ((size'+1) * 8)

-- print a number or an array
--   if the int argument is an encoded int, prints the int
--   else it's an array, print the contents of the array (and recur)
print :: forall mem m a . (MonadOutput m, MonadMemory mem m a) => Int64 -> m ()
print n = printContent 0 n >>= \s -> stdOut (s ++ "\n") where
  loop v depth index
    | index >= MV.length v = return []
    | otherwise = do
      h <- liftST (read v index) >>= printContent depth
      t <- loop v depth (index + 1)
      return $ h : t
  printContent :: Int -> Int64 -> m String
  printContent depth n
    | depth >= 4   = return $ "..."
    | n .&. 1 == 1 = return . show $ shiftR n 1
    | otherwise    = do
      size      <- readMem "print" n
      arr       <- readArray n
      contentsV <- loop arr depth 0
      return $ "{s:" ++ (mkString ", " $ show size : contentsV) ++ "}"

-- print an array error
arrayError :: (MonadOutput m, MonadMemory mem m a) => Int64 -> Int64 -> m ()
arrayError a x = do
  size <- readMem "arrayError" a
  stdErr $ "attempted to use position " ++ show (encodeNum x) ++
           " in an array that only has " ++ show size ++ " positions"
  halt
