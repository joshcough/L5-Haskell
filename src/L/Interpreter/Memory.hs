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

module L.Interpreter.Memory where

import Control.Applicative
import Control.Lens hiding (set)
import Control.Monad.Error.Class
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.ST.Class
import Data.Bits
import Data.Int
import Data.Monoid()
import Data.Vector.Fusion.Stream hiding ((++))
import Data.Vector.Generic.Mutable (update)
import Data.Vector.Mutable (STVector, read, write)
import qualified Data.Vector.Mutable as MV
import Prelude hiding (read)
import L.Interpreter.Output
import L.Interpreter.Runtime
import L.Utils

data Memory s = Memory {
  _runMemory :: STVector s Runtime
 ,_heapP     :: Int64 -- pointer to top of heap  -- this is good across all languages!
}
makeClassy ''Memory

type MonadMemory mem m = (Functor m, MonadST m, MonadError Halt m, MonadState mem m, HasMemory mem (World m))

oneMeg, twoMeg, memSize :: Int64
oneMeg = 1048576
twoMeg = oneMeg * 2
memSize = twoMeg
zero :: Int64
zero = 0
memSizeAsInt :: Int
memSizeAsInt = fromIntegral memSize

newMem :: MonadST m => m (Memory (World m))
newMem = liftST $ Memory <$> MV.replicate memSizeAsInt (Num zero) <*> pure 0

-- write an int into memory at the given address
writeMem :: MonadMemory mem m => String -> Runtime -> Runtime -> m ()
writeMem caller addr value = do
  p <- expectPointer addr (caller ++ "/writeMem")
  let index = (p `div` 8) :: Int64
      inbounds = index < memSize
  if inbounds
    then do m <- use memory; liftST $ write (_runMemory m) (fromIntegral index) value
    else exception $ caller ++ "tried to write out of bounds memory index: " ++ show index

-- read a single int from memory
readMem :: MonadMemory mem m => String -> Runtime -> m Runtime
readMem caller addr = do
  p <- expectPointer addr (caller ++ "/readMem")
  let index = (p `div` 8) :: Int64
      inbounds = index < memSize
  if inbounds
    then do m <- use memory; liftST $ read (_runMemory m) (fromIntegral index)
    else exception $ caller ++ "tried to access out of bounds memory index: " ++ show index

-- read an array from memory
readArray :: MonadMemory mem m => Runtime -> m (STVector (World m) Runtime)
readArray addr = do
  p    <- expectPointer addr "readArray"
  size <- evalAddrToNum p
  let startIndex :: Int
      startIndex = fromIntegral p `div` 8 + 1
  if startIndex + fromIntegral size < memSizeAsInt
    then uses memory (MV.slice startIndex (fromIntegral size) . _runMemory)
    else exception $ "readArray tried to access out of bounds memory index: " ++ show startIndex

evalAddrToNum :: MonadMemory mem m => Int64 -> m Int64
evalAddrToNum addr = do
  r <- readMem "evalToNum" $ Pointer addr
  expectNum r

encodeNum :: Int64 -> Int64
encodeNum n = shiftR n 1

-- TODO: test if heap runs into stack, and vice-versa
allocate :: (MonadState mem m, MonadMemory mem m) => Runtime -> Runtime -> m Runtime
allocate size r = do
  hp    <- use heapP
  size' <- encodeNum <$> expectNum size
  let rs      = Prelude.replicate (fromIntegral size') r
      indices = [(fromIntegral $ hp `div` 8)..]
      rsWithIndices = Prelude.zip indices $ Num size' : rs
  do m <- use memory; liftST $ update (_runMemory m) (fromList rsWithIndices)
  heapP += ((size'+1) * 8)
  return $ Pointer hp

-- print a number or an array
--   if the int argument is an encoded int, prints the int
--   else it's an array, print the contents of the array (and recur)
print :: forall mem m. (MonadOutput m, MonadMemory mem m) => Runtime -> m ()
print n = printContent 0 n >>= \s -> stdOut (s ++ "\n") where
  loop v depth index
    | index >= MV.length v = return []
    | otherwise = do
      h <- liftST (read v index) >>= printContent depth
      t <- loop v depth (index + 1)
      return $ h : t
  printContent :: Int -> Runtime -> m String
  printContent depth r
    | depth >= 4   = return "..."
    | otherwise = case r of
      -- TODO: should i make sure that i is odd?
      -- n .&. 1 == 1 = return . show $ shiftR n 1
      Num i         -> return . show $ shiftR i 1
      p@(Pointer _) -> do
        size      <- readMem "print" p >>= expectNum
        arr       <- readArray p
        contentsV <- loop arr depth 0
        return $ "{s:" ++ mkString ", " (show size : contentsV) ++ "}"
      FunctionPointer bad -> exception $ "cannot print a label, but tried to print: " ++ show bad

-- print an array error
arrayError :: (MonadOutput m, MonadMemory mem m) => Runtime -> Runtime -> m ()
arrayError a x = do
  size <- readMem "arrayError" a >>= expectNum
  index <- expectNum x
  stdErr $ "attempted to use position " ++ show (encodeNum index) ++
           " in an array that only has " ++ show size ++ " positions"
  halt
