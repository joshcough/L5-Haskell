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
import Data.Vector (Vector, freeze)
import Data.Vector.Fusion.Stream hiding ((++))
import Data.Vector.Generic.Mutable (update)
import Data.Vector.Mutable (STVector, read, write)
import qualified Data.Vector.Mutable as MV
import Prelude hiding (read)
import L.Interpreter.Output
import L.Interpreter.Runtime
import L.L1L2AST
import L.Utils

data Memory s = Memory {
  _runMemory :: STVector s Runtime
 ,_heapP     :: Int64 -- pointer to top of heap
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
  p <- expectPointer (caller ++ "/writeMem") addr
  let index = (p `div` 8) :: Int64
      inbounds = index < memSize
  if inbounds
    then do m <- use memory; liftST $ write (_runMemory m) (fromIntegral index) value
    else exception $ caller ++ "tried to write out of bounds memory index: " ++ show index

-- read a single int from memory
readMem :: MonadMemory mem m => String -> Runtime -> m Runtime
readMem caller addr = do
  p <- expectPointer (caller ++ "/readMem") addr
  let index = (p `div` 8) :: Int64
      inbounds = index < memSize
  if inbounds
    then do m <- use memory; liftST $ read (_runMemory m) (fromIntegral index)
    else exception $ caller ++ "tried to access out of bounds memory index: " ++ show index

-- read an array from memory
readArray :: MonadMemory mem m => Runtime -> m (STVector (World m) Runtime)
readArray addr = do
  p    <- expectPointer "readArray" addr
  size <- evalAddrToNum "readArray" p
  let startIndex :: Int
      startIndex = fromIntegral p `div` 8 + 1
  if startIndex + fromIntegral size < memSizeAsInt
    then uses memory (MV.slice startIndex (fromIntegral size) . _runMemory)
    else exception $ "readArray tried to access out of bounds memory index: " ++ show startIndex

evalAddrToNum :: MonadMemory mem m => String -> Int64 -> m Int64
evalAddrToNum caller addr = readMem caller (Pointer addr) >>= expectNum

encodeNum :: Int64 -> Int64
encodeNum n = shiftR n 1

-- TODO: test if heap runs into stack, and vice-versa
-- NOTE: allocate is Haskell's replicate
allocate :: (MonadState mem m, MonadMemory mem m) => Runtime -> Runtime -> m Runtime
allocate size r = do
  size' <- encodeNum <$> expectNum size
  newArray $ Prelude.replicate (fromIntegral size') r

newArray :: (MonadState mem m, MonadMemory mem m) => [Runtime] -> m Runtime
newArray rs = do
  hp   <- use heapP
  let size = encodeNum . fromIntegral $ Prelude.length rs
      indices = [(fromIntegral $ hp `div` 8)..]
      rsWithIndices = Prelude.zip indices $ Num size : rs
  do m <- use memory; liftST $ update (_runMemory m) (fromList rsWithIndices)
  heapP += ((size+1) * 8)
  return $ Pointer hp

-- print a number or an array
--   if the int argument is an encoded int, prints the int
--   else it's an array, print the contents of the array (and recur)
print :: forall mem m. (MonadOutput m, MonadMemory mem m) => Runtime -> m Runtime
print n = printContent 0 n >>= \s -> stdOut (s ++ "\n") >> return (Num 1) where
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
        size      <- arraySizeNum "print" p
        arr       <- readArray p
        contentsV <- loop arr depth 0
        return $ "{s:" ++ mkString ", " (show size : contentsV) ++ "}"
      FunctionPointer bad -> exception $ "cannot print a label, but tried to print: " ++ show bad

-- print an array error
arrayError :: (MonadOutput m, MonadMemory mem m) => Runtime -> Runtime -> m a
arrayError a x = do
  size  <- arraySizeNum "arrayError" a
  index <- expectNum x
  stdErr $ "attempted to use position " ++ show (encodeNum index) ++
           " in an array that only has " ++ show size ++ " positions"
  halt

arraySize :: (MonadMemory mem m) => String -> Runtime -> m Runtime
arraySize caller = readMem caller

arraySizeNum :: (MonadMemory mem m) => String -> Runtime -> m Int64
arraySizeNum caller a = readMem caller a >>= expectNum


-- TODO: refactor out safeMem or safeWithMem or whatever.
safeReadMem :: (MonadOutput m, MonadMemory mem m) => String -> Runtime -> Runtime -> m Runtime
safeReadMem caller p i = do
  size  <- arraySizeNum caller p
  index <- expectNum i
  if index < size
    then runOp p Increment i >>= readMem caller
    else arrayError p i

-- | sets the (arr[i] = e)
safeWriteMem :: (MonadOutput m, MonadMemory mem m) => String -> Runtime -> Runtime -> Runtime -> m ()
safeWriteMem caller p i v = do
  size  <- arraySizeNum caller p
  index <- expectNum i
  if index < size
    then runOp p Increment i >>= flip (writeMem caller) v
    else arrayError p i

freezeMem :: MonadST m => Memory (World m) -> m (Vector Runtime)
freezeMem m = liftST $ freeze (m^.runMemory)
