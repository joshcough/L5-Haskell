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
import Debug.Trace
import Prelude hiding (read)
import L.Interpreter.Output
import L.Interpreter.Runtime
import L.L1L2AST
import L.Utils

data MemoryConfig = MemoryConfig {
  _encoded     :: Bool
 ,_wordIndexed :: Bool -- True if you have to multiply by 8...TODO document more
}
makeClassy ''MemoryConfig

data Memory s = Memory {
  _runMemory :: STVector s Runtime
 ,_heapP     :: Int64 -- pointer to top of heap
 ,_memConfig :: MemoryConfig
}
makeClassy ''Memory

instance HasMemoryConfig (Memory s) where memoryConfig = memConfig

type MonadMemory mem m = (Functor m, MonadST m, MonadError Halt m, MonadState mem m, HasMemory mem (World m))

oneMeg, twoMeg, memSize :: Int64
oneMeg = 1048576
twoMeg = oneMeg * 2
memSize = twoMeg
zero :: Int64
zero = 0
memSizeAsInt :: Int
memSizeAsInt = fromIntegral memSize

newMem :: MonadST m => MemoryConfig -> m (Memory (World m))
newMem config = liftST $ Memory <$> MV.replicate memSizeAsInt (Num zero) <*> pure 0 <*> pure config

divideIndex :: MonadMemory mem m => Int64 -> m Int64
divideIndex = withMemIndex div

multiplyIndex :: MonadMemory mem m => Int64 -> m Int64
multiplyIndex = withMemIndex (*)

withMemIndex :: MonadMemory mem m => (Int64 -> Int64 -> Int64) -> Int64 -> m Int64
withMemIndex f i = do m <- use memory; return $ if (m^.memoryConfig^.wordIndexed) then f i 8 else i

-- write an int into memory at the given address
writeMem :: MonadMemory mem m => String -> Runtime -> Runtime -> m ()
writeMem caller addr value = do
  p     <- expectPointer (caller ++ "/writeMem") addr
  index <- divideIndex p
  if index < memSize
    then do m <- use memory; liftST $ write (m^.runMemory) (fromIntegral index) value
    else exception $ caller ++ "tried to write out of bounds memory index: " ++ show index

-- read a single int from memory
readMem :: MonadMemory mem m => String -> Runtime -> m Runtime
readMem caller addr = do
  p     <- expectPointer (caller ++ "/readMem") addr
  index <- divideIndex p
  if index < memSize
    then do m <- use memory; liftST $ read (m^.runMemory) (fromIntegral index)
    else exception $ caller ++ "tried to access out of bounds memory index: " ++ show index

-- read an array from memory
readArray :: MonadMemory mem m => Runtime -> m (STVector (World m) Runtime)
readArray addr = do
  p          <- expectPointer "readArray" addr
  startIndex <- divideIndex p <&> (+ 1)
  size       <- evalAddrToNum "readArray" p
  if startIndex + size < memSize
    then uses memory (MV.slice (fromIntegral startIndex) (fromIntegral size) . _runMemory)
    else exception $ "readArray tried to access out of bounds memory index: " ++ show startIndex

evalAddrToNum :: MonadMemory mem m => String -> Int64 -> m Int64
evalAddrToNum caller addr = readMem caller (Pointer addr) >>= expectNum

-- TODO: test if heap runs into stack, and vice-versa
-- NOTE: allocate is Haskell's replicate
allocate :: (MonadState mem m, MonadMemory mem m) => Runtime -> Runtime -> m Runtime
allocate size r = do
  size' <- expectNum size
  newArray $ Prelude.replicate (fromIntegral size') r

newArray :: (MonadState mem m, MonadMemory mem m) => [Runtime] -> m Runtime
newArray rs = do
  hp    <- use heapP
  index <- divideIndex hp
  let size = fromIntegral $ Prelude.length rs
      indices = [(fromIntegral index)..]
      rsWithIndices = Prelude.zip indices $ Num size : rs
  do m <- use memory; liftST $ update (m^.runMemory) (fromList rsWithIndices)
  heapSpaceUsed <- multiplyIndex (size+1)
  heapP += heapSpaceUsed
  return $ Pointer hp

-- print a number or an array
--   if argument is an int, prints the int
--   else it's an array, print the contents of the array (and recur)
print :: forall mem m. (MonadOutput m, MonadMemory mem m) => Bool -> Runtime -> m Runtime
print encoded n = printContent 0 n >>= \s -> stdOut (s ++ "\n") >> return (Num 1) where
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
      Num i         -> return . show $ if encoded then shiftR i 1 else i
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
  stdErr $ concat [
    "attempted to use position ", show index, " in an array that only has ", show size, " positions"]
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
  if index <= size
    then runOp p Increment i >>= readMem caller
    else arrayError p i

-- | sets the (arr[i] = e)
safeWriteMem :: (MonadOutput m, MonadMemory mem m) => String -> Runtime -> Runtime -> Runtime -> m ()
safeWriteMem caller p i v = do
  size  <- arraySizeNum caller p
  index <- expectNum i
  if index <= size
    then runOp p Increment i >>= flip (writeMem caller) v
    else arrayError p i

freezeMem :: MonadST m => Memory (World m) -> m (Vector Runtime)
freezeMem m = liftST $ freeze (m^.runMemory)
