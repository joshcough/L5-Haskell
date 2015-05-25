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
import L.L1.L1L2AST
import L.Util.Utils

data MemoryConfig = MemoryConfig {
  _encoded     :: Bool
 ,_wordIndexed :: Bool -- True if you have to multiply by 8...TODO document more
}
makeClassy ''MemoryConfig

data Memory s a = Memory {
  _runMemory :: STVector s (Runtime a)
 ,_heapP     :: Int64 -- pointer to top of heap
 ,_memConfig :: MemoryConfig
}
makeClassy ''Memory

instance HasMemoryConfig (Memory s a) where memoryConfig = memConfig

type MonadMemory mem m a = (Functor m, MonadST m, MonadError Halt m, MonadState mem m, HasMemory mem (World m) a, Show a)

oneMeg, twoMeg, memSize :: Int64
oneMeg = 1048576
twoMeg = oneMeg * 2
memSize = twoMeg
zero :: Int64
zero = 0
memSizeAsInt :: Int
memSizeAsInt = fromIntegral memSize

newMem :: MonadST m => MemoryConfig -> m (Memory (World m) a)
newMem config = liftST $ Memory <$> MV.replicate memSizeAsInt (Num zero) <*> pure 0 <*> pure config

divideIndex :: MonadMemory mem m a => Int64 -> m Int64
divideIndex = withMemIndex div

multiplyIndex :: MonadMemory mem m a => Int64 -> m Int64
multiplyIndex = withMemIndex (*)

withMemIndex :: MonadMemory mem m a => (Int64 -> Int64 -> Int64) -> Int64 -> m Int64
withMemIndex f i = do m <- use memory; return $ if (m^.memoryConfig^.wordIndexed) then f i 8 else i

-- write an int into memory at the given address
writeMem :: MonadMemory mem m a => String -> Runtime a -> Runtime a -> m ()
writeMem caller addr value = do
  p     <- expectPointer (caller ++ "/writeMem") addr
  index <- divideIndex p
  if index < memSize
    then do m <- use memory; liftST $ write (m^.runMemory) (fromIntegral index) value
    else exception $ caller ++ "tried to write out of bounds memory index: " ++ show index

-- read a single int from memory
readMem :: MonadMemory mem m a => String -> Runtime a -> m (Runtime a)
readMem caller addr = do
  p     <- expectPointer (caller ++ "/readMem") addr
  index <- divideIndex p
  if index < memSize
    then do m <- use memory; liftST $ read (m^.runMemory) (fromIntegral index)
    else exception $ caller ++ "tried to access out of bounds memory index: " ++ show index

-- read an array from memory
readArray :: MonadMemory mem m a => Runtime a -> m (STVector (World m) (Runtime a))
readArray addr = do
  p          <- expectPointer "readArray" addr
  startIndex <- divideIndex p <&> (+ 1)
  size       <- evalAddrToNum "readArray" p
  if startIndex + size < memSize
    then uses memory (MV.slice (fromIntegral startIndex) (fromIntegral size) . _runMemory)
    else exception $ "readArray tried to access out of bounds memory index: " ++ show startIndex

evalAddrToNum :: MonadMemory mem m a => String -> Int64 -> m Int64
evalAddrToNum caller addr = readMem caller (Pointer addr) >>= expectNum

-- TODO: test if heap runs into stack, and vice-versa
-- NOTE: allocate is Haskell's replicate
allocate :: (MonadState mem m, MonadMemory mem m a) => Runtime a -> Runtime a -> m (Runtime a)
allocate size r = do
  size' <- expectNum size
  newArray $ Prelude.replicate (fromIntegral size') r

newArray :: (MonadState mem m, MonadMemory mem m a) => [Runtime a] -> m (Runtime a)
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
print :: forall mem m a. (MonadOutput m, MonadMemory mem m a, Show a) => Bool -> Runtime a -> m (Runtime a)
print encoded n = printContent 0 n >>= \s -> stdOut (s ++ "\n") >> return (Num 1) where
  loop v depth index
    | index >= MV.length v = return []
    | otherwise = do
      h <- liftST (read v index) >>= printContent depth
      t <- loop v depth (index + 1)
      return $ h : t
  printContent :: Int -> Runtime a -> m String
  printContent depth r
    | depth >= 4   = return "..."
    | otherwise = case r of
      Num i         -> return . show $ if encoded then shiftR i 1 else i
      p@(Pointer _) -> do
        size      <- arraySizeNum "print" p
        arr       <- readArray p
        contentsV <- loop arr depth 0
        return $ "{s:" ++ mkString ", " (show size : contentsV) ++ "}"
      Runtime bad -> exception $ "impossible to print: " ++ show bad

-- print an array error
arrayError :: (MonadOutput m, MonadMemory mem m a) => Runtime a -> Runtime a -> m (Runtime a)
arrayError a x = do
  size  <- arraySizeNum "arrayError" a
  index <- expectNum x
  stdErr $ concat [
    "attempted to use position ", show index, " in an array that only has ", show size, " positions"]
  halt

arraySize :: MonadMemory mem m a => String -> Runtime a -> m (Runtime a)
arraySize caller = readMem caller

arraySizeNum :: MonadMemory mem m a => String -> Runtime a -> m Int64
arraySizeNum caller a = readMem caller a >>= expectNum

-- TODO: refactor out safeMem or safeWithMem or whatever.
safeReadMem :: (MonadOutput m, MonadMemory mem m a) => String -> Runtime a -> Runtime a -> m (Runtime a)
safeReadMem caller p i = do
  size  <- arraySizeNum caller p
  index <- expectNum i <&> (+1)
  if index <= size
    then runOp p Increment (Num index) >>= readMem caller
    else arrayError p (Num $ index-1)

-- | sets the (arr[i] = e)
safeWriteMem :: (MonadOutput m, MonadMemory mem m a) => String -> Runtime a -> Runtime a -> Runtime a -> m ()
safeWriteMem caller p i v = do
  size  <- arraySizeNum caller p
  index <- expectNum i <&> (+1)
  if index <= size
    then runOp p Increment (Num index) >>= flip (writeMem caller) v
    else arrayError p (Num $ index-1) >> return ()

-- | array reference (arr[i])
arrayRef :: (MonadOutput m, MonadMemory mem m a) => Runtime a -> Runtime a -> m (Runtime a)
arrayRef arr i = safeReadMem "L4-aref" arr i

-- | sets the (arr[i] = e)
arraySet :: (MonadOutput m, MonadMemory mem m a) => Runtime a -> Runtime a -> Runtime a -> m (Runtime a)
arraySet arr i r = safeWriteMem "Memory Array set" arr i r >> return lTrue

freezeMem :: MonadST m => Memory (World m) a -> m (Vector (Runtime a))
freezeMem m = liftST $ freeze (m^.runMemory)
