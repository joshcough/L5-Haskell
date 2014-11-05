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

module L.Interpreter.X86Computer where

import Control.Arrow (second)
import Control.Lens hiding (set)
import Control.Monad.Error.Class
import Control.Monad.Reader
import Control.Monad.ST.Class
import Data.Int
import Data.List (intercalate)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Monoid()
import Data.Vector (Vector, freeze)
import qualified Data.Vector as Vector
import Prelude hiding (read)
import L.L1L2AST
import L.Interpreter.ComputationResult
import L.Interpreter.Memory
import L.Interpreter.Output
import L.Interpreter.Runtime

type RegisterState = Map Register Runtime
type Ip = Int64 -- instruction pointer

data X86Computer s a = X86Computer {
   _registers   :: RegisterState    -- contains runtime values (Int64 for L1/L2, but probably a data type for other languages)
 , _computerMem :: Memory s         -- "" (same as registers)
 , _program     :: Vector a         -- ok...non-linear programs (L3 and above) might have trouble here.
 , _labels      :: Map Label Int64  -- however, this could me a map from label to something else
 , _ip          :: Ip               -- Ip might mean nothing to L3 and above, but then again maybe theres a way to make use of it.
}
makeClassy ''X86Computer

data FrozenX86Computer a = FrozenX86Computer {
  frozenRegisters :: RegisterState
 ,frozenMemory    :: Vector Runtime
 ,frozenHeapP     :: Int64
 ,frozenIp        :: Ip
 ,frozenInst      :: a
}

-- TODO: we have the ability to distinguish between stdout and stderr
--       we shold be able to use that
--       but forcing us into a string here makes this difficult.
instance Show a => Show (ComputationResult (FrozenX86Computer a)) where
  -- no need to show anything other than the output, if halted normally.
  show (ComputationResult output (Halted Normal) _) = concat $ fmap outputText output
  show (ComputationResult output rs c) = intercalate "\n" [
    "Output:     " ++ concat (fmap outputText output),
    "Run State:  " ++ show rs,
    "Registers:  " ++ (show . Map.map showRuntime . Map.filter (Num 0 /=) $ frozenRegisters c),
    "Memory!=0:  " ++ memDisplay,
    "Heap Ptr:   " ++ show (frozenHeapP c),
    "Inst Ptr:   " ++ show (frozenIp c),
    "Final Inst: " ++ show (frozenInst c) ] where
    memDisplay = show . map (second showRuntime) $ memList
    memList :: [(Int, Runtime)]
    memList = filter (\(_,r) -> Num 0 /= r) . zip [0..] . Vector.toList $ frozenMemory c

instance HasMemory (X86Computer s a) s where memory = computerMem

freezeX86Computer :: MonadST m => X86Computer (World m) a -> m (FrozenX86Computer a)
freezeX86Computer c = do
  m <- liftST $ freeze (c^.computerMem^.runMemory)
  return $ FrozenX86Computer (c^.registers) m (c^.computerMem^.heapP) (c^.ip) ((c^.program) Vector.! fromIntegral (c^.ip))

type MonadX86Computer c m a = (HasX86Computer c (World m) a, MonadMemory c m)

rspStart :: Int64
rspStart = fromIntegral memSize * 8

registerStartState :: Map Register Runtime
registerStartState = Map.insert rsp (Pointer rspStart) $ Map.fromList $ zip registersList (repeat $ Num 0)

newX86Computer :: (MonadST m, Show x, Show s) => Program x s -> m (X86Computer (World m) (Instruction x s))
newX86Computer p = do
  m <-  newMem (MemoryConfig True True) -- encoded numbers, word indexed
  return X86Computer {
    _registers   = registerStartState,
    _computerMem = m,
    _program     = Vector.fromList insts,
    _labels      = Map.map fromIntegral $ labelIndices insts,
    _ip          = 0
  } where insts = programToList p

-- set the value of a register to an int value
writeReg :: MonadX86Computer c m a => Register -> Runtime -> m ()
writeReg reg newValue = registers.at reg ?= newValue

-- set the value of r1 to the value of r2
set :: MonadX86Computer c m a => Register -> Register -> m ()
set r1 r2 = (registers.at r1) <~ use (registers.at r2)

-- read the value of a register
readReg :: MonadX86Computer c m a => Register -> m Runtime
readReg r = use (registers.at r) >>=
  maybe (throwError . Exceptional $ "error: unitialized register: " ++ show r) return

-- push the given int argument onto the top of the stack
-- adjust rsp accordingly
-- from: http://www.cs.virginia.edu/~evans/cs216/guides/x86.html
-- "push first decrements ESP by 4, then places its operand 
--  into the contents of the 32-bit location at address [ESP]."
push :: MonadX86Computer c m a => Runtime -> m ()
push value = do
  rspVal  <- readReg rsp >>= expectPointer "push"
  writeReg rsp (Pointer $ rspVal - 8)
  writeMem "push" (Pointer $ rspVal - 8) value

-- pop the top value off the stack into the given register
-- adjust rsp accordingly.
pop :: MonadX86Computer c m a => Register -> m ()
pop r = do
  rspVal  <- readReg rsp
  rspValN <- expectPointer "pop" rspVal
  s       <- readMem "pop" rspVal
  writeReg r s
  writeReg rsp (Pointer $ rspValN + 8)

findLabelIndex :: MonadX86Computer c m a => Label -> m Int64
findLabelIndex l = use (labels.at l) >>= maybe (exception $ "no such label: " ++ l) return

goto :: MonadX86Computer c m a => Runtime -> m ()
goto (Num i) = ip .= i
goto (Pointer p) = exception $ "goto called with pointer: " ++ show p
goto (FunctionPointer l) = do i <- findLabelIndex l; ip .= i

currentInst :: MonadX86Computer c m a => m a
currentInst = do
  ip' <- uses ip fromIntegral
  p   <- use program
  when (ip' >= memSize || ip' < 0) halt
  return $ p Vector.! fromIntegral ip'

hasNextInst :: MonadX86Computer c m a => m Bool
hasNextInst = do
  ip' <- use ip
  p   <- use program
  return $ ip' < fromIntegral (Vector.length p)

-- advance the computer to the next instruction
nextInst :: MonadX86Computer c m a => m ()
nextInst = ip += 1

-- the main loop, runs a computer until completion
runX86Computer :: (MonadOutput m, MonadX86Computer c m a) => (a -> m ()) -> m ()
runX86Computer step = forever $ currentInst >>= step
