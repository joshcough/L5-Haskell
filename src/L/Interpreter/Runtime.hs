{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}

module L.Interpreter.Runtime where

import Control.Monad.Error.Class
import Data.Int
import L.Interpreter.Output

data Runtime = Num Int64 | Pointer Int64 | FunctionPointer String deriving Eq

type MonadRuntime m = MonadError Halt m

addRuntimes :: MonadRuntime m => Runtime -> Runtime -> m Runtime
addRuntimes (Num l) (Num r)     = return . Num     $ l + r
addRuntimes (Num l) (Pointer r) = return . Pointer $ l + r
addRuntimes (Pointer l) (Num r) = return . Pointer $ l + r
addRuntimes l r  = exception $ "tried to add " ++ showRuntime l ++ " to " ++ showRuntime r

showRuntime :: Runtime -> String
showRuntime (Num i)             = "(Num " ++ show i ++ ")"
showRuntime (Pointer i)         = "(Pointer " ++ show i ++ ")"
showRuntime (FunctionPointer l) = "(FunctionPointer " ++ l ++ ")"

expectNum :: MonadRuntime m => Runtime -> m Int64
expectNum (Num i) = return i
expectNum r       = exception $ "expected a Num, but got: " ++ showRuntime r

expectPointer :: MonadRuntime m => Runtime -> m Int64
expectPointer (Pointer i) = return i
expectPointer r           = exception $ "expected Pointer, but got: " ++ showRuntime r
