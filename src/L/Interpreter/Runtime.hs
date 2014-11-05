{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}

module L.Interpreter.Runtime where

import Control.Monad.Error.Class
import Data.Bits
import Data.Int
import L.Interpreter.Output
import L.L1L2AST
import L.L3.L3AST

data Runtime = Num Int64 | Pointer Int64 | FunctionPointer String deriving Eq

type MonadRuntime m = MonadError Halt m

lTrue, lFalse :: Runtime
lTrue  = Num 1
lFalse = Num 0

--TODO: rename to x86Op
runOp :: MonadRuntime m => Runtime -> X86Op -> Runtime -> m Runtime
runOp (Num l)     Increment  (Num r)      = return $ Num     (l + r)
runOp (Num l)     Increment  (Pointer r)  = return $ Pointer (l + r)
runOp (Pointer l) Increment  (Num r)      = return $ Pointer (l + r)
runOp (Num l)     Decrement  (Num r)      = return $ Num     (l - r)
runOp (Pointer l) Decrement  (Num r)      = return $ Pointer (l - r)
runOp (Num l)     Multiply   (Num r)      = return $ Num     (l * r)
runOp (Num i)     LeftShift  (Num amount) = return $ Num $   shiftL i (fromIntegral amount)
runOp (Num i)     RightShift (Num amount) = return $ Num $   shiftR i (fromIntegral amount)
runOp (Num l)     BitwiseAnd (Num r)      = return $ Num     (l .&. r)
-- If you're going to mess with pointers like this, you're just going to get back a Num.
runOp (Pointer l) BitwiseAnd (Num r)      = return $ Num     (l .&. r)
runOp l op r  = exception $
  "cannot do " ++ showRuntime l ++ " " ++ x86OpSymbol op ++ " " ++ showRuntime r


mathOp :: MonadRuntime m => PrimName -> Runtime -> Runtime -> m Runtime
mathOp Add      (Num l) (Num r) = return . Num $ l + r
mathOp Sub      (Num l) (Num r) = return . Num $ l - r
mathOp Mult     (Num l) (Num r) = return . Num $ l * r
mathOp LessThan (Num l) (Num r) = return . boolToNum $ l <  r
mathOp LTorEQ   (Num l) (Num r) = return . boolToNum $ l <= r
mathOp EqualTo  (Num l) (Num r) = return . boolToNum $ l == r
mathOp b l r = exception $
  concat ["invalid arguments to ", show b, " :", showRuntime l, ", " , showRuntime r]

boolToNum :: Bool -> Runtime
boolToNum True  = Num 1
boolToNum False = Num 0


showRuntime :: Runtime -> String
showRuntime (Num i)             = "(Num " ++ show i ++ ")"
showRuntime (Pointer i)         = "(Pointer " ++ show i ++ ")"
showRuntime (FunctionPointer l) = "(FunctionPointer " ++ l ++ ")"

isNumber :: Runtime -> Runtime
isNumber (Num _) = lTrue
isNumber _       = lFalse

-- TODO: should label return true here?
isPointer :: Runtime -> Runtime
isPointer (Pointer _) = lTrue
isPointer _           = lFalse

expectNum :: MonadRuntime m => Runtime -> m Int64
expectNum (Num i) = return i
expectNum r       = exception $ "expected a Num, but got: " ++ showRuntime r

expectPointer :: MonadRuntime m => String -> Runtime -> m Int64
expectPointer _ (Pointer i) = return i
expectPointer caller r      = exception $ caller ++ " expected Pointer, but got: " ++ showRuntime r

foldRuntime :: (Runtime -> a) -> (Runtime -> a) -> (Runtime -> a) -> Runtime -> a
foldRuntime fn fp fc = f where
  f n@(Num _)             = fn n
  f p@(Pointer _)         = fp p
  f c@(FunctionPointer _) = fc c
