{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}

module L.Interpreter.Runtime where

import Control.Monad.Error.Class
import Data.Bits
import Data.Int
import L.Interpreter.Output
import L.L1.L1L2AST
import L.L3.L3AST

data Runtime = Num Int64 | Pointer Int64 | FunctionPointer Label deriving Eq

instance Show Runtime where
  show (Num i)             = "(Num " ++ show i ++ ")"
  show (Pointer i)         = "(Pointer " ++ show i ++ ")"
  show (FunctionPointer l) = "(FunctionPointer " ++ show l ++ ")"

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
  "cannot do " ++ show l ++ " " ++ x86OpSymbol op ++ " " ++ show r


mathOp :: MonadRuntime m => PrimName -> Runtime -> Runtime -> m Runtime
mathOp Add      (Num l) (Num r) = return . Num $ l + r
mathOp Sub      (Num l) (Num r) = return . Num $ l - r
mathOp Mult     (Num l) (Num r) = return . Num $ l * r
mathOp LessThan (Num l) (Num r) = return . boolToNum $ l <  r
mathOp LTorEQ   (Num l) (Num r) = return . boolToNum $ l <= r
mathOp EqualTo  (Num l) (Num r) = return . boolToNum $ l == r
mathOp b l r = exception $
  concat ["invalid arguments to ", show b, " :", show l, ", " , show r]

boolToNum :: Bool -> Runtime
boolToNum True  = Num 1
boolToNum False = Num 0

isNumber :: Runtime -> Runtime
isNumber (Num _) = lTrue
isNumber _       = lFalse

isArray :: Runtime -> Runtime
isArray = isPointer

-- TODO: should label return true here?
isPointer :: Runtime -> Runtime
isPointer (Pointer _)         = lTrue
isPointer (FunctionPointer _) = lTrue
isPointer _                   = lFalse

expectNum :: MonadRuntime m => Runtime -> m Int64
expectNum (Num i) = return i
expectNum r       = exception $ "expected a Num, but got: " ++ show r

expectPointer :: MonadRuntime m => String -> Runtime -> m Int64
expectPointer _ (Pointer i) = return i
expectPointer caller r      = exception $ caller ++ " expected Pointer, but got: " ++ show r

foldRuntime :: (Runtime -> a) -> (Runtime -> a) -> (Runtime -> a) -> Runtime -> a
foldRuntime fn fp fc = f where
  f n@(Num _)             = fn n
  f p@(Pointer _)         = fp p
  f c@(FunctionPointer _) = fc c
