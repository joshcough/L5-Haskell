{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}

module L.Interpreter.Runtime where

import Control.Monad.Error.Class
import Data.Bits
import Data.Foldable hiding (concat)
import Data.Int
import L.Interpreter.Output
import L.L1.L1L2AST
import L.L3.L3AST

data Runtime a = Num Int64 | Pointer Int64 | Runtime a deriving (Eq, Functor, Foldable, Show)

instance Monad Runtime where
  return = Runtime
  Num i     >>= _ = Num i
  Pointer i >>= _ = Pointer i
  Runtime a >>= f = f a

isPointer :: Runtime a -> Bool
isPointer (Pointer _) = True
isPointer _           = False

type MonadRuntime m = MonadError Halt m

lTrue, lFalse :: Runtime a
lTrue  = Num 1
lFalse = Num 0

--TODO: rename to x86Op
runOp :: (Show a, MonadRuntime m) => Runtime a -> X86Op -> Runtime a -> m (Runtime a)
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
runOp l op r  = exception $ "cannot do " ++ show l ++ " " ++ x86OpSymbol op ++ " " ++ show r

mathOp :: (Show a, MonadRuntime m) => PrimName -> Runtime a -> Runtime a -> m (Runtime a)
mathOp Add      (Num l) (Num r) = return . Num $ l + r
mathOp Sub      (Num l) (Num r) = return . Num $ l - r
mathOp Mult     (Num l) (Num r) = return . Num $ l * r
mathOp LessThan (Num l) (Num r) = return . boolToNum $ l <  r
mathOp LTorEQ   (Num l) (Num r) = return . boolToNum $ l <= r
mathOp EqualTo  (Num l) (Num r) = return . boolToNum $ l == r
mathOp b l r = exception $
  concat ["invalid arguments to ", show b, " :", show l, ", " , show r]

boolToNum :: Bool -> Runtime a
boolToNum True  = Num 1
boolToNum False = Num 0

isNumber :: Runtime a -> Runtime a
isNumber (Num _) = lTrue
isNumber _       = lFalse

isArray :: Runtime a -> Runtime a
isArray = boolToNum . isPointer

expectNum :: (Show a, MonadRuntime m) => Runtime a -> m Int64
expectNum (Num i) = return i
expectNum r       = exception $ "expected a Num, but got: " ++ show r

expectPointer :: (Show a, MonadRuntime m) => String -> Runtime a -> m Int64
expectPointer _ (Pointer i) = return i
expectPointer caller r      = exception $ caller ++ " expected Pointer, but got: " ++ show r
