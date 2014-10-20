{-# LANGUAGE GADTs #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module L.CompilerWithThrists where

import Control.Applicative
import Control.Category
import Data.Thrist
import L.LCompiler
import L.L1L2AST
import L.L1.L1
import L.L2.L2
import Prelude hiding ((.),id)

x86CompilerThrist :: Thrist (LCompiler m) String String
x86CompilerThrist = id

l1CompilerThrist :: CompilerMonad m => Thrist (LCompiler m) L1 String
l1CompilerThrist = Cons l1Compiler x86CompilerThrist

l2CompilerThrist :: CompilerMonad m => Thrist (LCompiler m) L2 String
l2CompilerThrist = Cons l2Compiler l1CompilerThrist

class Cat t where
  foldCat :: Category q => (forall a b. p a b -> q a b) -> t p a b -> q a b
  mapCat :: (forall a b. p a b -> q a b) -> t p a b -> t q a b
  traverseCat :: Applicative m => (forall a b. p a b -> m (q a b)) -> t p a b -> m (t q a b)

instance Cat Thrist where
  foldCat _ Nil = id
  foldCat f (Cons x xs) =  foldCat f xs . f x
  mapCat _ Nil = Nil
  mapCat f (Cons x xs) = Cons (f x) (mapCat f xs)
  traverseCat _ Nil = pure Nil
  traverseCat f (Cons x xs) = Cons <$> f x <*> traverseCat f xs

l2c :: CompilerMonad m => LCompiler m L2 String
l2c = foldCat id l2CompilerThrist

{-
instance ProfunctorMonad Thrist where
  proreturn p = Cons p Nil
  projoin ttp = error "todo"

l2l1CThrist ::  CompilerMonad m => Thrist (LCompiler m) L2 L1
l2l1CThrist = proreturn l2Compiler

l3l1CThrist ::  CompilerMonad m => Thrist (LCompiler m) L2 L1
l3l1CThrist = proreturn l2Compiler . proreturn l3Compiler

import Prelude hiding ((.),id)

data Thrist l i k where
  Nil :: Thrist l i i
  Snoc :: Thrist l j k -> l i j -> Thrist l i k

instance Category (Thrist l) where
  id = Nil
  xs . Nil = xs
  xs . Snoc ys y = undefined
  -- ...



foo :: Language i o -> Kleisli m i o
runKleisli (foldCat foo) :: Thrist Language i o -> i -> m o

foldCat foo :: Thrist Language i o -> i -> o

data Language i o where
  Language :: -- (Show i, Show o) =>
    Parser i      ->
    Compiler  i o ->
    Interpreter i ->
    Extension     ->
    Language i o

instance Functor (Language i) where
instance Semigroupoid Language where
  Language _ c' _ _ `o` Language p c i e = Language p c'' i e where
     c'' = c >=> c'
-}