{-# LANGUAGE GADTs #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}

module L.CompilerWithThrists where

import Data.Thrist
import L.LCompiler
import L.L1L2AST
import L.L1.L1
import L.L2.L2

x86CompilerThrist :: CompilerMonad m => Thrist LCompiler (m String) (m String)
x86CompilerThrist = Cons (LCompiler $ \_ _ -> id) Nil

l1CompilerThrist :: CompilerMonad m => Thrist LCompiler (m L1) (m String)
l1CompilerThrist = Cons l1Compiler x86CompilerThrist

l2CompilerThrist :: CompilerMonad m => Thrist LCompiler (m L2) (m String)
l2CompilerThrist = Cons l2Compiler l1CompilerThrist

xxx = l2CompilerThrist _wat


{-
import Prelude hiding ((.),id)

data Thrist l i k where
  Nil :: Thrist l i i
  Snoc :: Thrist l j k -> l i j -> Thrist l i k

instance Category (Thrist l) where
  id = Nil
  xs . Nil = xs
  xs . Snoc ys y = undefined
  -- ...

class Cat t where
  foldCat :: Category q => (forall a b. p a b -> q a b) -> t p a b -> q a b
  mapCat :: (forall a b. p a b -> q a b) -> t p a b -> t q a b
  traverseCat :: Applicative m => (forall a b. p a b -> m (q a b)) -> t p a b -> m (t q a b)

instance Cat Thrist where
  foldCat f Nil = id
  foldCat f (Snoc xs x) = foldCat f xs . f x
  mapCat f Nil = Nil
  mapCat f (Snoc xs x) = Snoc (mapCat f xs) (f x)
  traverseCat f Nil = pure Nil
  traverseCat f (Snoc xs x) = Snoc <$> traverseCat f xs <*> f x

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