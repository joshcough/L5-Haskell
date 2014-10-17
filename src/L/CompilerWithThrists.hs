{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-
module L.Compiler where

import Control.Applicative
import Control.Category
import Data.Maybe
import L.L1.L1Interp (Computer, showComputerOutput)
import L.IOHelpers
import L.Read
import L.NativeRunner
import Debug.Trace
import Prelude hiding ((.),id)

type Val a            = Either String a
type Parser         i = SExpr -> Val i
type Compiler     i o = i -> Val o
type Interpreter    i = i -> Computer
type Extension        = String

data Language i o where
  Language :: (Show i, Show o) =>
    Parser i      ->
    Compiler  i o ->
    Interpreter i ->
    Maybe (Language o a) ->
    Language i o

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
