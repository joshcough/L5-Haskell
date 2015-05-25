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

module L.Interpreter.ComputationResult where

import Control.Monad.Reader
import Data.Monoid()
import Prelude hiding (read)
import L.Interpreter.Output

data ComputationResult a = ComputationResult {
   _output    :: [Output]
  ,_haltState :: RunState
  ,_internals :: a
} deriving (Functor)

-- TODO: this ignores the final result. should something be done about that?
mkComputationResult :: ([Output], (Either Halt b, a)) -> ComputationResult a
mkComputationResult (o, (Left  h , c)) = ComputationResult o (Halted h) c
mkComputationResult (o, (Right _, c))  = ComputationResult o Running c
