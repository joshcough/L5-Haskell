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
import L.Interpreter.Memory
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
