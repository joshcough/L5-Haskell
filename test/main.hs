{-# LANGUAGE GADTs #-}

module Main where

import Test.Framework.Runners.Console
import Test.Framework.Providers.API
import Test.Framework.Providers.HUnit
import Test.HUnit

import Data.List
import Data.Traversable

--import LProperties
import LUnitTests

main = do
  unitTests <- LUnitTests.tests
  defaultMain [unitTests] --, LProperties.tests]

