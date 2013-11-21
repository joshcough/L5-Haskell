{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE LiberalTypeSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import Test.QuickCheck
import Test.QuickCheck.Function
import Test.QuickCheck.Instances
import Test.Framework
import Test.Framework.TH
import Test.Framework.Providers.QuickCheck2

import L1Tests

-- | /NB:/ when adding a test suite here, make sure you add it to
-- -- the @other-modules:@ block under @test-suite properties@ in
-- -- @ermine.cabal@ or you'll break @cabal sdist@.
main :: IO ()
main = defaultMain [L1Tests.tests]
