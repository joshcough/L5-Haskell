{-# LANGUAGE TemplateHaskell #-}
{--
--http://stackoverflow.com/questions/2259926/testing-io-actions-with-monadic-quickcheck
module LProperties where

import Test.QuickCheck
import Test.QuickCheck.Function
import Test.QuickCheck.Instances
import Test.QuickCheck.Monadic (assert, monadicIO, pick, pre, run)
import Test.Framework.TH
import Test.Framework.Providers.QuickCheck2

prop_list_reverse_reverse :: [Int] -> Bool
prop_list_reverse_reverse list = list == reverse (reverse list)

tests = $testGroupGenerator
--}
