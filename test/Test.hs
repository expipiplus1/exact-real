{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DataKinds #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Main (main) where

import Test.Tasty (testGroup, TestTree)
import Test.Tasty.TH (defaultMainGenerator)
import Test.Tasty.QuickCheck (Positive(..), testProperty)

import Data.CReal.Internal
import Data.CReal.Extra ()

import Fractional (fractional)

-- How many binary digits to use for comparisons TODO: Test with many different
-- precisions
type Precision = 10

infixr 1 ==>
(==>) :: Bool -> Bool -> Bool
False ==> _ = True
True ==> b = b

{-# ANN test_fractional "HLint: ignore Use camelCase" #-}
test_fractional :: [TestTree]
test_fractional = [fractional (undefined :: CReal Precision)]

prop_decimalDigits :: Positive Int -> Bool
prop_decimalDigits (Positive p) = let d = decimalDigitsAtPrecision p
                                  in 10^d >= (2^p :: Integer) &&
                                     (d > 0 ==> 10^(d-1) < (2^p :: Integer))

prop_showIntegral :: Integer -> Bool
prop_showIntegral i = show i == show (fromInteger i :: CReal 0)

main :: IO ()
main = $(defaultMainGenerator)

