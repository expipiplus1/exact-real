{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Main (main) where

import Data.List (inits)
import Data.Maybe (fromJust)
import Data.Ratio ((%))
import Test.Tasty (testGroup, TestTree)
import Test.Tasty.QuickCheck (Positive(..), testProperty, (===), Property, (==>), (.&&.), testProperty)
import Test.Tasty.TH (defaultMainGenerator)
import Test.Tasty.HUnit (Assertion, (@=?), testCase)

import Data.CReal.Converge
import Data.CReal.Internal
import Data.CReal.Extra ()

import BoundedFunctions (boundedFunctions)
import Floating (floating)
import Ord (ord)
import Read (read')
import Real (real)
import RealFrac (realFrac)
import RealFloat (realFloat)
import Random (random)

-- How many binary digits to use for comparisons TODO: Test with many different
-- precisions
type Precision = 10

{-# ANN test_floating "HLint: ignore Use camelCase" #-}
test_floating :: [TestTree]
test_floating = [floating (undefined :: CReal Precision)]

{-# ANN test_ord "HLint: ignore Use camelCase" #-}
test_ord :: [TestTree]
test_ord = [ ord (undefined :: CReal Precision) ]

{-# ANN test_real "HLint: ignore Use camelCase" #-}
test_real :: [TestTree]
test_real = [ real (\x -> 1 % toInteger (max 1 (crealPrecision (x::CReal Precision)))) ]

{-# ANN test_realFrac "HLint: ignore Use camelCase" #-}
test_realFrac :: [TestTree]
test_realFrac = [ realFrac (undefined :: CReal Precision) ]

{-# ANN test_realFloat "HLint: ignore Use camelCase" #-}
test_realFloat :: [TestTree]
test_realFloat = [ realFloat (undefined :: CReal Precision) ]

{-# ANN test_read "HLint: ignore Use camelCase" #-}
test_read :: [TestTree]
test_read = [ read' (undefined :: CReal Precision) ]

{-# ANN test_random "HLint: ignore Use camelCase" #-}
test_random :: [TestTree]
test_random = [ random (undefined :: CReal Precision) ]

prop_decimalDigits :: Positive Int -> Property
prop_decimalDigits (Positive p) = let d = decimalDigitsAtPrecision p
                                  in 10^d >= (2^p :: Integer) .&&.
                                     (d > 0 ==> 10^(d-1) < (2^p :: Integer))

prop_showIntegral :: Integer -> Property
prop_showIntegral i = show i === show (fromInteger i :: CReal 0)

prop_shiftL :: CReal Precision -> Int -> Property
prop_shiftL x s = x `shiftL` s === x * 2 ** fromIntegral s

prop_shiftR :: CReal Precision -> Int -> Property
prop_shiftR x s = x `shiftR` s === x / 2 ** fromIntegral s

prop_showNumDigits :: Positive Int -> Rational -> Property
prop_showNumDigits (Positive places) x =
  let s = rationalToDecimal places x
  in length (dropWhile (/= '.') s) === places + 1

--
-- Testing Data.CReal.Converge
--

case_convergeErrEmptyCReal :: Assertion
case_convergeErrEmptyCReal = convergeErr undefined [] @=? (Nothing :: Maybe (CReal 0))

case_convergeErrEmptyUnit :: Assertion
case_convergeErrEmptyUnit = convergeErr undefined [] @=? (Nothing :: Maybe ())

case_convergeEmptyCReal :: Assertion
case_convergeEmptyCReal = converge [] @=? (Nothing :: Maybe (CReal 0))

case_convergeEmptyUnit :: Assertion
case_convergeEmptyUnit = converge [] @=? (Nothing :: Maybe ())

prop_convergeCollatzInteger :: Positive Integer -> Property
prop_convergeCollatzInteger (Positive x) = converge (iterate collatz x) === Just 1
  where collatz :: Integer -> Integer
        collatz c | c == 1 = 1
                  | even c = c `div` 2
                  | otherwise = c * 3 + 1


case_convergePointNineRecurringCReal :: Assertion
case_convergePointNineRecurringCReal = (Just 1 :: Maybe (CReal Precision)) @=?
                                       converge (read <$> pointNineRecurring)
  where pointNineRecurring = ("0.9" ++) <$> inits (repeat '9')

prop_convergeErrSqrtCReal :: Positive (CReal Precision) -> Property
prop_convergeErrSqrtCReal (Positive x) = sqrt' (x ^ (2::Int)) === x
  where sqrt' x' = let initialGuess = x'
                       improve y = (y + x' / y) / 2
                       err y = abs (x' - y * y)
                   in fromJust $ convergeErr err (tail $ iterate improve initialGuess)

-- Test that the behavior when error is too small is correct
prop_convergeErrSmallSqrtCReal :: Positive (CReal Precision) -> Property
prop_convergeErrSmallSqrtCReal (Positive x) = sqrt' (x ^ (2::Int)) === x
  where sqrt' x' = let initialGuess = x'
                       improve y = (y + x' / y) / 2
                       err y = abs (x' - y * y) / 128
                   in fromJust $ convergeErr err (tail $ iterate improve initialGuess)

prop_convergeErrSqrtInteger :: Positive Integer -> Property
prop_convergeErrSqrtInteger (Positive x) = sqrt' (x ^ (2::Int)) === x
  where sqrt' x' = let initialGuess = x'
                       improve y = (y + x' `quot` y) `quot` 2
                       err y = abs (x' - y * y)
                   in fromJust $ convergeErr err (tail $ iterate improve initialGuess)

--
--
--

{-# ANN test_boundedFunctions "HLint: ignore Use camelCase" #-}
test_boundedFunctions :: [TestTree]
test_boundedFunctions = [ boundedFunctions (undefined :: CReal Precision) ]

prop_expPosNeg :: CReal Precision -> Property
prop_expPosNeg x = expPosNeg x === (exp x, exp (-x))

prop_square :: CReal Precision -> Property
prop_square x = square x === x * x

main :: IO ()
main = $(defaultMainGenerator)

