{-# LANGUAGE MagicHash #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}

module Data.CReal.Internal
  ( CReal(..)
  , atPrecision
  , crealPrecision

  , (/.)
  , log2
  , log10
  , decimalDigitsAtPrecision
  , rationalToDecimal
  ) where

import Data.Ratio (numerator,denominator,(%))
import GHC.Base (Int(..))
import GHC.Integer.Logarithms (integerLog2#, integerLogBase#)
import GHC.TypeLits

-- $setup
-- >>> :set -XDataKinds

infixl 7 /.

default ()

-- | The type CReal represents a fast binary Cauchy sequence. This is
-- a Cauchy sequence with the property that the pth element will be within
-- 2^-p of the true value. Internally this sequence is represented as
-- a function from Ints to Integers.
newtype CReal (n :: Nat) = CR (Int -> Integer)

-- | crealPrecision x returns the type level parameter representing x's default
-- precision.
--
-- >>> crealPrecision (1 :: CReal 10)
-- 10
crealPrecision :: KnownNat n => CReal n -> Int
crealPrecision = fromInteger . natVal

-- | @x \`atPrecision\` p@ returns the numerator of the pth element in the
-- Cauchy sequence represented by x. The denominator is 2^p.
--
-- >>> 10 `atPrecision` 10
-- 10240
atPrecision :: CReal n -> Int -> Integer
(CR x) `atPrecision` p = x p

-- | A CReal with precision p is shown as a decimal number d such that d is
-- within 2^-p of the true value.
--
-- >>> show (47176870 :: CReal 0)
-- "47176870"
instance KnownNat n => Show (CReal n) where
  show x = showAtPrecision (crealPrecision x) x

-- | @signum (x :: CReal p)@ returns the sign of @x@ at precision @p@. It's
-- important to remember that this /may not/ represent the actual sign of @x@ if
-- the distance between @x@ and zero is less than 2^-@p@.
--
-- This is a little bit of a fudge, but it's probably better than failing to
-- terminate when trying to find the sign of zero.
--
-- >>> signum (0.1 :: CReal 2)
-- 0.0
instance Num (CReal n) where
  fromInteger i = CR (\p -> i * 2 ^ p)

  negate (CR x) = CR (negate . x)

  abs (CR x) = CR (abs . x)

  {-# INLINE (+) #-}
  CR x1 + CR x2 = CR (\p -> let n1 = x1 (p + 2)
                                n2 = x2 (p + 2)
                            in (n1 + n2) /. 4)

  {-# INLINE (*) #-}
  CR x1 * CR x2 = CR (\p -> let s1 = log2 (abs (x1 0) + 2) + 3
                                s2 = log2 (abs (x2 0) + 2) + 3
                                n1 = x1 (p + s2)
                                n2 = x2 (p + s1)
                            in (n1 * n2) /. 2^(p + s1 + s2)  )

  signum x = CR (\p -> signum (x `atPrecision` p) * 2^p)

-- | Taking the reciprocal of zero will not terminate
instance Fractional (CReal n) where
  -- This should be in base
  fromRational n = fromInteger (numerator n) / fromInteger (denominator n)

  {-# INLINE recip #-}
  -- TODO: Make recip 0 throw an error (if, for example, it would take more
  -- than 4GB of memory to represent the result)
  recip (CR x) = CR (\p -> let s = findFirstMonotonic ((3 <=) . abs . x)
                               n = x (p + 2 * s + 2)
                           in 2^(2 * p + 2 * s + 2) /. n)

-- | Values of type @CReal p@ are compared for equality at precision @p@. This
-- may cause values which differ by less than 2^-p to compare as equal.
--
-- >>> 0 == (0.1 :: CReal 1)
-- True
instance KnownNat n => Eq (CReal n) where
  -- TODO, should this try smaller values first?
  x == y = let p = crealPrecision x
           in (x - y) `atPrecision` p == 0

-- | Like equality values of type @CReal p@ are compared at precision @p@.
instance KnownNat n => Ord (CReal n) where
  compare x y = let p = crealPrecision x
                in compare ((x - y) `atPrecision` p) 0

--------------------------------------------------------------------------------
-- Some utility functions
--------------------------------------------------------------------------------

--
-- Showing CReals
--

-- | Return a string representing a decimal number within 2^-p of the value
-- represented by the given @CReal p@.
showAtPrecision :: Int -> CReal n -> String
showAtPrecision p (CR x) = let places = decimalDigitsAtPrecision p
                               r = x p % 2^p
                           in rationalToDecimal places r

-- | How many decimal digits are required to represent a number to within 2^-p
decimalDigitsAtPrecision :: Int -> Int
decimalDigitsAtPrecision 0 = 0
decimalDigitsAtPrecision p = log10 (2^p) + 1

-- | @rationalToDecimal p x@ returns a string representing @x@ at @p@ decimal
-- places.
rationalToDecimal :: Int -> Rational -> String
rationalToDecimal places r = s
  where ds = show ((numerator r * 10^places) /. denominator r)
        (is, fs) = splitAt (length ds - places) ds
        is' = case is of
                ""  -> "0"
                "-" -> "-0"
                _   -> is
        suff = case places of
                 0 -> ""
                 _ -> '.' : take places (fs ++ repeat '0')
        s = is' ++ suff

--
-- Integer operations
--

-- | Division rounding to the nearest integer and rounding half integers to the
-- nearest even integer.
(/.) :: Integer -> Integer -> Integer
n /. d = round (n % d)

-- | @log2 x@ returns the base 2 logarithm of @x@ rounded towards zero.
log2 :: Integer -> Int
log2 x = I# (integerLog2# x)

-- | @log10 x@ returns the base 10 logarithm of @x@ rounded towards zero.
log10 :: Integer -> Int
log10 x = I# (integerLogBase# 10 x)

--
-- Searching
--

-- | Given a monotonic function
findFirstMonotonic :: (Int -> Bool) -> Int
findFirstMonotonic p = binarySearch l' u'
  where (l', u') = findBounds 0 1
        findBounds l u = if p u then (l, u)
                                else findBounds u (u*2)
        binarySearch l u = let m = l + ((u - l) `div` 2)
                           in if | l+1 == u  -> l
                                 | p m       -> binarySearch l m
                                 | otherwise -> binarySearch m u

