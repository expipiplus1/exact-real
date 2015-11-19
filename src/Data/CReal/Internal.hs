{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE PostfixOperators #-}

-----------------------------------------------------------------------------
-- | This module exports a bunch of utilities for working inside the CReal
-- datatype. One should be careful to maintain the CReal invariant when using
-- these functions
----------------------------------------------------------------------------
module Data.CReal.Internal
  ( CReal(..)
  , atPrecision
  , crealPrecision

  , (.*)
  , (*.)
  , (.*.)
  , mulBounded
  , mulBoundedL
  , recipBounded

  , expBounded
  , logBounded

  , atanBounded
  , sinBounded
  , cosBounded

  , shiftL
  , shiftR

  , powerSeries
  , alternateSign

  , (/.)
  , log2
  , log10
  , isqrt

  , showAtPrecision
  , decimalDigitsAtPrecision
  , rationalToDecimal
  ) where

import Data.List (scanl')
import Data.Ratio (numerator,denominator,(%))
import GHC.Base (Int(..))
import GHC.Integer.Logarithms (integerLog2#, integerLogBase#)
import GHC.TypeLits
import Numeric (readSigned, readFloat)

{-# ANN module "HLint: ignore Reduce duplication" #-}

-- $setup
-- >>> :set -XDataKinds

infixl 7 /.

default ()

-- | The type CReal represents a fast binary Cauchy sequence. This is
-- a Cauchy sequence with the invariant that the pth element will be within
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

instance KnownNat n => Read (CReal n) where
  readsPrec _ = readSigned readFloat

-- | @signum (x :: CReal p)@ returns the sign of @x@ at precision @p@. It's
-- important to remember that this /may not/ represent the actual sign of @x@ if
-- the distance between @x@ and zero is less than 2^-@p@.
--
-- This is a little bit of a fudge, but it's probably better than failing to
-- terminate when trying to find the sign of zero. The class still respects the
-- abs-signum law though.
--
-- >>> signum (0.1 :: CReal 2)
-- 0.0
--
-- >>> signum (0.1 :: CReal 3)
-- 1.0
instance Num (CReal n) where
  {-# INLINE fromInteger #-}
  fromInteger i = CR (\p -> i * 2 ^ p)

  {-# INLINE negate #-}
  negate (CR x) = CR (negate . x)

  {-# INLINE abs #-}
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
  fromRational n = fromInteger (numerator n) *. recipBounded (fromInteger (denominator n))

  {-# INLINE recip #-}
  -- TODO: Make recip 0 throw an error (if, for example, it would take more
  -- than 4GB of memory to represent the result)
  recip (CR x) = CR (\p -> let s = findFirstMonotonic ((3 <=) . abs . x)
                               n = x (p + 2 * s + 2)
                           in 2^(2 * p + 2 * s + 2) /. n)

instance Floating (CReal n) where
  -- TODO: Could we use something faster such as Ramanujan's formula
  pi = 4 * piBy4

  exp x = let CR o = x / ln2
              l = o 0
              y = x - fromInteger l * ln2
          in if l == 0
               then expBounded x
               else expBounded y `shiftL` fromInteger l

  -- | Range reduction on the principle that ln (a * b) = ln a + ln b
  log x = let CR o = x
              l = log2 (o 2) - 2
          in if   -- x <= 0.75
                | l < 0  -> - log (recip x)
                  -- 0.75 <= x <= 2
                | l == 0 -> logBounded x
                  -- x >= 2
                | l > 0  -> let a = x `shiftR` l
                            in logBounded a + fromIntegral l *. ln2

  sqrt (CR x) = CR (\p -> let n = x (2 * p)
                          in isqrt n)

  -- | This will diverge when the base is not positive
  x ** y = exp (log x * y)

  logBase x y = log y / log x

  sin x = cos (x - pi / 2)

  cos x = let CR o = x / piBy4
              s = o 1 /. 2
              octant = fromInteger $ s `mod` 8
              offset = x - (fromIntegral s * piBy4)
              fs = [          cosBounded
                   , negate . sinBounded . subtract piBy4
                   , negate . sinBounded
                   , negate . cosBounded . (piBy4-)
                   , negate . cosBounded
                   ,          sinBounded . subtract piBy4
                   ,          sinBounded
                   ,          cosBounded . (piBy4-)]
          in (fs !! octant) offset

  tan x = sin x .* recip (cos x)

  asin x = 2 * atan (x .*. recipBounded (1 + sqrt (1 - x.*.x)))

  acos x = pi/2 - asin x

  atan x = let -- q is 4 times x to within 1/4
               q = x `atPrecision` 2
           in if   -- x <= -1
                 | q <  -4 -> atanBounded (negate (recipBounded x)) - pi / 2
                   -- -1.25 <= x <= -0.75
                 | q == -4 -> -pi / 4 - atanBounded ((x + 1) .*. recipBounded (x - 1))
                   -- 0.75 <= x <= 1.25
                 | q ==  4 -> pi / 4 + atanBounded ((x - 1) .*. recipBounded (x + 1))
                   -- x >= 1
                 | q >   4 -> pi / 2 - atanBounded (recipBounded x)
                   -- -0.75 <= x <= 0.75
                 | otherwise -> atanBounded x

  -- TODO: benchmark replacing these with their series expansion
  sinh x = (exp x - exp (-x)) / 2
  cosh x = (exp x + exp (-x)) / 2
  tanh x = let e2x = exp (2 * x)
           in (e2x - 1) / (e2x + 1)

  asinh x = log (x + sqrt (x * x + 1))
  acosh x = log (x + sqrt (x + 1) * sqrt (x - 1))
  atanh x = (log (1 + x) - log (1 - x)) / 2

-- | 'toRational' returns the CReal n evaluated at a precision of 2^-n
instance KnownNat n => Real (CReal n) where
  toRational x = let p = crealPrecision x
                 in x `atPrecision` p % 2^p

instance KnownNat n => RealFrac (CReal n) where
  properFraction x = let p = crealPrecision x
                         n = (x `atPrecision` p) `quot` 2^p
                         f =  x - fromInteger n
                     in (fromInteger n, f)

-- | Several of the functions in this class ('floatDigits', 'floatRange',
-- 'exponent', 'significand') only make sense for floats represented by a
-- mantissa and exponent. These are bound to error.
--
-- @atan2 y x `atPrecision` p@ performs the comparison to determine the
-- quadrant at precision p. This can cause atan2 to be slightly slower than atan
instance KnownNat n => RealFloat (CReal n) where
  floatRadix _ = 2
  floatDigits _ = error "Data.CReal.Internal floatDigits"
  floatRange _ = error "Data.CReal.Internal floatRange"
  decodeFloat x = let p = crealPrecision x
                  in (x `atPrecision` p, -p)
  encodeFloat m n = fromRational (m % 2^(-n))
  exponent = error "Data.CReal.Internal exponent"
  significand = error "Data.CReal.Internal significand"
  scaleFloat = flip shiftL
  isNaN _ = False
  isInfinite _ = False
  isDenormalized _ = False
  isNegativeZero _ = False
  isIEEE _ = False
  atan2 y x = CR (\p ->
    let y' = y `atPrecision` p
        x' = x `atPrecision` p
        θ = if | x' > 0            ->  atan (y/x)
               | x' == 0 && y' > 0 ->  pi/2
               | x' <  0 && y' > 0 ->  pi + atan (y/x)
               | x' <= 0 && y' < 0 -> -atan2 (-y) x
               | y' == 0 && x' < 0 ->  pi    -- must be after the previous test on zero y
               | x'==0 && y'==0    ->  0     -- must be after the other double zero tests
               | otherwise         ->  error "Data.CReal.Internal atan2"
    in θ `atPrecision` p)

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
  max (CR x) (CR y) = CR (\p -> max (x p) (y p))
  min (CR x) (CR y) = CR (\p -> min (x p) (y p))

--------------------------------------------------------------------------------
-- Some utility functions
--------------------------------------------------------------------------------

--
-- Constants
--

piBy4 :: CReal n
piBy4 = 4 * atanBounded (1/5) - atanBounded (1 / 239) -- Machin Formula

ln2 :: CReal n
ln2 = logBounded 2

--
-- Bounded multiplication
--

infixl 7 `mulBounded`, `mulBoundedL`, .*, *., .*.

(.*), (*.), (.*.) :: CReal n -> CReal n -> CReal n
(.*) = mulBoundedL
(*.) = flip mulBoundedL
(.*.) = mulBounded

-- | The first argument to @mulBoundedL@ must be in the range [-1..1]
mulBoundedL :: CReal n -> CReal n -> CReal n
mulBoundedL (CR x1) (CR x2) = CR (\p -> let s1 = 4
                                            s2 = log2 (abs (x2 0) + 2) + 3
                                            n1 = x1 (p + s2)
                                            n2 = x2 (p + s1)
                                        in (n1 * n2) /. 2^(p + s1 + s2))

-- | Both arguments to @mulBounded@ must be in the range [-1..1]
mulBounded :: CReal n -> CReal n -> CReal n
mulBounded (CR x1) (CR x2) = CR (\p -> let s1 = 4
                                           s2 = 4
                                           n1 = x1 (p + s2)
                                           n2 = x2 (p + s1)
                                       in (n1 * n2) /. 2^(p + s1 + s2))

-- | The absolute value of the argument to @recipBounded@ must be greater than
-- or equal to 1
recipBounded :: CReal n -> CReal n
recipBounded (CR x) = CR (\p -> let s = 2
                                    n = x (p + 2 * s + 2)
                                in 2^(2 * p + 2 * s + 2) /. n)

--
-- Bounded exponential functions
--

-- | The input to expBounded must be in the range (-1..1)
expBounded :: CReal n -> CReal n
expBounded x = let q = [1 % (n!) | n <- [0..]]
               in powerSeries q (max 5) x

-- | The input must be in [2/3..2]
logBounded :: CReal n -> CReal n
logBounded x = let q = [1 % n | n <- [1..]]
                   y = (x - 1) .* recip x
               in y .* powerSeries q id y

--
-- Bounded trigonometric functions
--

-- | The input to sinBounded must be in (-1..1)
sinBounded :: CReal n -> CReal n
sinBounded x = let q = alternateSign (scanl' (*) 1 [ 1 % (n*(n+1)) | n <- [2,4..]])
               in x * powerSeries q (max 1) (x .*. x)

-- | The input to cosBounded must be in (-1..1)
cosBounded :: CReal n -> CReal n
cosBounded x = let q = alternateSign (scanl' (*) 1 [1 % (n*(n+1)) | n <- [1,3..]])
               in powerSeries q (max 1) (x .*. x)

-- | The input to atanBounded must be in [-1..1]
atanBounded :: CReal n -> CReal n
atanBounded x = let q = scanl' (*) 1 [n % (n + 1) | n <- [2,4..]]
                    d = 1 + x .*. x
                    rd = recipBounded d
                in CR (\p -> ((x .*. rd) .* powerSeries q (+1) (x .*. x .*. rd)) `atPrecision` p)

--
-- Multiplication with powers of two
--

-- | @x \`shiftR\` n@ is equal to @x@ divided by 2^@n@
--
-- @n@ can be negative or zero
--
-- This can be faster than doing the division
shiftR :: CReal n -> Int -> CReal n
shiftR (CR x) n = CR (\p -> let p' = p - n
                            in if p' >= 0
                                 then x p'
                                 else x 0 /. 2^(-p'))

-- | @x \`shiftL\` n@ is equal to @x@ multiplied by 2^@n@
--
-- @n@ can be negative or zero
--
-- This can be faster than doing the multiplication
shiftL :: CReal n -> Int -> CReal n
shiftL x = shiftR x . negate


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
rationalToDecimal places r = p ++ is ++ if places > 0 then "." ++ fs else ""
  where r' = abs r
        p = case signum r of
              -1 -> "-"
              _  -> ""
        ds = show ((numerator r' * 10^places) /. denominator r')
        l = length ds
        (is, fs) = if | l <= places -> ("0", replicate (places - l) '0' ++ ds)
                      | otherwise -> splitAt (length ds - places) ds


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

-- | @isqrt x@ returns the square root of @x@ rounded towards zero.
isqrt :: Integer -> Integer
isqrt x | x < 0     = error "Sqrt applied to negative Integer"
        | x == 0    = 0
        | otherwise = until satisfied improve initialGuess
  where improve r    = (r + (x `div` r)) `div` 2
        satisfied r  = sq r <= x && sq (r + 1) > x
        initialGuess = 2 ^ (log2 x `div` 2)
        sq r         = r * r

-- | Factorial function
(!) :: Integer -> Integer
(!) x = product [2..x]

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


--
-- Power series
--

-- | Apply 'negate' to every other element, starting with the second
--
-- >>> alternateSign [1..5]
-- [1,-2,3,-4,5]
alternateSign :: Num a => [a] -> [a]
alternateSign = zipWith ($) (cycle [id, negate])

-- | @powerSeries q f x `atPrecision` p@ will evaluate the power series with
-- coefficients @q@ at precision @f p@ at @x@
--
-- @f@ should be a function such that the CReal invariant is maintained
--
-- See any of the trig functions for an example
powerSeries :: [Rational] -> (Int -> Int) -> CReal n -> CReal n
powerSeries q termsAtPrecision (CR x) =
  CR (\p -> let t = termsAtPrecision p
                d = log2 (toInteger t) + 2
                p' = p + d
                p'' = p' + d
                m = x p''
                xs = (%1) <$> iterate (\e -> m * e /. 2^p'') (2^p')
                r = sum . take (t + 1) . fmap (round . (* (2^d))) $ zipWith (*) q xs
            in r /. 4^d)

