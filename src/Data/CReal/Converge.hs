{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

-- | The Converge type class.
module Data.CReal.Converge
  ( Converge(..)
  ) where

import Data.Coerce (coerce)
import Data.CReal.Internal (CReal(..), atPrecision, crMemoize)
import Data.Proxy (Proxy)
import GHC.TypeLits (someNatVal, SomeNat(..))

-- $setup
-- >>> :set -XFlexibleContexts
-- >>> import Data.CReal.Internal

-- | If a type is an instance of Converge then it represents a stream of values
-- which are increasingly accurate approximations of a desired value
class Converge a where
  -- | The type of the value the stream converges to.
  type Element a

  -- | 'converge' is a function that returns the value the stream is converging
  -- to.
  --
  -- If the stream is empty then it should return nothing.
  --
  -- >>> let initialGuess = 1 :: Double
  -- >>> let improve x = (x + 121 / x) / 2
  -- >>> converge (iterate improve initialGuess)
  -- Just 11.0
  --
  -- >>> converge [] :: Maybe [Int]
  -- Nothing
  converge :: a -> Maybe (Element a)

-- | Every list of equatable values is an instance of 'Converge'. 'converge'
-- returns the first element which is equal to the succeeding element in the
-- list. If the list ends before the sequence converges the last value is
-- returned.
instance {-# OVERLAPPABLE #-} Eq a => Converge [a] where
  type Element [a] = a
  converge = lastMay . takeWhileDifferent
  {-# INLINE converge #-}

-- | The overlapping instance for @'CReal' n@ has a slightly different
-- behavior. The instance for 'Eq' will cause 'converge' to return a value when
-- the list converges to within 2^-n (due to the 'Eq' instance for @'CReal'
-- n@) despite the precision the value is requested at by the surrounding
-- computation. This instance will return a value approximated to the correct
-- precision.
--
-- Find where log x = π using Newton's method
-- >>> let initialGuess = 1
-- >>> let improve x = x - x * (log x - pi)
-- >>> let Just y = converge (iterate improve initialGuess)
-- >>> showAtPrecision 10 y
-- "23.1406"
-- >>> showAtPrecision 50 y
-- "23.1406926327792686"
instance {-# OVERLAPPING #-} Converge [CReal n] where
  type Element [CReal n] = CReal n
  converge [] = Nothing
  converge xs =
    Just $ crMemoize (\p ->
      case someNatVal (toInteger p) of
           Nothing -> error "Data.CReal.Converge p should be non negative"
           Just (SomeNat (_ :: Proxy p')) ->
             (last . takeWhileDifferent . (coerce :: [CReal n] -> [CReal p']) $ xs)
              `atPrecision` p)
  {-# INLINE converge #-}

takeWhileDifferent :: Eq a => [a] -> [a]
takeWhileDifferent (x1:x2:xs) = if x1 == x2
                                  then [x1]
                                  else x1 : takeWhileDifferent (x2:xs)
takeWhileDifferent xs = xs

lastMay :: [a] -> Maybe a
lastMay [] = Nothing
lastMay xs = Just (last xs)


