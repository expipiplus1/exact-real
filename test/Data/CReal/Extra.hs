{-# OPTIONS_GHC -fno-warn-orphans #-}

module Data.CReal.Extra
  ( module Data.CReal
  ) where

import Data.CReal
import Data.CReal.Internal (log2)
import Data.Ratio ((%))
import GHC.TypeLits
import System.Random (Random(..))
import Test.QuickCheck (Arbitrary(..), choose)
import Test.QuickCheck.Checkers (EqProp(..), eq)

instance KnownNat n => EqProp (CReal n) where
  (=-=) = eq

instance KnownNat n => Arbitrary (CReal n) where
  arbitrary = do
    integralPart <- fromInteger <$> arbitrary
    fractionalPart <- choose (-0.5, 0.5)
    pure (integralPart + fractionalPart)

instance KnownNat n => Random (CReal n) where
  randomR (lo, hi) g = let d = hi - lo
                           l = 1 + log2 (abs d `atPrecision` 0)
                           p = l + crealPrecision lo
                           (n, g') = randomR (0, 2^p) g
                           r = fromRational (n % 2^p)
                       in (r * d + lo, g')
  random = randomR (0, 1)

