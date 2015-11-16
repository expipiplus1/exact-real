{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

-- | Add some more newtypes for restricting arbitrary instances
module Test.QuickCheck.Extra
  ( module Test.QuickCheck
  , UnitInterval(..)
  , BiunitInterval(..)
  , Tiny(..)
  ) where

import Test.QuickCheck (Arbitrary(..), choose, suchThat)
import Test.QuickCheck.Checkers (EqProp)
import Test.QuickCheck.Modifiers (NonZero(..))
import System.Random (Random)

deriving instance Num a => Num (NonZero a)
deriving instance Fractional a => Fractional (NonZero a)
deriving instance EqProp a => EqProp (NonZero a)

newtype UnitInterval a = UnitInterval a
  deriving(Eq, Ord, Show, Read, Num, Integral, Fractional, Floating, Real, Enum, Functor, Random, EqProp)

instance (Arbitrary a, Num a, Random a) => Arbitrary (UnitInterval a) where
  arbitrary = choose (0, 1)
  shrink (UnitInterval a) = UnitInterval <$> shrink a

newtype BiunitInterval a = BiunitInterval a
  deriving(Eq, Ord, Show, Read, Num, Integral, Fractional, Floating, Real, Enum, Functor, Random, EqProp)

instance (Arbitrary a, Num a, Random a) => Arbitrary (BiunitInterval a) where
  arbitrary = choose (-1, 1)
  shrink (BiunitInterval a) = BiunitInterval <$> shrink a

newtype Tiny a = Tiny a
  deriving(Eq, Ord, Show, Read, Num, Integral, Real, Enum, Functor)

-- | Chosen rather arbitrarily just so the tests involving exponentiation don't take too long
tinyBound :: Num a => a
tinyBound = 1000000000

instance (Num a, Ord a, Arbitrary a) => Arbitrary (Tiny a) where
  arbitrary = Tiny <$> arbitrary `suchThat` ((< tinyBound) . abs)



