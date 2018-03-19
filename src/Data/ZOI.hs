{-# LANGUAGE LambdaCase #-}

module Data.ZOI where

import Test.QuickCheck.Arbitrary
import Test.QuickCheck.Gen (elements)

-- $setup
-- >>> :set -XScopedTypeVariables
-- >>> import Test.QuickCheck

data ZOI 
  = MinusInfinity
  | MinusOne
  | Zero
  | One
  | Infinity
  deriving(Read, Show, Eq, Ord)

instance Arbitrary ZOI where
  arbitrary = elements [MinusInfinity, MinusOne, Zero, One, Infinity]

data Range = Range
  { lower :: !ZOI
  , upper :: !ZOI
  }
  deriving(Read, Show, Eq)

-- |
-- prop> \r -> negateRange (rationalToRange r) === rationalToRange (negate r)
integerToRange :: Integer -> Range
integerToRange n | n < (-1)  = Range MinusInfinity MinusOne
                 | n == (-1) = Range MinusOne MinusOne
                 | n == 0    = Range Zero Zero
                 | n == 1    = Range One One
                 | otherwise = Range One Infinity

-- |
-- prop> \r -> negateRange (rationalToRange r) === rationalToRange (negate r)
rationalToRange :: Rational -> Range
rationalToRange r | r < (-1)  = Range MinusInfinity MinusOne
                  | r == (-1) = Range MinusOne MinusOne
                  | r < 0     = Range MinusOne Zero
                  | r == 0    = Range Zero Zero
                  | r < 1     = Range Zero One
                  | r == 1    = Range One One
                  | otherwise = Range One Infinity

unitInterval :: Range
unitInterval = Range Zero One

-- |
-- >>> negateRange biunitInterval
-- Range {lower = MinusOne, upper = One}
biunitInterval :: Range
biunitInterval = Range MinusOne One

add :: Range -> Range -> Range
add (Range l1 u1) (Range l2 u2) =
  Range (addLowerBound l1 l2) (addUpperBound u1 u2)

addUpperBound :: ZOI -> ZOI -> ZOI
addUpperBound Infinity      _             = Infinity
addUpperBound _             Infinity      = Infinity
addUpperBound MinusInfinity _             = MinusInfinity
addUpperBound _             MinusInfinity = MinusInfinity
addUpperBound Zero          n             = n
addUpperBound n             Zero          = n
addUpperBound One           One           = Infinity
addUpperBound MinusOne      One           = Zero
addUpperBound One           MinusOne      = Zero
addUpperBound MinusOne      MinusOne      = MinusOne

addLowerBound :: ZOI -> ZOI -> ZOI
addLowerBound a b = negateZOI (addUpperBound (negateZOI a) (negateZOI b))

-- | 
-- prop> \(l :: ZOI) (u :: ZOI) -> addLowerBound l u === addLowerBound' l u
addLowerBound' MinusInfinity _             = MinusInfinity
addLowerBound' _             MinusInfinity = MinusInfinity
addLowerBound' Infinity      _             = Infinity
addLowerBound' _             Infinity      = Infinity
addLowerBound' Zero          n             = n
addLowerBound' n             Zero          = n
addLowerBound' MinusOne      MinusOne      = MinusInfinity
addLowerBound' MinusOne      One           = Zero
addLowerBound' One           MinusOne      = Zero
addLowerBound' One           One           = One

negateRange :: Range -> Range
negateRange (Range l u) = Range (negateZOI u) (negateZOI l)

negateZOI :: ZOI -> ZOI
negateZOI = \case
  MinusInfinity -> Infinity
  MinusOne -> One
  Zero -> Zero
  One -> MinusOne
  Infinity -> MinusInfinity
