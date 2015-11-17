{-# LANGUAGE ScopedTypeVariables #-}

module RealFrac
  ( realFrac
  ) where

import Data.Ratio.Extra ()
import Test.QuickCheck.Checkers (EqProp, (=-=))
import Test.Tasty (testGroup, TestTree)
import Test.Tasty.QuickCheck (testProperty, Arbitrary)

realFrac :: forall a. (Arbitrary a, EqProp a, Show a, RealFrac a) =>
            a -> TestTree
realFrac x = testGroup "Test RealFrac instance" ts
  where ts = [ properFractionLaws "properFraction laws" x ]

properFractionLaws :: forall a. (Arbitrary a, EqProp a, Show a, RealFrac a) =>
                      String -> a -> TestTree
properFractionLaws s _ = testGroup s ts
  where ts = [ testProperty "x = n + f"
                            (\x -> let (n, f) = properFraction (x :: a)
                                   in x =-= fromInteger n + f)
             , testProperty "n has same sign"
                            (\x -> let (n, _) = properFraction (x :: a)
                                   in n == 0 || sign x == sign (n::Int))
             , testProperty "abs f < 1"
                            (\x -> let (_::Int, f) = properFraction (x :: a)
                                   in abs f < 1)
             ]

data Sign = Positive
          | Negative
  deriving (Eq, Show)

-- | Note that this returns Positive on zero rather than 0 like signum
sign :: (Ord a, Num a) => a -> Sign
sign x = if x < 0 then Negative
                  else Positive
