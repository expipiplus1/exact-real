{-# LANGUAGE ScopedTypeVariables #-}

-- | Add a bunch of checkers for testing properties of different algebraic
-- structures
module Test.QuickCheck.Classes.Extra
  ( module Test.QuickCheck.Classes
  , group
  , abelian
  , ring
  , commutativeRing
  , field
  ) where

import Data.Group (invert, Group, Abelian)
import Data.Monoid ((<>), Sum(..), Product)
import Data.Monoid.Extra ()
import Test.QuickCheck.Extra (Arbitrary)
import Test.QuickCheck.Modifiers (NonZero)
import Test.QuickCheck.Checkers (commutes, EqProp, (=-=))
import Test.QuickCheck.Classes
import Test.Tasty.Extra (testGroup, TestTree, testTreeFromBatch, testTreeFromNamedBatch)
import Test.Tasty.QuickCheck (testProperty, Property)

distributesL :: EqProp a => (a -> a -> a) -> (a -> a -> a) -> a -> a -> a -> Property
distributesL (*:) (+:) a b c = a *: (b +: c) =-= (a *: b) +: (a *: c)

distributesR :: EqProp a => (a -> a -> a) -> (a -> a -> a) -> a -> a -> a -> Property
distributesR (*:) = distributesL (flip (*:))

distributes :: (Arbitrary a, EqProp a, Show a) => String -> (a -> a -> a) -> (a -> a -> a) -> TestTree
distributes s (*:) (+:) = testGroup s ts
  where ts = [testProperty "left distributes" (distributesL (*:) (+:)),
              testProperty "right distributes" (distributesR (*:) (+:))]

group :: forall a. (Arbitrary a, EqProp a, Group a, Show a) => String -> a -> TestTree
group s _ = testGroup s ts
  where
    ts = [ testTreeFromBatch (monoid (undefined :: a))
         , testProperty "left inverse element" (\(x :: a) -> x <> invert x =-= mempty)
         , testProperty "right inverse element" (\(x :: a) -> invert x <> x =-= mempty)
         ]

abelian :: forall a. (Arbitrary a, EqProp a, Abelian a, Show a) => String -> a -> TestTree
abelian s _ = testGroup s ts
  where
    ts = [ group "group" (undefined :: a)
         , testProperty "commutative" (commutes ((<>) :: a -> a -> a))
         ]

ring :: forall a. (Arbitrary a, EqProp a, Num a, Show a) => String -> a -> TestTree
ring s _ = testGroup s ts
  where
    ts = [ abelian "abelian under Sum" (undefined :: Sum a)
         , testTreeFromNamedBatch "monoid under product" (monoid (undefined :: Product a))
         , distributes "* distributes over +" (*) ((+) :: a -> a -> a)
         ]

commutativeRing :: forall a. (Arbitrary a, EqProp a, Num a, Show a) => String -> a -> TestTree
commutativeRing s _ = testGroup s ts
  where ts = [ring "ring" (undefined :: a),
             testProperty "* commutes" (commutes ((*) :: a -> a -> a))]

field :: forall a. (Arbitrary a, EqProp a, Fractional a, Show a, Ord a) => String -> a -> TestTree
field s _ = testGroup s ts
  where ts = [abelian "Abelian under Sum" (undefined :: Sum a),
              abelian "Abelian under Product NonZero" (undefined :: Product (NonZero a)),
              distributes "* distributes over +" (*) ((+) :: a -> a -> a)]

