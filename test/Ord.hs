{-# LANGUAGE ScopedTypeVariables #-}

module Ord
  ( ord
  ) where

import Test.QuickCheck (Arbitrary)
import Test.QuickCheck.Checkers (EqProp)
import Test.QuickCheck.Classes.Extra (ordRel, complement)
import Test.Tasty.Extra (testGroup, TestTree, testTreeFromNamedBatch)
import Test.Tasty.QuickCheck (Gen, oneof, arbitrary, testProperty, property)

ord :: forall a. (Arbitrary a, Show a, EqProp a, Ord a) => a -> TestTree
ord _ = testGroup "Test Ord instance" ts
  where
    gen :: a -> Gen a
    gen x = oneof [pure x, arbitrary]
    ts = [ testTreeFromNamedBatch "<= is a total ordering" (ordRel (<=) gen)
         , testTreeFromNamedBatch ">= is a total ordering" (ordRel (>=) gen)
         , complement "< is the complement of >=" gen (<) (>=)
         , complement "> is the complement of <=" gen (>) (<=)
         , testProperty "max x y >= x and y" (property $ \x y ->
                               let m = max x y :: a
                               in m >= x && m >= y)
         , testProperty "max x y == x or y" (property $ \x y ->
                               let m = max x y :: a
                               in m == x || m == y)
         , testProperty "min x y >= x and y" (property $ \x y ->
                               let m = min x y :: a
                               in m <= x && m <= y)
         , testProperty "min x y == x or y" (property $ \x y ->
                               let m = min x y :: a
                               in m == x || m == y)
         ]
