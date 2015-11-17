{-# OPTIONS_GHC -fno-warn-orphans #-}
module Data.Ratio.Extra
  ( module Data.Ratio
  ) where

import Data.Ratio
import Test.QuickCheck.Checkers (EqProp(..), eq)

instance Eq a => EqProp (Ratio a) where
  (=-=) = eq
