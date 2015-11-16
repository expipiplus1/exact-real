{-# OPTIONS_GHC -fno-warn-orphans #-}

module Data.CReal.Extra
  ( module Data.CReal
  ) where

import Test.QuickCheck.Checkers (EqProp(..), eq)
import Data.CReal
import GHC.TypeLits

instance KnownNat n => EqProp (CReal n) where
  (=-=) = eq

