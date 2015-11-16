{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

-- | Add instances for Arbitrary and EqProp for Sum and Product
module Data.Monoid.Extra
  ( module Data.Monoid
  ) where

import Data.Monoid (Sum(..), Product(..))
import Test.QuickCheck (Arbitrary)
import Test.QuickCheck.Checkers (EqProp)

deriving instance Arbitrary a => Arbitrary (Sum a)

deriving instance EqProp a => EqProp (Sum a)

deriving instance Arbitrary a => Arbitrary (Product a)

deriving instance EqProp a => EqProp (Product a)
