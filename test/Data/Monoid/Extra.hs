{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving         #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

-- | Add instances for Arbitrary and EqProp for Sum and Product
module Data.Monoid.Extra
  ( module Data.Monoid
  ) where

import           Data.Monoid              (Product (..), Sum (..))
import           Test.QuickCheck.Checkers (EqProp)

deriving instance EqProp a => EqProp (Sum a)

deriving instance EqProp a => EqProp (Product a)
