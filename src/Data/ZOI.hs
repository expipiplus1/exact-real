{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Data.ZOI where

-- import qualified Prelude 
import Prelude hiding (recip, div, exp, sin, cos, tan, sinh, cosh, tanh)
import Test.QuickCheck.Arbitrary
-- import Data.Maybe(fromMaybe)
import Test.QuickCheck.Gen (elements, suchThat)

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

instance Arbitrary Range where
  arbitrary = do
    l <- arbitrary
    u <- arbitrary `suchThat` (>= l)
    pure $ Range l u

-- |
-- prop> \r -> negateRange (integerToRange r) === integerToRange (negate r)
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

fromReal :: Real a => a -> Range 
fromReal = rationalToRange . toRational

unitInterval :: Range
unitInterval = Range Zero One

-- |
-- >>> negateRange biunitInterval
-- Range {lower = MinusOne, upper = One}
biunitInterval :: Range
biunitInterval = Range MinusOne One

-- | 
--
-- prop> \(a :: Double) b -> fromReal (a + b) `isSubset` add (fromReal a) (fromReal b)
add :: Range -> Range -> Range
add (Range l1 u1) (Range l2 u2) =
  Range (addLowerBound l1 l2) (addUpperBound u1 u2)

-- | 
--
-- prop> \(a :: Double) b -> fromReal (a - b) `isSubset` sub (fromReal a) (fromReal b)
sub :: Range -> Range -> Range
sub r1 r2 = add r1 (negateRange r2)

-- | 
--
-- prop> \(a :: Double) b -> fromReal (a * b) `isSubset` mul (fromReal a) (fromReal b)
-- prop> \(a :: Double) b (c :: Double) d -> (fromReal (a * b) `union` fromReal (c * d)) `isSubset` mul (fromReal a `union` fromReal c) (fromReal b `union` fromReal d)
mul :: Range -> Range -> Range
mul r1' r2' = case (r1', r2') of
  (Range Zero Zero, _) -> Range Zero Zero
  (_, Range Zero Zero) -> Range Zero Zero
  (Range One One, r) -> r
  (r, Range One One) -> r
  (Range MinusOne MinusOne, r) -> negateRange r
  (r, Range MinusOne MinusOne) -> negateRange r
  (Range Zero One, r) -> r `union` Range Zero Zero
  (r, Range Zero One) -> r `union` Range Zero Zero
  (Range MinusOne Zero, r) -> negateRange (r `union` Range Zero Zero)
  (r, Range MinusOne Zero) -> negateRange (r `union` Range Zero Zero)
  (Range MinusOne One, r) -> r `union` negateRange r
  (r, Range MinusOne One) -> r `union` negateRange r
  (r1, r2)
    | nonNegative r1
    , nonNegative r2
    -> r1 `union` r2
  (r1, r2)
    | nonPositive r1
    , nonPositive r2
    -> negateRange r1 `union` negateRange r2
  (r1, r2)
    | nonNegative r1
    , nonPositive r2
    -> negateRange r1 `union` r2
  (r1, r2)
    | nonPositive r1
    , nonNegative r2
    -> r1 `union` negateRange r2 
  _ -> Range MinusInfinity Infinity

-- | 
--
-- prop> \(a :: Double) b -> b /= 0 ==> fromReal (a / b) `isSubset` div (fromReal a) (fromReal b)
-- prop> \(a :: Double) (b :: Double) (c :: Double) (d :: Double) -> b /= 0 && d /= 0 ==> (fromReal (a / b) `union` fromReal (c / d)) `isSubset` div (fromReal a `union` fromReal c) (fromReal b `union` fromReal d)
div :: Range -> Range -> Range
div a b = a `mul` recip b

-- | 
--
-- prop> \(a :: Double) -> fromReal (Prelude.recip a) `isSubset` recip (fromReal a)
-- prop> \(a :: Double) (b :: Double) -> (fromReal (Prelude.recip a) `union` fromReal (Prelude.recip b)) `isSubset` recip (fromReal a `union` fromReal b)
recip :: Range -> Range
recip = \case
  Range MinusInfinity MinusInfinity -> Range Zero Zero
  Range MinusInfinity MinusOne      -> Range MinusOne Zero
  Range MinusInfinity Zero          -> Range MinusInfinity Zero
  Range MinusInfinity One           -> Range MinusOne Infinity
  Range MinusInfinity Infinity      -> Range MinusInfinity Infinity
  Range MinusOne      MinusOne      -> Range MinusOne MinusOne
  Range MinusOne      Zero          -> Range MinusInfinity MinusOne
  Range MinusOne      One           -> Range MinusInfinity Infinity
  Range MinusOne      Infinity      -> Range MinusInfinity One
  Range Zero          Zero          -> Range MinusInfinity Infinity
  Range Zero          One           -> Range One Infinity
  Range Zero          Infinity      -> Range Zero Infinity
  Range One           One           -> Range One One
  Range One           Infinity      -> Range Zero One
  Range Infinity      Infinity      -> Range Zero Zero

nonNegative :: Range -> Bool
nonNegative (Range l _) = l >= Zero

nonPositive :: Range -> Bool
nonPositive (Range _ u) = u <= Zero

--   -> fromMaybe (Range MinusInfinity Infinity) $ do
--     r1Cmp0 <- compare0 r1
--     r2Cmp0 <- compare0 r2
--     pure $ if r1Cmp0 == r2Cmp0
--       then Range One Infinity
--       else Range MinusInfinity MinusOne

  -- let sign :: Maybe Sign
  --     sign = do
  --       r1Cmp0 <- compare0 r1
  --       r2Cmp0 <- compare0 r2
  --       case (r1Cmp0, r2Cmp0) of
  --         (LT, LT) -> Positive
  --         (LT, EQ) -> Zero
  --         (LT, GT) -> Negative
  --         (EQ, _ ) -> Zero
  --         (GT, LT) -> Negative
  --         (GT, EQ) -> Zero
  --         (GT, GT) -> Positive
  --     lowerBound = _
  --     upperBound = _
  -- in case sign of
  --      Nothing -> _
  --      Just Negative -> negateRange (Range lowerBound upperBound)
  --      Just Zero -> Range Zero Zero
  --      Just Positive -> Range lowerBound upperBound

-- compare0 :: Range -> Maybe Ordering

-- data Sign = Negative | Zero | Positive

union :: Range -> Range -> Range
union (Range l1 u1) (Range l2 u2) = Range (min l1 l2) (max u1 u2)

-- | 
--
-- prop> \(d :: Double) -> fromReal (Prelude.exp d) `isSubset` exp (fromReal d)
exp :: Range -> Range
exp = \case
  Range _        MinusInfinity -> Range Zero Zero
  Range _        Zero          -> Range Zero One
  Range Zero     _             -> Range One Infinity
  Range Infinity _             -> Range Infinity Infinity
  Range _        _             -> Range Zero Infinity

-- | 
--
-- prop> \(d :: Double) -> fromReal (Prelude.sin d) `isSubset` sin (fromReal d)
sin :: Range -> Range
sin = \case
  Range MinusOne MinusOne -> Range MinusOne Zero
  Range MinusOne Zero     -> Range MinusOne Zero
  Range Zero     Zero     -> Range Zero Zero
  Range Zero     One      -> Range Zero One
  Range One      One      -> Range Zero One
  _                       -> Range MinusOne One

-- | 
--
-- prop> \(d :: Double) -> fromReal (Prelude.cos d) `isSubset` cos (fromReal d)
cos :: Range -> Range
cos = \case
  Range MinusOne MinusOne -> Range Zero One
  Range MinusOne Zero     -> Range Zero One
  Range Zero     Zero     -> Range One One
  Range Zero     One      -> Range Zero One
  Range One      One      -> Range Zero One
  _                       -> Range MinusOne One

-- | 
--
-- prop> \(d :: Double) -> fromReal (Prelude.tan d) `isSubset` tan (fromReal d)
tan :: Range -> Range
tan = \case
  Range Zero Zero -> Range Zero Zero
  _ -> Range MinusInfinity Infinity

-- | 
--
-- prop> \(d :: Double) -> fromReal (Prelude.sinh d) `isSubset` sinh (fromReal d)
sinh :: Range -> Range
sinh = \case
  Range Zero Zero -> Range Zero Zero
  _ -> Range MinusInfinity Infinity

-- | 
--
-- prop> \(d :: Double) -> fromReal (Prelude.cosh d) `isSubset` cosh (fromReal d)
cosh :: Range -> Range
cosh = \case
  Range Zero Zero -> Range One One
  _ -> Range One Infinity

-- | 
--
-- prop> \(d :: Double) -> fromReal (Prelude.tanh d) `isSubset` tanh (fromReal d)
tanh :: Range -> Range
tanh = \case
  Range Zero     Zero          -> Range Zero Zero
  Range _        MinusInfinity -> Range MinusOne MinusOne
  Range _        MinusOne      -> Range MinusOne Zero
  Range _        Zero          -> Range MinusOne Zero
  Range Zero     _             -> Range Zero One
  Range One      _             -> Range Zero One
  Range Infinity _             -> Range One One
  _                            -> Range MinusOne One

-- | Compare against zero
--
-- >>> compare0 (Range Zero Zero)
-- Just EQ
--
-- >>> compare0 (Range MinusInfinity Infinity)
-- Nothing
--
-- >>> compare0 (Range MinusInfinity MinusOne)
-- Just LT
--
-- >>> compare0 (Range Zero Infinity)
-- Nothing
compare0 :: Range -> Maybe Ordering
compare0 = \case
  Range _        MinusInfinity -> Just LT
  Range _        MinusOne      -> Just LT
  Range Zero     Zero          -> Just EQ
  Range One      _             -> Just GT
  Range Infinity _             -> Just GT
  _                            -> Nothing

-- |
--
-- >>> isSubset unitInterval biunitInterval
-- True
--
-- >>> isSubset (Range Zero One) (Range MinusOne One)
-- True
--
-- >>> isSubset (Range Zero Infinity) biunitInterval
-- False
--
-- prop> \a -> isSubset a (Range MinusInfinity Infinity)
isSubset :: Range -> Range -> Bool
isSubset (Range l1 u1) (Range l2 u2) = l2 <= l1 && u2 >= u1

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
