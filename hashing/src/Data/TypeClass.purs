module Data.TypeClass where

import Prelude
import Data.Foldable

newtype Complex = Complex
  { real :: Number
  , imaginary :: Number
  }

instance showComplex :: Show Complex where
  show (Complex { real, imaginary }) = show real <> " + " <> show imaginary <> "i"

instance eqComplex :: Eq Complex where
  eq (Complex {real: r1, imaginary: i1}) (Complex {real: r2, imaginary: i2}) = r1 == r2 && i1 == i2

showCompare :: forall a. Ord a => Show a => a -> a -> String
showCompare a1 a2 | a1 < a2 =
  show a1 <> " is less than " <> show a2
showCompare a1 a2 | a1 > a2 =
  show a1 <> " is greater than " <> show a2
showCompare a1 a2 =
  show a1 <> " is equal to " <> show a2

data NonEmpty a = NonEmpty a (Array a)

instance eqNonEmpty :: (Eq a, Eq (Array a)) => Eq (NonEmpty a) where
  eq (NonEmpty a1 arr1) (NonEmpty a2 arr2) = a1 == a2 && arr1 == arr2

instance appendNonEmpty :: (Semigroup a, Semigroup (Array a)) => Semigroup (NonEmpty a) where
  append (NonEmpty a1 arr1) (NonEmpty a2 arr2) = NonEmpty (a1 <> a2) (arr1 <> arr2)

instance showNonEmpty :: (Show a, Show (Array a)) => Show (NonEmpty a) where
  show (NonEmpty a arr) = show a <> " " <> show arr

instance mapNonEmpty :: Functor NonEmpty where
  map f (NonEmpty v arr) = NonEmpty (f v) (map f arr)

data Extended a = Finite a | Infinite

instance eqExtended :: (Eq a) => Eq (Extended a) where
  eq (Finite a) (Finite b) = a == b
  eq Infinite Infinite = true
  eq Infinite (Finite _) = false
  eq (Finite _) Infinite = false

instance extendedOrd :: (Eq a, Ord a) => Ord (Extended a) where
  compare Infinite (Finite _) = GT
  compare (Finite _) Infinite = LT
  compare Infinite Infinite = EQ
  compare (Finite a) (Finite b) = compare a b

instance foldableNonEmpty :: Foldable NonEmpty where
  foldl f a (NonEmpty b arr) = foldl f (f a b) arr
  foldr f a (NonEmpty b arr) = foldr f (f b a) arr
  foldMap f (NonEmpty b arr) = (f b) <> foldMap f arr

data OneMore f a = OneMore a (f a)

instance foldableOneMore :: Foldable f => Foldable (OneMore f) where
  foldl f a (OneMore b arr) = foldl f (f a b) arr
  foldr f a (OneMore b arr) = foldr f (f b a) arr
  foldMap f (OneMore b arr) = (f b) <> foldMap f arr
