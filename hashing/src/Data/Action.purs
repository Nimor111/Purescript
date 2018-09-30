module Data.Action where

import Prelude

import Data.Monoid
import Data.Semigroup
import Data.Array

class Monoid m <= Action m a where
  act :: m -> a -> a

newtype Multiply = Multiply Int

instance semigroupMultiply :: Semigroup Multiply where
  append (Multiply m) (Multiply n) = Multiply (n * m)

instance monoidMultiply :: Monoid Multiply where
  mempty = Multiply 1

instance showMultiply :: Show Multiply where
  show (Multiply n) = show n

instance repeatAction :: Action Multiply String where
  act (Multiply 1) s = s
  act (Multiply n) s = s <> act (Multiply (n - 1)) s

instance arrayAction :: Action m a => Action m (Array a) where
  act m [] = []
  act m arr = map (act m) arr

newtype Self m = Self m

instance selfShow :: Show m => Show (Self m) where
  show (Self m) = show m

instance selfAction :: Monoid m => Action m (Self m) where
  act m1 (Self m2) = Self (m1 <> m2)
