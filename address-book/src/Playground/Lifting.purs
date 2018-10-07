module Playground.Lifting where

import Prelude

import Control.Applicative
import Control.Apply

import Data.List
import Data.Semiring
import Data.Maybe
import Data.Ring
import Data.EuclideanRing

combineList :: forall a f. Applicative f => List (f a) -> f (List a)
combineList Nil = pure Nil
combineList (Cons x xs) = Cons <$> x <*> combineList xs

optionalMult :: forall a. Semiring a => Maybe a -> Maybe a -> Maybe a
optionalMult = lift2 (*)

optionalAdd :: forall a. Semiring a => Maybe a -> Maybe a -> Maybe a
optionalAdd = lift2 (+)

optionalDiv :: forall a. EuclideanRing a => Maybe a -> Maybe a -> Maybe a
optionalDiv = lift2 (/)

optionalSub :: forall a. Ring a => Maybe a -> Maybe a -> Maybe a
optionalSub = lift2 (-)

combineMaybe :: forall a f. Applicative f => Maybe (f a) -> f (Maybe a)
combineMaybe Nothing = pure Nothing
combineMaybe (Just f) = Just <$> f
