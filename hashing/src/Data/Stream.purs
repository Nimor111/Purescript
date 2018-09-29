module Data.Stream where

import Prelude
import Data.Array as Array
import Data.String as String
import Data.Monoid (class Monoid, mempty)
import Data.Maybe (Maybe(..))

class Stream stream element where
  uncons :: stream -> Maybe { head :: element, tail :: stream }

instance streamArray :: Stream (Array a) a where
  uncons = Array.uncons

instance streamString :: Stream String String.CodePoint where
  uncons = String.uncons

foldStream :: forall l e m. Stream l e => Monoid m => (e -> m) -> l -> m
foldStream f list =
  case uncons list of
    Nothing -> mempty
    Just cons -> f cons.head <> foldStream f cons.tail
