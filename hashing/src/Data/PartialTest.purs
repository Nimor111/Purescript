module Data.PartialTest where

import Prelude

import Data.Foldable
import Data.Maybe (Maybe(..))

maxArr :: Partial => Array Int -> Maybe Int
maxArr arr = maximum arr
