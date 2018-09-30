module Data.Hashable where

import Prelude

import Data.Function (on)
import Data.Char (toCharCode)
import Data.Foldable (foldr)
import Data.String.CodeUnits (toCharArray)
import Data.Maybe (Maybe(..), fromJust)
import Data.Tuple (Tuple(..))
import Data.Either (Either(..))
import Data.Array (nubBy)

import Partial.Unsafe (unsafePartial)

newtype HashCode = HashCode Int

instance eqHashCode :: Eq HashCode where
  eq (HashCode m) (HashCode n) = m == n

instance showHashCode :: Show HashCode where
  show (HashCode m) = show m

hashCode :: Int -> HashCode
hashCode h = HashCode (h `mod` 65535)

class Eq a <= Hashable a where
  hash :: a -> HashCode

-- redistributes the hashcodes over 0 to 65535
combineHashes :: HashCode -> HashCode -> HashCode
combineHashes (HashCode h1) (HashCode h2) = hashCode (73 * h1 + 53 * h2)

hashEqual :: forall a. Hashable a => a -> a -> Boolean
hashEqual = eq `on` hash

instance hashInt :: Hashable Int where
  hash = hashCode

instance hashBoolean :: Hashable Boolean where
  hash false = hashCode 0
  hash true = hashCode 1

instance hashChar :: Hashable Char where
  hash = hashCode <<< toCharCode

instance hashArray :: Hashable a => Hashable (Array a) where
  hash = foldr combineHashes (hashCode 0) <<< map hash

instance hashString :: Hashable String where
  hash = hash <<< toCharArray

instance hashMaybe :: Hashable a => Hashable (Maybe a) where
  hash Nothing = hashCode 0
  hash (Just a) = (hashCode 1) `combineHashes` (hash a)

instance hashTuple :: (Hashable a, Hashable b) => Hashable (Tuple a b) where
  hash (Tuple m n) = (hash m) `combineHashes` (hash n)

instance hashEither :: (Hashable a, Hashable b) => Hashable (Either a b) where
  hash (Left a) = hashCode 0 `combineHashes` hash a
  hash (Right b) = hashCode 1 `combineHashes` hash b

compareEqual :: forall a. Eq a => a -> a -> Maybe Ordering
compareEqual m n = if m == n then Just EQ else Nothing

duplicates :: forall a. Hashable a => Ord a => Array a -> Array a
duplicates = nubBy hashCompare
  where
    hashCompare :: forall a. Hashable a => Ord a => a -> a -> Ordering
    hashCompare x y = if x `hashEqual` y
                        then unsafePartial $ fromJust $ x `compareEqual` y
                        else compare x y

newtype Hour = Hour Int

instance eqHour :: Eq Hour where
  eq (Hour n) (Hour m) = mod n 12 == mod m 12

instance hashableHour :: Hashable Hour where
  hash (Hour m) = HashCode ((m `mod` 65355) `mod` 12)
