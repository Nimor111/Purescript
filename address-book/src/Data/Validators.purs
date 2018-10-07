module Data.Validators where

import Control.Applicative
import Control.Apply
import Prelude

import Data.AddressBook (Person(..), PhoneNumber(..), person, address, phoneNumber)
import Data.String (length)
import Data.Either (Either(..))
import Data.Validation.Semigroup (V(..), invalid)

type Errors = Array String

type Address =
  { street :: String
  , city :: String
  , country :: String
  }

nonEmpty :: String -> String -> V Errors Unit
nonEmpty field "" = invalid ["Field '" <> field <> "' cannot be empty."]
nonEmpty _  _ = pure unit

lengthIs :: String -> Int -> String -> V Errors Unit
lengthIs field len value | length value < len =
  invalid ["Field '" <> field <> "' must have length " <> show len]
lengthIs _ _ _ =
  pure unit

validateAddress :: Address -> V Errors Address
validateAddress o =
  address <$> (nonEmpty "Street"    o.street       *> pure o.street)
          <*> (nonEmpty "City"      o.city         *> pure o.city)
          <*> (lengthIs "Country" 5 o.country      *> pure o.country)

{-- validatePerson :: Person -> Either String Person --}
{-- validatePerson (Person o) = --}
{--   person <$> (nonEmpty o.firstName *> pure o.firstName) --}
{--          <*> (nonEmpty o.lastName  *> pure o.lastName) --}
{--          <*> pure o.address --}
{--          <*> pure o.phones --}
