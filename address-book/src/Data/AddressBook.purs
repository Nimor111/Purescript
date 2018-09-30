module Data.AddressBook where

import Prelude

import Effect (Effect)
import Effect.Console (log)

import Control.Plus (empty)
import Data.List (List(..), filter, head, null, nubBy)
import Data.Maybe (Maybe)

type Address =
  { street :: String
  , city :: String
  , country :: String
  }

type Entry =
  { firstName :: String
  , lastName :: String
  , address:: Address
  }

type AddressBook = List Entry

address :: String -> String -> String -> Address
address street city country = { street, city, country }

showEntry :: Entry -> String
showEntry entry = entry.firstName <> ", " <>
                  entry.lastName <> ": " <>
                  showAddress entry.address

showAddress :: Address -> String
showAddress address = address.street <> ", " <>
                      address.city <> ", " <>
                      address.country

emptyBook :: AddressBook
emptyBook = empty

{-- insertEntry :: Entry -> AddressBook -> AddressBook --}
{-- insertEntry entry ab = Cons entry ab --}

insertEntry :: Entry -> AddressBook -> AddressBook
insertEntry = Cons

query :: String -> String -> AddressBook -> Maybe Entry
query firstName lastName =
  head <<< filter filterEntry where
    filterEntry = \entry -> entry.firstName == firstName && entry.lastName == lastName

queryByStreet :: String -> AddressBook -> Maybe Entry
queryByStreet street =
  head <<< filter filterByAddress where
    filterByAddress = \entry -> entry.address.street == street

queryName :: String -> String -> AddressBook -> Boolean
queryName firstName lastName = null <<< filter filterByName where
  filterByName = \entry -> entry.firstName == firstName && entry.lastName == lastName

removeDuplicates :: AddressBook -> AddressBook
removeDuplicates =
  nubBy (\e1 e2 -> e1.firstName == e2.firstName && e1.lastName == e2.lastName)
