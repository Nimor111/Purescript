module Data.AddressBook where

import Prelude

import Control.Plus (empty)
import Data.List (List(..), filter, head, null, nubBy)
import Data.Maybe (Maybe)
import Data.Show (class Show)
import Data.Foldable (foldMap)

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

data PhoneType = HomePhone | WorkPhone | CellPhone | OtherPhone

newtype PhoneNumber = PhoneNumber
  { type_ :: PhoneType
  , number :: String
  }

newtype Person = Person
  { firstName :: String
  , lastName :: String
  , address :: Address
  , phones :: Array PhoneNumber
  }

instance showPhoneNumber :: Show PhoneNumber where
  show (PhoneNumber {type_, number}) = show type_ <> ": " <> show number

instance showPerson :: Show Person where
  show (Person {firstName, lastName, address, phones}) = lastName
    <> ", "
    <> firstName
    <> ": "
    <> showAddress address
    <> foldMap (\x -> ", " <> show x) phones

instance showPhoneType :: Show PhoneType where
  show HomePhone = "Home"
  show WorkPhone = "Work"
  show CellPhone = "Cell"
  show OtherPhone = "Other"

person :: String -> String -> Address -> Array PhoneNumber -> Person
person firstName lastName address phones =
  Person { firstName, lastName, address, phones }

examplePerson :: Person
examplePerson = person "John" "Smith" (address "123 Fake St." "FakeTown" "CA")
  [phoneNumber HomePhone "555-555-5555", phoneNumber CellPhone "555-555-0000"]

phoneNumber :: PhoneType -> String -> PhoneNumber
phoneNumber type_ number =
  PhoneNumber { type_ , number }

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
