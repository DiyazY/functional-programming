module Phone_book_map
( addEntry
, findEntries
) where

import Phone_type2

import qualified Data.Map as Map
type Name = String
type Phonebook = Map.Map Name [Phone]

addEntry :: Name -> String -> String -> String -> Phonebook -> Phonebook
findEntries :: Name -> Phonebook -> [Phone]

findEntries name book = case phone of
        Just x -> x
        Nothing -> [] 
    where phone = Map.lookup name $ book


addEntry name phoneType countryCode phoneNumber phonebook
    | elem newPhone currentPhones = phonebook 
    | otherwise = Map.insert name (newPhone : currentPhones) phonebook
    where newPhone = readPhone phoneType countryCode phoneNumber
          currentPhones = Map.findWithDefault [] name phonebook