module Phone_book
( addEntry
, findEntries
) where

import Phone_type2

data PhoneBookEntry = PhoneBookEntry { name :: String , phone :: Phone } deriving(Eq, Ord, Show)
type PhoneBook = [PhoneBookEntry]

findEntries :: String -> PhoneBook -> PhoneBook
findEntries givenName book = [entry | entry<-book, (name entry) == givenName]

addEntry :: String -> String -> String -> String -> PhoneBook -> PhoneBook
addEntry name pt cc pn book 
    | elem entry book = book  -- check if the entry already exists
    | otherwise = [entry] ++ book  -- add the new entry to the phone book --> this would be better [entry:book]
    where entry = PhoneBookEntry { name = name, phone = readPhone pt cc pn }