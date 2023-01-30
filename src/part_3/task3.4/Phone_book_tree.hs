{-
Now solve Task 2.4 using binary search trees so that you create one module called Phone_type2 (in a file Phone_type2.hs) and Phone_book_tree (in a file Phone_book_tree.hs)  
- those are the files to be returned. (See section "recursive data structures" in Chapter 8 of the textbook, if binary search trees are unfamiliar to you.)

The file Phone_type2 needs to follow the instructions for Task 2.6. (You can in fact submit the same file, if you have solved Task 2.6)

Let's assume you have the following in your code (copy-paste from here):
```
import Phone_type2
type Name = String
data Phonebook = Empty | Node String [Phone] Phonebook Phonebook deriving (Show,Eq)
```

Make the module Phone_book_tree export datatype Phonebook(Empty) and functions
```
addEntry :: Name -> String -> String -> String -> Phonebook -> Phonebook
findEntries :: Name -> Phonebook -> [Phone]
```
The first string contains the phone type, second the country code, and the third contains the phone number. 
You can assume that the values added to the phone book are correct.

Please read the following:

In data Phonebook statement there is a list of phones for a name - so when adding an entry with an existing name it should be added 
to the list rather than creating a new node.
A thing I was not prepared for is that some students have added the new telephone numbers 
for a person using list concatenation (++)  instead of adding to the front of the list (:) 
I could get away with this by sorting the lists before comparing results, and at first I considered that. 
However, I would not like to encourage using (++) without a good reason, as in principle it means first going through 
the existing list and then adding to the end of it, while (:) adds to the front of the list without a need to traverse the list, and thereby I want to encourage using (:) when it is straightforward.

So, add the new numbers to the front of the list using (:) and give those lists as they are in findEntries
-}


module Phone_book_tree
( Phonebook(Empty)
, addEntry
, findEntries
) where

import Phone_type2
type Name = String
data Phonebook = Empty | Node String [Phone] Phonebook Phonebook deriving (Show,Eq)

addEntry :: Name -> String -> String -> String -> Phonebook -> Phonebook
findEntries :: Name -> Phonebook -> [Phone]

addEntry name phoneType phoneCountry phoneNumber Empty = Node name [readPhone phoneType phoneCountry phoneNumber] Empty Empty
addEntry name phoneType phoneCountry phoneNumber (Node n phones left right)
    | name == n = Node n (addEntryToList newPhone phones) left right
    | name > n = Node n phones left (addEntry name phoneType phoneCountry phoneNumber right)
    | name < n  = Node n phones (addEntry name phoneType phoneCountry phoneNumber left) right
    where newPhone = readPhone phoneType phoneCountry phoneNumber

findEntries _ Empty = []
findEntries name (Node n phones left right)
    | name == n = phones
    | name > n = findEntries name right
    | name < n = findEntries name left

addEntryToList :: Phone -> [Phone] -> [Phone]
addEntryToList entry list 
    | elem entry list = list  -- check if the entry already exists
    | otherwise = [entry] ++ list   -- add the new entry to the phone book --> this would be better [entry:book]
