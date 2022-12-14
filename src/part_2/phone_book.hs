{-
Use the types and functions you have used in Task 2.3 in this task and the following types by copy-pasting all of those into your source file:

```
data PhoneBookEntry = PhoneBookEntry { name :: String , phone :: Phone } deriving(Eq, Ord, Show)
type PhoneBook = [PhoneBookEntry]
```
Make functions to:

-Find a list of entries by a name. (All entries where the name is the given one)
```
findEntries :: String -> PhoneBook -> PhoneBook
```

-Add a new entry, given a string for the name, the three strings for the phone like in Task 2.3 and the list of phone book entries to add the new entry to.
If there already exists an entry with the given name and the given number(phoneNo field in Phone), then make no change.
```
addEntry :: String -> String -> String -> String -> PhoneBook -> PhoneBook
```

The first string contains the name, the second phone type, third the country code, and the fourth contains the phone number.
-}
-- predefinedCountryCodes = [1,2] -- TODO test with addEntry "PersonA" "WorkLandline" "358" "123456789"
data PhoneBookEntry = PhoneBookEntry { name :: String , phone :: Phone } deriving(Eq, Ord, Show)
type PhoneBook = [PhoneBookEntry]

findEntries :: String -> PhoneBook -> PhoneBook
findEntries givenName book = [entry | entry<-book, (name entry) == givenName]

addEntry :: String -> String -> String -> String -> PhoneBook -> PhoneBook
addEntry name pt cc pn book 
    | elem entry book = book  -- check if the entry already exists
    | otherwise = [entry] ++ book  -- add the new entry to the phone book
    where entry = PhoneBookEntry { name = name, phone = readPhone pt cc pn }

readPhone :: String -> String -> String -> Phone
readPhone pt cc pn = Phone { phoneType = toPhoneType pt
                            , countryCode = toCountryCode cc
                            , phoneNo = toPhoneNo pn
                            }

data CountryCode = CountryCode {code :: Integer
                               } deriving (Eq, Ord) 
data PhoneNo = PhoneNo {num :: Integer
                       } deriving (Eq, Ord) 
 
instance Show CountryCode where
    show (CountryCode code) = "+" ++ show code

instance Show PhoneNo where
    show (PhoneNo num) = show num

toCountryCode :: String -> CountryCode
toCountryCode "" = error "Empty country code"
toCountryCode c
    | onlyDigits c == False = error "Incorrect country code"
    | (read (trim c) :: Integer) < 0 = error "Negative country code"
    | elem (read (trim c) :: Integer) predefinedCountryCodes == False = error "Unknown country code"
    | otherwise = CountryCode (read (trim c) :: Integer)

toPhoneNo :: String -> PhoneNo
toPhoneNo "" = error "Empty phone number"
toPhoneNo c
    | onlyDigits c == False = error "Incorrect phone number"
    | (read c :: Integer) < 0 = error "Negative phone number"
    | otherwise = PhoneNo (read c :: Integer)

data PhoneType = WorkLandline | PrivateMobile | WorkMobile | Other deriving (Show, Eq, Read, Ord)
data Phone = Phone { phoneType :: PhoneType
                   , countryCode :: CountryCode
                   , phoneNo :: PhoneNo
                   } deriving (Ord)

instance Show Phone where
    show (Phone x y z ) =  show y ++ " " ++ show z ++ " " ++ "(" ++ show x ++ ")"

instance Eq Phone where
    Phone x y z  ==  Phone x' y' z' = z == z'

toPhoneType :: String -> PhoneType
toPhoneType "" = error "Missing phone type"
toPhoneType pt 
    | pt == "WorkLandline"= read pt
    | pt == "PrivateMobile"= read pt
    | pt == "WorkMobile"= read pt
    | pt == "Other"= read pt
    | otherwise = error "Incorrect phone type"


onlyDigits :: String -> Bool
onlyDigits [] = False
onlyDigits [s] = isDigit s
onlyDigits (x:xs)
    | x == '+' = onlyDigits xs
    | isDigit x = onlyDigits xs
    | otherwise = False

isDigit :: Char -> Bool
isDigit c = c >= '0' && c <= '9'

trim (x:xs)
    | x=='+' = xs
    | otherwise = x:xs
