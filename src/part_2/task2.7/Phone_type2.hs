{-
Now solve Task 2.4 using Data.Map so that you create one module called Phone_type2 (in a file Phone_type2.hs) 
and Phone_book_map (in a file Phone_book_map.hs) - those are the files to be returned.

The file Phone_type2 needs to follow the instructions for Task 2.6. (You can in fact submit the same file, 
if you have solved Task 2.6). You should insert the following into your Phone_type2.hs:
```
predefinedCountryCodes = [44,358]
```

Those will be the codes used in testing.
```
import qualified Data.Map as Map
type Name = String
type Phonebook = Map.Map Name [Phone]
```

Make the module Phone_book_map export the following functions:
```
addEntry :: Name -> String -> String -> String -> Phonebook -> Phonebook
findEntries :: Name -> Phonebook -> [Phone]
```

The first string contains the phone type, second the country code, and the third contains the phone number. 
When adding a new phone number for a person which already exists, the new number should be added to the front of the list.
-}
module Phone_type2 
( fromPhoneNo
, toPhoneNo
, readPhone
, Phone
) where

predefinedCountryCodes = [44,358]

readPhone :: String -> String -> String -> Phone
readPhone pt cc pn = Phone { phoneType = readPhoneType pt, countryCode = readCountryCode cc, phoneNo = readPhoneNo pn}
readPhoneNo :: String -> PhoneNo
readPhoneNo p 
    | onlyDigits p == False = error "Incorrect phone number"
    | otherwise = toPhoneNo (read p :: Integer)

readCountryCode :: String -> Maybe CountryCode
readCountryCode "" = Nothing
readCountryCode cc = Just (toCountryCode cc)

readPhoneType :: String -> Maybe PhoneType
readPhoneType "" = Nothing
readPhoneType pt = Just (toPhoneType pt)

fromPhoneNo :: PhoneNo -> Integer
fromPhoneNo p =  (num p)

data CountryCode = CountryCode {code :: Integer
                               } deriving (Eq, Ord) 
data PhoneNo = PhoneNo {num :: Integer
                       } deriving (Eq, Ord, Read) 
 
instance Show CountryCode where
    show (CountryCode code) = "+" ++ show code

instance Show PhoneNo where
    show (PhoneNo num) = show num

toCountryCode :: String -> CountryCode
toCountryCode "" = error "Empty country code"
toCountryCode c
    | onlyDigits (trimPlus c) == False = error "Incorrect country code"
    | (read (trimPlus c) :: Integer) < 0 = error "Negative country code"
    | elem (read (trimPlus c) :: Integer) predefinedCountryCodes == False = error "Unknown country code"
    | otherwise = CountryCode (read (trimPlus c) :: Integer)

toPhoneNo :: Integer -> PhoneNo
toPhoneNo c
    | c < 0 = error "Negative phone number"
    | otherwise = PhoneNo (c)

data PhoneType = WorkLandline | PrivateMobile | WorkMobile | Other deriving (Show, Eq, Read, Ord)
data Phone = Phone { phoneType :: Maybe PhoneType
                   , countryCode :: Maybe CountryCode
                   , phoneNo :: PhoneNo
                   } deriving (Ord)

instance Show Phone where
  show (Phone pt cc pn) = 
    trimW (concat $ filter (not . null) [showCountryCode cc, showPhoneNo pn, showPhoneType pt])
    where showPhoneType Nothing = ""
          showPhoneType (Just pt) = " (" ++ show pt ++ ")"
          showCountryCode Nothing = ""
          showCountryCode (Just cc) = " " ++ show cc
          showPhoneNo (pn) = " " ++ show pn


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
    | x == '-' = onlyDigits xs
    | isDigit x = onlyDigits xs
    | otherwise = False

isDigit :: Char -> Bool
isDigit c = c >= '0' && c <= '9'

trimW (x:xs)
    | x==' ' = xs
    | otherwise = x:xs

trimPlus (x:xs)
    | x=='+' = xs
    | otherwise = x:xs





