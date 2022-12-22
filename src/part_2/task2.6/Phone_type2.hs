{-
Organise your solution for Task 2.4 into modules so that you create one module called Phone_type2 (in a file Phone_type2.hs) 
and Phone_book (in a file Phone_book.hs)  - those are the files to be returned.

Make Phone_type2 export type Phone and the following functions:
```
fromPhoneNo :: PhoneNo -> Integer
toPhoneNo :: Integer -> PhoneNo
readPhone :: String -> String -> String -> Phone
```
This means that Phone_type2.hs is to contain everything that is needed for addEntry and findEntries, introduced below. 
If you use some other function names between Phone_type2 and Phone_book, it probably does not matter.

Include in the module Phone_type2 the following:
```
predefinedCountryCodes = [44,358]
```

Make the module Phone_book export functions
```
addEntry :: String -> String -> String -> String -> PhoneBook -> PhoneBook

findEntries :: String -> PhoneBook -> PhoneBook
```
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





