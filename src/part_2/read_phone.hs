{-
Implement a readPhone function (String -> String -> String -> Phone) as follows:

The function
a) reads the phone type from the first string.

b) reads the country code code from the second string in the following way:
1. if the code has a '+' or "00" in the front, remove them
2. check that the code exists in a predefined list of country codes  (predefinedCountryCodes)
3. read an integer out of the remaining string
4. call the function(that checks that the integer is >= 0) you created in Task4.2 with the integer to create the value for CountryCode.

c) reads the phone number from the third string by reading it as an integer and then calling the function you created in Task4.2.

If the input is correct, create a telephone number. Else, call error to throw an exception. Note that the read function or your functions 
from Task4.2 may also generate an exception.

The exceptions your functions should throw are the following:
"Negative country code"*
"Negative phone number"*
"Missing phone type"   (this is for empty PhoneType)*
"Incorrect phone type"   (this is for a PhoneType different than the acceptable ones)*
"Incorrect country code" (this is for CountryCode that cannot be read as an integer, e.g. contains letters)*
"Empty country code" (this is for CountryCode that is an empty string*
"Unknown country code" (this is a country code not in the list)
"Empty phone number"  (this is for a PhoneNo that is an empty string)*
"Incorrect phone number"  (this is for a PhoneNo that cannot be read as an integer, e.g. contains letters)*


The predefined country codes will be available to your program as a function predefinedCountryCodes that returns a list of integers. 
You can test it with something like

predefinedCountryCodes = [1,2]
Just remove that before submitting.
-}

-- predefinedCountryCodes = [1,2]

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
    | (read c :: Integer) < 0 = error "Negative country code"
    | (isInTheList (read c :: Integer) predefinedCountryCodes) == False = error "Unknown country code"
    | otherwise = CountryCode (read c :: Integer)

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
                   } deriving (Eq)

instance Show Phone where
    show (Phone x y z ) =  show y ++ " " ++ show z ++ " " ++ "(" ++ show x ++ ")"


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
    | isDigit x = onlyDigits xs
    | otherwise = False

isDigit :: Char -> Bool
isDigit c = c >= '0' && c <= '9'

isInTheList _ [] = False
isInTheList code [s]  = code == s
isInTheList code (x:xs)
    | code == x = True
    | otherwise = isInTheList code xs
