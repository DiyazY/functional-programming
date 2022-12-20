{-
Now solve Task 2.2 with the following changes:

-Use newtype for CountryCode and PhoneNo (EDIT: used to be PhoneType) 
so that they have constructors MakeCountryCode and MakePhoneNo (EDIT: used to be MakePhoneType).


-Change the definition of Phone so that country code and phone type are optional using Maybe. ( Maybe PhoneType and Maybe CountryCode)

-Implement the follwing functions and of course the necessary show functions.
```
toCountryCode :: Integer -> CountryCode
toPhoneNo :: Integer -> PhoneNo
fromPhoneNo :: PhoneNo -> Integer
readPhoneType :: String -> Maybe PhoneType
readCountryCode :: String -> Maybe CountryCode
readPhoneNo :: String -> PhoneNo
readPhone :: String -> String -> String -> Phone
```
-Change the Show instance of Phone so that it does not show country code or phone type if they are Nothing.

-Make the readPhone function accept empty strings for phone type and country code. If they are empty make them Nothing.
-}

-- predefinedCountryCodes = [1,2]

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
    | onlyDigits c == False = error "Incorrect country code"
    | (read c :: Integer) < 0 = error "Negative country code"
    | elem (read c :: Integer) predefinedCountryCodes == False = error "Unknown country code"
    | otherwise = CountryCode (read c :: Integer)

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
