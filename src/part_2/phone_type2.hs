{-
Define the types, the record and the function as in Task 2.1 with the following changes:

Now, instead of using type synonyms, define data types CountryCode and PhoneNo so that both of them have a value constructor that takes an integer.
Derive instances for Eq and Ord for CountryCode and PhoneNo and make Show instances for them so that:
CountryCode: print '+' in front of the number.
PhoneNo: print only the number.
Make a function for both of them (toCountryCode and toPhoneNo) that takes an Integer and throws an error if the integer is negative otherwise it creates the value.

If CountryCode is negative, the error should be "Negative country code" and if PhoneNo is negative, the error should be "Negative phone number" and you should 
follow these literally to pass the automatic testing.

Derive an instance for Eq, Ord, Read, and Show for PhoneType.

Again, using the record syntax, define Phone type for phone numbers that has only one value constructor with fields
phoneType :: PhoneType,
countryCode :: CountryCode, (This time the type defined as above)
and phoneNo :: PhoneNo. (This time the type defined as above)

Derive an instance for Eq and Ord for the record, but for Show make it "pretty-print" the information in this form:
<country code><space><phone number><space><phone type in parenthesis>
e.g. +358 123456789 (WorkLandline)
-}


data CountryCode = CountryCode {code :: Integer
                               } deriving (Eq, Ord) 
data PhoneNo = PhoneNo {num :: Integer
                       } deriving (Eq, Ord) 
 
instance Show CountryCode where
    show (CountryCode code) = "+" ++ show code

instance Show PhoneNo where
    show (PhoneNo num) = show num

toCountryCode :: Integer -> CountryCode
toCountryCode c
    | c < 0 = error "Negative country code"
    | otherwise = CountryCode c

toPhoneNo :: Integer -> PhoneNo
toPhoneNo c
    | c < 0 = error "Negative phone number"
    | otherwise = PhoneNo c

data PhoneType = WorkLandline | PrivateMobile | WorkMobile | Other deriving (Show, Eq, Read, Ord)
data Phone = Phone { phoneType :: PhoneType
                   , countryCode :: CountryCode
                   , phoneNo :: PhoneNo
                   } deriving (Eq)

instance Show Phone where
    show (Phone x y z ) =  show y ++ " " ++ show z ++ " " ++ "(" ++ show x ++ ")"