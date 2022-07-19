{-
*practising guards and splitting your code into smaller functions*

Write a function validate :: String -> Bool that, given a string validates the string as a Finnish IBAN code.

For details, see https://en.wikipedia.org/wiki/International_Bank_Account_Number#Validating_the_IBAN.

You will also need the following information:

Length of a Finnish IBAN code is 18.
Finnish IBAN begins with the country code FI.
All the characters after the country code are digits.
You can assume that the input is without whitespaces.
-}
validate :: String -> Bool
validate x 
    | y <- x, length y == 18,
    (y !! 0 == 'F' && y !! 1 == 'I'),
     onlyDigits (tail (tail y)),
     ((read (moveCharsToEnd(replace' y)) :: Integer) `mod` 97 == 1) = True
    | otherwise = False



onlyDigits :: String -> Bool
--Checks whether given list consists only of digits.
onlyDigits [] = False
onlyDigits [s] = isDigit s
onlyDigits (x:xs)
    | isDigit x = onlyDigits xs
    | otherwise = False

replace' :: String -> String
--Replaces the country code FI with 1518 at the beginning of the string.
replace' xs | x <- xs = '1':'5':'1':'8' : drop 2 xs

isDigit :: Char -> Bool
isDigit c = c >= '0' && c <= '9'

--Takes the first 6 (4) characters and puts them to end of the string.
--6 because I have converted FI to 1518, so that's two additional chars.
moveCharsToEnd :: String -> String
moveCharsToEnd x | x <- x = drop 6 x ++ take 6 x

