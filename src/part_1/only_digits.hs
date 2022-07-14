{-
*learning recursion*
Write a function onlyDigits :: String -> Bool that, given a string, checks whether the string contains only digits or not. 
Empty string should return false.
-}
onlyDigits :: String -> Bool
onlyDigits "" = False
onlyDigits (x:xs)   
    | x == '0' && isDigit = True
    | x == '1' && isDigit = True
    | x == '2' && isDigit = True
    | x == '3' && isDigit = True
    | x == '4' && isDigit = True
    | x == '5' && isDigit = True
    | x == '6' && isDigit = True
    | x == '7' && isDigit = True
    | x == '8' && isDigit = True
    | x == '9' && isDigit = True
    | otherwise = False
    where isDigit = case xs of [] -> True
                               xs -> onlyDigits xs
