{-
*learning a list comprehension*
Write a function headOrLast :: [String] -> Char -> [String] that, given a list of strings and a character, 
evaluates to a list with all the strings of the input list that either begin or end with the input character.
-}
headOrLast::[String] -> Char -> [String]
headOrLast strings char = [s|s<-strings, head s==char || last s==char] 
