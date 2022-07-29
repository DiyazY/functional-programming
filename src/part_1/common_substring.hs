{-
Write a function commonSubstring :: String -> String -> String that, given two strings s1 and s2, 
computes a common “substring” of s1 and s2 as follows. The function finds the earliest common 
character c (closest to head of either s1 or s2 appearing in both sequences). 
The function removes c and all the characters before it in both strings, puts c in the output string, and continues.

If there are two candidates for the earliest common character, pick the one from s1.
-}

commonSubstring :: String -> String -> String
commonSubstring _ [] = []
commonSubstring [] _ = []
commonSubstring (s1:rest) s2
    | length str > 0 = head str : commonSubstring rest (tail(str))
    | otherwise = commonSubstring rest s2
    where str = returnRest s1 s2

returnRest _ [] = []
returnRest x (y:rest) 
    | x == y = y:rest
    | otherwise = returnRest x rest
