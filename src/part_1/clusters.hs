{-
Write a function clusters that is given:
- f, a distance function of type String -> String -> Float (like the ones in Task 1.10)
- d :: Float
- ss :: [String]
For each string s  in ss, the function clusters computes a "cluster", 
ie a list of similar strings in ss (strings that are at most distance d from the s). 
The list of strings similar to s should also contain s (if the distance function allows).

The clusters and the list of clusters may be in any order. The grader sorts them.

Calling this function with function of

1.10 a), d=0.3 and ss=["aaabc", "aabdd", "a", "aa", "abdd", "bcbcb", "", "abcdefghij"] 
should return [[""],["a","aa"],["a","aa","aaabc"],["aa","aaabc","aabdd","bcbcb"],["aaabc","aabdd","abdd"],["aaabc","bcbcb"],["aabdd","abdd"],["abcdefghij"]] (in some order).

1.10 b), d=0.2 and ss=["123a","456789b","45","abc", "ab1", "a12", "abcdefghij"] 
should return [[],[],[],["123a","45","456789b","a12"],["123a","45","456789b","a12"],["45","456789b"],["45","456789b"]] (in some order).
-}

clusters :: (String -> String -> Float) -> Float -> [String] -> [[String]]
clusters f d ss = [[str | str<-ss, (f str s) <= d ]| s<-ss]

distance2 :: String -> String -> Float
distance2 [] [] = 0
distance2 s1 s2 = ( fromIntegral(length [x | x<-s1, not (isDigit x)]) + fromIntegral(length [x | x<-s2, not (isDigit x)] ))/fromIntegral(length s1 + length s2)

isDigit :: Char -> Bool
isDigit c = c >= '0' && c <= '9'