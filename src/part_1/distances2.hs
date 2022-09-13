{-
Write a function distanceFilter :: (String -> String -> Float) -> Float -> String -> [String] -> [String] that, 
given a distance function f, a Float d, a String s and a list of Strings ss, 
returns all the strings in ss that are at most d distance away from s.

You can use the following function for testing purposes:

distance3 :: String -> String -> Float
distance3 x y = fromIntegral $ abs $ length x - length y

Hint: Use the filter or foldl functions. You can also try using basic recursion or list comprehension.
-}

distance3 :: String -> String -> Float
distance3 x y = fromIntegral $ abs $ length x - length y

distanceFilter :: (String -> String -> Float) -> Float -> String -> [String] -> [String]
distanceFilter f d s ss = [str | str<-ss, (f s str)<=d ]