{-
Make a program that reads repeatedly lines from terminal (that is a program  that can be compiled ie it includes the main function). 
If the line is follows the format of one of the two  lines below, that is, there is first a command encode or decode followed by an Int 
and then some text.

```
encode Int the rest of the line can have an arbitrary number of words
decode Int the rest of this line can also have an arbitrary number of words
```
then use the encode and decode functions given in the Learn you a Haskell book to encode/decode all the other words in the line. 
You can (and in fact should) use the words function to separate the words in the input line.

The encode and decode are given below, together with the potetially useful readMaybe function.
```
import Data.Char

encode :: Int -> String -> String 
encode shift msg = 
 let ords = map ord msg 
     shifted = map (+ shift) ords 
 in map chr shifted


decode :: Int -> String -> String
decode shift msg = encode (negate shift) msg

readMaybe :: (Read a) => String -> Maybe a
readMaybe st = case reads st of [(x,"")] -> Just x
                                _ -> Nothing
```

If the two first words are not encode followed by an Int or decode followed by an Int, output: “I cannot do that”
Stop when the user types “quit” and respond to that with "bye"
A possible execution (user inputs are in blue):
```
hello there
I cannot do that
encode 4 well this is an easy task
{ipp xlmw mw er iew} xewo
encode 4 Merry Christmas
Qivv} Glvmwxqew
decode 4 Qivv} Glvmwxqew
Merry Christmas
quit
bye
```
-}
import Data.Char

main = do
    exp <- getLine  
    if exp == "quit"
        then do
            putStrLn "bye"
            return()
        else do
            let list = words exp
            if (length list > 2 && (list !! 0 == "encode" || list !! 0 == "decode")&& onlyDigits((list !! 1))) 
                then do
                    let rest = drop 2 list
                    let num = (read (list !! 1) :: Int)
                    if ((list !! 0) == "encode") 
                        then do 
                            let result = unwords [encode num x | x<-rest]
                            putStrLn (result)
                        else do
                            let result = unwords [decode num x | x<-rest]
                            putStrLn (result)
                else do
                    putStrLn "I cannot do that"
            main

encode :: Int -> String -> String 
encode shift msg = 
 let ords = map ord msg 
     shifted = map (+ shift) ords 
 in map chr shifted

onlyDigits :: String -> Bool
onlyDigits [] = False
onlyDigits [s] = isDigit s
onlyDigits (x:xs)
    | isDigit x = onlyDigits xs
    | otherwise = False

decode :: Int -> String -> String
decode shift msg = encode (negate shift) msg