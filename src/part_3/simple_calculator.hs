{-
Make a program that reads repeatedly lines from terminal (that is a complete program you need to compile and that includes the main function). 
If the line is follows the format of one of the three lines below:
```
Int ‘+’ Int
Int ‘-‘ Int
Int ‘*’ Int
```

then calculate the result of the arithmetic operation.
Otherwise output an error message: “I cannot calculate that”
Stop when the user types “quit” and respond to that with "bye"
A possible execution (user inputs are in blue):
```
3 + 5
8
a + 3
I cannot calculate that
4 - 3
1
quit
bye
```
Depending on how you implement your program, you may find the following function useful:

```
readMaybe :: (Read a) => String -> Maybe a
readMaybe st = case reads st of [(x,"")] -> Just x
                                _ -> Nothing
```

To build:
1. ghc --make simple_calculator.hs
2. ./simple_calculator
-}

main = do
    exp <- getLine  
    if exp == "quit"
        then do
            putStrLn "bye"
            return()
        else do
            let list = split exp
            let operator = getOperator exp
            if (length list == 2 && onlyDigits(list !! 0) && onlyDigits(list !! 0)) 
                then do
                    let result = calc list operator
                    putStrLn (show result)
                else do
                    putStrLn "I cannot calculate that"
            main

getOperator :: String -> String
getOperator [] = ""
getOperator (c:rest) | c == '+' = "+"
                     | c == '-' = "-"
                     | c == '*' = "*"
                     | otherwise = getOperator rest

split :: String -> [String]
split [] = [""]
split (c:cs) | c == '+' || c == '-' || c == '*'  = "" : rest
             | otherwise = (c : head rest) : tail rest
    where rest = split cs

onlyDigits :: String -> Bool
onlyDigits [] = False
onlyDigits [s] = isDigit s
onlyDigits (x:xs)
    | isDigit x = onlyDigits xs
    | otherwise = False

isDigit :: Char -> Bool
isDigit c = c >= '0' && c <= '9' || c == ' '

calc :: [String] -> String -> Integer
calc list operator
    | operator == "+" = (read (list !! 0) :: Integer) + (read (list !! 1) :: Integer)
    | operator == "-" = (read (list !! 0) :: Integer) - (read (list !! 1) :: Integer)
    | operator == "*" = (read (list !! 0) :: Integer) * (read (list !! 1) :: Integer)

