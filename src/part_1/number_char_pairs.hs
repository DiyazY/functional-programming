{-
Let us number the smaller case characters from ‘a’ to ‘z’ with numbers starting from 1, that is, ‘a’ is given 1, ‘b’ is given number 2, etc.
Write two functions:
A function charsDivisibleBy :: Int -> [Char] that, given a number n, returns all the characters that have a number divisible by n.
A function charsProductOf :: [Int] -> [Char] that, given a list of numbers ns, returns all the characters that have a number that is a product of any two numbers in ns.
As an example, charsDivisibleBy 2 = "bdfhjlnprtvxz" and charsProductOf [2,3,4] = "fhl".
-}


charsDivisibleBy :: Int -> [Char]
charsDivisibleBy 0 = []
charsDivisibleBy 1 = ['a'..'z']
charsDivisibleBy x = [ a | (n, a)<-zip [1..26] ['a'..'z'], n `mod` x == 0 ]

charsProductOf :: [Int] -> [Char]
charsProductOf [] = []
charsProductOf [_] = []
charsProductOf list = [ a | (n, a) <- zip [1..26] ['a'..'z'], x<-products,  n==x ] 
                    where products = [g*y | (i, g)<-zip [0..] list, (j, y)<-zip [0..] list, j>i && g*y<=26]