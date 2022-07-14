{-
*learning if..else*
We represent playing cards with (Char, Int) pairs. ‘s’ means spades, ‘h’ hearts, ‘c’ clubs’ and ‘d’ diamonds, with number values going from 2 to 14 (Ace being 14). 
Consider a game, where a player is dealt two cards and wins credits based on the following rules:

If the player has the Ace of Spades (‘s’, 14), then the player wins 14 credits.
Otherwise if the player has two consecutive numbers of the same suit, then the player wins 8 credits.
Otherwise if the player has a pair (same number values), then the player wins 6 credits.
Otherwise if the player has to consecutive numbers, then the player wins 4 credits.
Otherwise if the player has two cards of the same suit, then the player wins 2 credits.
Otherwise, the player wins 0 credits.
Write a function credits :: (Char, Int) -> (Char, Int) -> Int that evaluates the given credits.

You can assume that the given cards are real.
-} 
-- credits :: (Char, Int) -> (Char, Int) -> Int
-- credits (c1, val1) (c2, val2)= if(c1=='s' && val1==12 && c1 ==c2 && val1==val2)then 14 else 
--     if (c1 == c2 && abs(val1-val2)==1) then 8 else
--         if (val1==val2) then 6 else
--             if (abs(val1-val2)==1) then 4 else
--                 if (c1==c2) then 2 else 0

-- *learning pattern matching and guards*
credits :: (Char, Int) -> (Char, Int) -> Int
credits (c1, val1) (c2, val2)
    | c1=='s' && val1==12 && c1 ==c2 && val1==val2 = 14
    | c1 == c2 && abs(val1-val2)==1 = 8
    | val1==val2 = 6
    | abs(val1-val2)==1 = 4
    | c1==c2 = 2
    | otherwise = 0