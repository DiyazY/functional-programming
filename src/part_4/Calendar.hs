{-
You can utilise functions found at dates.hs.
However, you need to copy-paste and modify the necessary parts into your code.

You need to implement a dialogue system that responds to user inputs, given line-by-line. 
The exercise is about calendar events, having a date, a place, and a name.

The user inputs you need to manage are:
```
Event <name> happens at <place> on <date>
Tell me about <eventname>
What happens on <date>
What happens at <place>
Quit
```
The values are given inside of < and >. The format of the date is YYYY-MM-DD.
Example:
```
Event 'Event X' happens at 'Place Y' on '2019-10-08'
Tell me about 'Event X'
What happens on '2019-10-08'
What happens at 'Place Y'
Quit
```

You need to some way separate the values in the input. The number of spaces (whitespace in general) is not important, 
so 'Event    A' is the same as 'Event A', and in the inputs, there will always be just one space (' ') used as a separator, 
and there are no extra spaces before or after the "'" character, in particular, nothing like :
event '   Place X  ' 

While you can separate the values as you like, we have a couple of suggestions.  You may use the groupBy function (see e.g. http://learnyouahaskell.com/modules#data-list.) 

If the input is not one of those (including format problems), the response is
```
I do not understand that. I understand the following:
*Event <name> happens at <place> on <date>
*Tell me about <eventname>
*What happens on <date>
*What happens at <place>
*Quit
```
Event <name> happens at <place> on <date>
For this input, if it is correct, the response should be:
```
ok
```

 At the same time, the input information should be stored in the list. If an event with the same name already existed, 
 the information is changed (old event removed, new one added).

If the date is not correct even though the format is otherwise ok, then your program should respond:
```
Bad date
```

There are no rules for event name and place content, ie they do not need to be checked.

```
Tell me about <eventname>
```

For this input, the response is of format:
```
Event <name> happens at <place> on <date>
```

or, if such an event does not exist , then the answer is:
```
I do not know of such event
```

```
What happens on <date>
```
For this input, the response is a list of statements, of format:
```
Event <name> happens on <date>
```

All events taking place on that date should be listed, sorted in ascending order by name.
If there are no events with the date, then the answer is:
```
Nothing that I know of
```

```
What happens at <place>
```
For this input, the response is a list of statements, each on a separate line, of format:

```
Event <name> happens at <place>
```
All events taking place on that date should be listed, sorted in ascending order by name.
If there are no events that match that place, then the answer is:
```
Nothing that I know of
```

```
Quit
```

For the fifth input, the response is:
```
bye
```
and your program should terminate.

Following file will be appended at the end of your source code to create an executable program. 
Your task is to implement the function doCommand and all the other functions that you need to complete the task. 
doCommand performs the necessary activities and returns the possibly updated event list (unless the command is Quit).
```
data EventInfo = EventInfo { name :: String
                           , place :: String
                           , date :: Date
                           } deriving(Eq)

main = loop $ return []
 
loop :: IO [EventInfo] -> IO ()
loop ioEvents =
 do
 input <- getLine
 if input == "Quit"
   then putStrLn "bye"
   else doCommand input ioEvents
```

Template for doCommand:
```
doCommand :: String -> IO [EventInfo] -> IO ()
doCommand input ioEvents = do
  events <- ioEvents --Now you can use events as [EventInfo]
  ...
  loop $ return possiblyChangedEvents
``` 

A possible execution:
```
Event 'Event A' happens at 'Place A1' on '2001-20-40'
Bad date
Event 'Event A' happens at 'Place A1' on '2001-02-02'
ok
Event 'Event A' happens at 'Place A1' on '2001-02-03'
ok
Event 'Event A' happens at 'Place A2' on '2001-02-03'
ok
Event 'Event G1' happens at 'Place G' on '2008-02-02'
ok
Event 'Event G21' happens at 'Place G' on '2008-02-02'
ok
Event 'Event G22' happens at 'Place G' on '2008-02-02'
ok
Event 'Event G0' happens at 'Place G' on '2009-02-02'
ok
Event 'Event G4' happens at 'Place G' on '2010-02-02'
ok
Event 'Event G5' happens at 'Place G' on '2011-02-02'
ok
Tell me about 'Event ABC'
I do not know of such event
Tell me about 'Event A'
Event Event A happens at Place A2 on 2001-02-03
Tell me 'Place A'
I do not understand that. I understand the following:
*Event <name> happens at <place> on <date>
*Tell me about <eventname>
*What happens on <date>
*What happens at <place>
*Quit
What happens on '2001-02-02'
Nothing that I know of
What happens on '2001-02-03'
Event Event A happens on 2001-02-03
What happens at 'Place A1'
Nothing that I know of
What happens at 'Place A2'
Event Event A happens at Place A2
What happens at 'Place G'
Event Event G0 happens at Place G
Event Event G1 happens at Place G
Event Event G21 happens at Place G
Event Event G22 happens at Place G
Event Event G4 happens at Place G
Event Event G5 happens at Place G
Quit
bye
```
-}

import Data.List (sortBy, groupBy)
import Data.List (unfoldr)
import Data.Char (isSpace)
import Text.Printf
import Data.Time.Calendar (fromGregorianValid)


-- comment it
data EventInfo = EventInfo { name :: String
                           , place :: String
                           , date :: Date
                           } deriving(Eq)

main = loop $ return []
 
loop :: IO [EventInfo] -> IO ()
loop ioEvents =
 do
 input <- getLine
 if input == "Quit"
   then putStrLn "bye"
   else doCommand input ioEvents

printEvents :: [EventInfo] -> IO ()
printEvents events = if null events
    then putStrLn "Nothing that I know of"
    else putStrLn (unlines (map formatEvent events))
-- comment it



data Date = Date { year :: Int
                 , month :: Int
                 , day :: Int
                 } deriving (Eq, Ord)

instance Show Date where
  show (Date y m d) = printf "%04d-%02d-%02d" y m d


readMaybe :: (Read a) => String -> Maybe a
readMaybe st = case reads st of [(x,"")] -> Just x
                                _ -> Nothing

formatEvent :: EventInfo -> String
formatEvent event = name event ++ " on " ++ show (date event)

instance Show EventInfo where
  show (EventInfo name place date) = "Event " ++ name ++ " happens at " ++ place ++ " on " ++ show date

parseDate :: String -> Maybe Date
parseDate str =
  case split '-' str of
    [y, m, d] ->
      case (readMaybe y :: Maybe Integer, readMaybe m :: Maybe Int, readMaybe d :: Maybe Int) of
        (Just year, Just month, Just day) ->
          case fromGregorianValid year month day of
            Just _ -> Just $ Date (fromInteger year) month day
            Nothing -> Nothing

split :: Char -> String -> [String]
split ch str = 
  let (pre, suf) = break (== ch) str
  in case suf of
       [] -> [pre]
       _ -> pre : split ch (tail suf)

findEventByName :: [EventInfo] -> String -> Maybe EventInfo
findEventByName events name = case filter (\(EventInfo n _ _) -> n == name) events of
                                [] -> Nothing
                                (x:_) -> Just x

getEventsOnDate :: [EventInfo] -> Date -> [EventInfo]
getEventsOnDate events date = filter (\(EventInfo _ _ d) -> d == date) events

getEventsAtPlace :: [EventInfo] -> String -> [EventInfo]
getEventsAtPlace events place = filter (\(EventInfo _ p _) -> p == place) events

doCommand :: String -> IO [EventInfo] -> IO ()
doCommand input ioEvents = do
  events <- ioEvents
  let parts = splitSentence input
  case parts of
    ["Event", name, "happens", "at", place, "on", dateStr] ->
      case parseDate dateStr of
        Nothing -> do 
          putStrLn "Bad date"
          loop ioEvents
        Just date -> do
          let events' = filter (\(EventInfo n _ _) -> n /= name) events
          putStrLn "ok"
          loop $ return $ EventInfo name place date : events'
    ["Tell", "me", "about", name] -> do
      case findEventByName events name of
        Nothing -> do 
          putStrLn "I do not know of such event"
          loop ioEvents
        Just event -> do
          putStrLn $ show event
          loop ioEvents
    ["What", "happens", "on", dateStr] ->
      case parseDate dateStr of
        Nothing -> do 
          putStrLn "Bad date"
          loop ioEvents
        Just date ->
          let events' = getEventsOnDate events date
          in if null events'
               then do
                putStrLn "Nothing that I know of"
                loop ioEvents
               else do 
                mapM_ (putStrLn . show) ( sortBy (\(EventInfo n1 _ _) (EventInfo n2 _ _) -> compare n1 n2) events')
                loop ioEvents
    ["What", "happens", "at", place] ->
      let events' = getEventsAtPlace events place
      in if null events'
           then do 
            putStrLn "Nothing that I know of"
            loop ioEvents
           else do 
            -- putStrLn $ "ad"
            -- mapM_ (putStrLn . showEventInfo) events'
            mapM_ (putStrLn . showEventInfo) (sortBy (\(EventInfo n1 _ _) (EventInfo n2 _ _) -> compare n1 n2) events')
            loop ioEvents
    ["Quit"] -> putStrLn "bye"
    _ -> do
      putStrLn "I do not understand that. I understand the following:\n\
               \*Event <name> happens at <place> on <date>\n\
               \*Tell me about <eventname>\n\
               \*What happens on <date>\n\
               \*What happens at <place>\n\
               \*Quit"
      loop ioEvents

splitSentence :: String -> [String]
splitSentence = unfoldr getNext
  where
    getNext :: String -> Maybe (String, String)
    getNext [] = Nothing
    getNext ('\'':xs) = Just $ (trimQuotes y, trimQuotes z)
      where (y, '\'':w) = break (== '\'') xs
            z = dropWhile isSpace w
    getNext (x:xs)
      | isSpace x = getNext xs
      | otherwise = Just $ (trimQuotes (takeWhile (not . isSpace) (x:xs)), dropWhile (not . isSpace) xs)
    
    trimQuotes :: String -> String
    trimQuotes [] = []
    trimQuotes s@(x:xs)
      | x == '\'' && last xs == '\'' = init xs
      | otherwise = s

showEventInfo :: EventInfo -> String
showEventInfo (EventInfo name place date) = "Event " ++ name ++ " happens at " ++ place



