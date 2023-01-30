{-
Make an interactive phone book application that only allows for insertions and searching the values. 
The commands that your application has to understand are (with arbitrary number of spaces between the parts):

add name phone_type country_code phone_no
find name

If the command is not find name or add name, output "Cannot do that".  
For the find name command, output a list of phone numbers for that name in the format used in Task 2.4.

When the program starts, print "Welcome to phone book application". After each addition, print "Done". 
When the user gives the command "quit", respond with "bye" and end the program.

You can do the implementation of the phone book however you like.  You may also use the modules developed in the previous tasks. 
The main function needs to be in a file named phone_book_app.hs and you the other modules, if such are used, cannot be such that they would need to be in a sub-directory. The program must be possible to compile and execute with all files in the same directory with phone_book_app

In this task, you may assume that the values added to the phone book are correct. 
The only country codes to be used are 44 and 358 (possibly with leading 00 or leading +)


Below you can see an example execution:

```
Welcome to phone book application
add Bob Other +358 123545
Done
do not add anything
Cannot do that
find Bob
[+358 123545 (Other)]
quit
bye
```
-}

import Phone_book_tree
import Phone_type2

main :: IO ()
main = do
  putStrLn "Welcome to phone book application"
  phonebookApp Empty

phonebookApp :: Phonebook -> IO ()
phonebookApp pb = do
  line <- getLine
  if line == "quit"
    then putStrLn "bye"
    else let cmd = words line in
      case cmd of
        ["add", name, phoneType, countryCode, phoneNo] -> do
          let pb' = addEntry name phoneType countryCode phoneNo pb
          putStrLn "Done"
          phonebookApp pb'
        ["find", name] -> do
          let entries = findEntries name pb
          putStrLn $ show entries
          phonebookApp pb
        _ -> do
          putStrLn "Cannot do that"
          phonebookApp pb