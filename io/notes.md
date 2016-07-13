##Compiling haskell programs

Create a file

*helloworld.hs*
```hs
main = putStrLn "hello world"
```

Compile it

```sh
ghc --make helloworld
```

Run it

```sh
./helloworld
-- hello world
```

##I/O actions

I/O actions are only performed if they are given a name of ``` main ``` or inside a bigger I/O action that is composed with a ``` do ``` block.
``` do ``` blocks can glue together multiple I/O actions but will eventually have to fall into a ``` main ``` to be performed.

I/O actions encapsulate results once performed. To bind the result to a name the ``` <- ``` keyword must be used. The last action in a do block cannot be bound to a name. The ``` do ``` block extracts the value of the last action and binds it to it's own result.

```hs
main = do
    foo <- putStrLn "What's your name?"
    name <- getLine
    putStrLn ("howdy " ++ name ++ "!")
```

The result of the do block and the value of ``` foo ``` would be ``` () ``` - a dummy value.

The ``` let ``` keyword doesn't need the ``` in ``` keyword inside a ``` do ``` block

```hs
import Data.Char
main = do
    putStrLn "What's your first name?"
    firstName <- getLine
    putStrLn "What's your secone name?"
    secondName <- getLine
    let upperFirstName = map toUpper firstname
        upperSecondName = map toUpper secondName
    putStrLn $ "howdy " ++ upperFirstName ++ " " ++ upperSecondName ++ "!"
```

The ``` return ``` makes an I/O action out of a pure value. It does not end the execution of a function.
When dealing with ``` do ``` blocks we use ``` return ``` to create an action that doesn't do anything (``` return () ```) or because we want the ``` do ``` block to have a result other than it's last action.

###Useful I/O functions

 - ``` putStr ``` returns an I/O action that will print a string to the terminal. Unlike ``` putStrLn ``` it doesn't jump to a new line after
 - ``` putChar ``` returns an I/O action that will print a character to the terminal
 - ``` print ``` returns an I/O action that will print any type that's an instance of ``` Show ``` to the terminal
 - ``` getChar ``` is an I/O action that reads a character from the input
 - ``` when ``` is a function in the ``` Control.Monad ``` module that takes a boolean value and an I/O action and if the boolean value is ``` True ``` it returns the same I/O action that it was supplied. If it received ``` False ``` it returns the ``` () ``` action.

```hs
import Control.Monad
main = do
    c <- getChar
    when (c /= ' ') $ do
        putChar c
        main
```

 - ``` sequence ``` takes a list of I/O actions and returns an I/O action that will perform those actions one after another. The result contained in the I/O action will be a list of results of all the I/O actions that were performed
 - ``` forever ``` takes an I/O action and returns an I/O action that repeats

The ``` getContents ``` function is an I/O action that reads everything from the standard input until it encounters an end-of-file character.

```hs
import Data.Char
main = do
    contents <- getContents
    putStr (map toUpper contents)
```

The ``` interact ``` function takes a function of type ``` String -> String ``` as a parameter and returns an I/O action that will take some input, run that function on it and print out the functions results.

```hs
main = interact $ map toUpper
```

##Files and streams

```hs
import System.IO
main = do
    handle <- openFile "filename.txt" ReadMode
    contents <- hGetContents handle
    putStr contents
    hClose handle
```

IO modes are
 - ReadMode
 - WriteMode
 - AppendMode
 - ReadWriteMode

```hs
import System.IO
main = do
    withFile "filename.txt" ReadMode (\handle -> do
        contents <- hGetContents handle
        putStr contents)
```

###File I/O functions

 - ``` hGetLine ```
 - ``` hPutStr ```
 - ``` hPutStrLn  ```
 - ``` hGetChar ```

The ``` readFile ``` function returns an I/O action that will read a file and bind it's contents to something as a string.

```hs
import System.IO
main = do
    contents <- readFile "filename.txt"
    putStr contents
```

The ``` writeFile ``` function takes a path to a file and a string and returns an I/O action that does the writing.

```hs
import System.IO
main = do
    contents <- readFile "filename.txt"
    writeFile "newFile.txt" (map toUpper contents)
```

The ``` appendFile ``` function works in a similar way to ``` writeFile ``` but appends stuff to the supplied file instead of (potentially) replacing an existing file.

##Command line arguments

```hs
import System.Environment
import Data.List

main = do
    args <- getArgs
    progName <- getProgName
    putStrLn "The arguments are:"
    mapM putStrLn getArgs
    putStrLn "The program name is:"
    putStrLn progName
```


