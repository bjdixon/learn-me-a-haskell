##Strings

Strings are lists of characters

```hs
let x = 'c'
let y = "c"
```

x is of type Char where as y is of type list of Chars

###Types

```hs
main :: IO ()
str :: String
i :: Integer
c :: Char
```

- main has type IO. 
- str has type String
- i has type Integer

###Printing

```hs
print "a string"
putStrLn "another string"
putStr "a third string"
```

###Concatenation

++ operator or concat function

```hs
"joined" ++ " strings"
concat ["another", " joined ", "string"]
```

- The infix operator ++ has the type [a] -> [a] -> [a]
  eg. (++) :: [a] -> [a] -> [a]
- The prefix function concat has the type [[a]] -> [a]
  eg. concat :: [[a]] -> [a]

Which means ++ and concat can be used to join arrays of any (non mixed) type arrays

```hs
[1, 2, 3] ++ [4, 5, 6]
-- [1,2,3,4,5,6]

concat [[1, 2, 3], [4, 5, 6], [7, 8, 8]]
-- [1,2,3,4,5,6,7,8,9]
```

###List functions

The cons operator ``` : ``` inserts an item into the beginning of a list

```hs
1 : [2, 3, 4]
-- [1,2,3,4]

'h' : "ello"
-- "hello"
```

The function ``` head ``` returns the first item in a list

```hs
head [1, 2, 3]
-- 1
```

The function ``` tail ``` returns all items in a list except the first item

```hs
tail [1, 2, 3]
-- [2,3]
```

The ``` take ``` function returns the number of items specified from the list starting from the left

```hs
take 3 [1, 2, 3, 4, 5]
-- [1,2,3]
```

The ``` drop ``` function returns all items from the list after passing over the specified number of items starting from the left

```hs
drop 3 [1, 2, 3, 4, 5]
-- [4,5]
```

The ``` !! ``` operator returns the item specified by the index from the list

```hs
[1, 2, 3, 4, 5] !! 3
-- 4
```

