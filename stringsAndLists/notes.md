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

Using ++ involves walking through the whole list on the left hand side of ++. Avoid this with lists that are long.

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

The ``` last ``` function returns the last item in a list

```hs
last [1, 2, 3]
-- 3
```

The ``` init ``` function returns a list except the last item

```hs
init [1, 2, 3]
-- [1, 2]
```

Be careful not to use head, tail, last or init on empty lists.

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

The ``` length ``` function returns the length of a list

```hs
length [1, 2, 3]
-- 3
```

The ``` null ``` function returns True or False based on if a list is empty

```hs
null [1, 2, 3]
-- False
null []
-- True
```

The ``` reverse ``` function returns a reversed list

```hs
reverse [1, 2, 3]
-- [3, 2, 1]
```

The ``` maximum ``` function returns the biggest element in a list

```hs
maximum [1, 3, 2]
-- 3
```

The ``` minimum ``` function returns the smallest element in a list

```hs
minimum [2, 1, 3]
-- 1
```

The ``` sum ``` function returns the sum of a list of numbers

```hs
sum [1, 2, 3]
-- 6
```

The ``` product ``` function returns the product of a list of numbers

```hs
product [1, 2, 3]
-- 6
```

The ``` elem ``` function returns True or False depending on if an element exists in a list

```hs
elem 4 [1, 2, 3]
-- false
```

The ``` elem ``` function is often used as an infix function

```hs
2 `elem` [1, 2, 3]
-- True
```

###List ranges

Lists containing lists of numbers or characters can be defined as such:

```hs
[1..5]
-- [1, 2, 3, 4, 5]
['a'..'d']
-- "abcd"
```

Steps can be used in ranges

```hs
[2, 4..16]
-- [2, 4, 6, 8, 10, 12, 14, 16]
[3, 6..18]
-- [3, 6, 9, 12, 15, 18]
[2, 8..16]
-- [2, 8, 14]
[6, 5..1]
-- [6, 5, 4, 3, 2, 1]
```

Avoid using floating point numbers in ranges as the results may be unpredictable

```hs
[0.1, 0.3..1]
-- [0.1, 0.3, 0.5, 0.7, 0.8999999, 1.099999]
```

Ranges can produce inifinite lists. The ``` cycle ``` function takes a list and cycles it into an infinite list

```hs
take 10 (cycle [1, 2, 3])
-- [1, 2, 3, 1, 2, 3, 1, 2, 3, 1]
```
The ``` repeat ``` function takes an element and returns an infinite list of that item

```hs
take 5 (repeat 1)
-- [1, 1, 1, 1, 1]
```

The ``` replicate ``` function can be used to create a list of identitcal elements

```hs
replicate 3 5
-- [5, 5, 5]
```

###List comprehensions

In list comprehensions the output function goes before the ``` | ``` and the (optional) predicate follows a comma to filter the results

```hs
[x*2 | x <- [1..10]]
-- [2, 4, 6, 8, 10, 12, 14, 16, 18, 20]
[x*2 | x <- [1..10], x*2 `mod` 3 == 0]
-- [6, 12, 18]
```

Multiple predicates can be used separating them with commas

```hs
[x*2 | x <- [1..10], x*2 /= 10, x*2 /= 16]
-- [2, 4, 6, 8, 12, 14, 18, 20]
```

List comprehensions can take multiple input lists separated by commas

```hs
[x*y | x <- [2, 3, 10], y <- [1..3]]
-- [2, 4, 6, 3, 6, 9, 10, 20, 30]
[x*y | x <- [2, 3, 10], y <- [1..3], x*y `mod` 3 == 0]
-- [6, 3, 6, 9, 30]
```

```hs
removeUpperCase st = [c | c <- st, c `elem` ['a'..'z']]
removeUpperCase "HELlo WOrLd"
-- "lord"
```

