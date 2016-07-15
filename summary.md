##Basic syntax

###REPL

**:load FILENAME** loads the code from the source file
**:module** unloads the file 

###Expressions

**normal form** is an expression that can no longer be reduced.
eg. 1 + 1 can be reduced to it's normal form of 2.

**redexes** are reducible expressions

**normalizing** is the process of recucing (or executing) expressions

###Functions

a function is an expression applied to an argument

all functions take one argument and return one result

eg. function definition syntax:
```hs
triple x = x * 3
```

eg. invoking a function syntax
```hs
triple 3

triple (triple 3)

triple (triple (3 + 1))
```

###Variables

type variables (variables in type signatures) start at a and follow from there (b, c)

functions are labeled from f and follow from there (g, h).

closely related or helper functions may be decorated f' (f prime) or f1

arguments to functions start at x

###let and where expressions

**let**

```hs
let x = 5 in x
-- 5
x
-- Not in scope: 'x'

let x = 5 in x * x
-- 25

let x = 5; y = 6 in x * y
-- 30

let x = 3; y = 1000 in x + 3
-- 6

let x = 3
-- 3
x
-- 3
```

**where**

```hs
-- moduleName.hs
times = x * y
    where x = 5
          y = 6

-- eof

times
-- 30
```

Using let and where in modules binds variables locally. Omitting let/where binds the variables to global/top level scope

###Lambdas

```hs
(\x -> x * 2) 2
-- 4

let square = \x -> x * x
square 3
-- 9
```

let is syntactic sugar for lambdas

```hs
let id x = x
-- same as
let id \x -> x

id "hello"
-- "hello"
```

let expressions in lambda form

```hs
let a = b in c
--same as
(\a -> c) b

let x = 5 in x + 10
-- same as
(\x -> x + 10) 5
```

where expressions in lambda form

```hs
c where a = b
-- same as
(\a -> b) c

x + 10 where x = 5
-- same as
(\x -> x + 10) 5
```

###Infix operators/functions

|Operator | Name      | Purpose/application                  |
|:---:    | :---      | :---                                 |
|+        | plus      | addition                             |
|-        | minus     | substraction                         |
|\*       | asterisk  | multiplication                       |
|/        | slash     | fractional division                  |
|^        | caret     | exponentiation                       |
|$        | dollar    | lowest possible precedence           |
|div      | divide    | integral division, round down        |
|mod      | modulo    | remainder after division             |
|quot     | quotient  | integral division, round towards zero|
|rem      | remainder | remainder after division             |

```hs
9 / 5
1.8
```

**Some infix functions have prefix function equivelents**

```hs
div 9 5
1
```

```hs
mod 9 5
4
```

**$ a convenience operator with the lowest possible precedence**

Used to remove the need for some parentheses. Always evaluated last.

```hs
(2^) $ 2 + 2
-- 16

(2^) (2 + 2)
-- 16

(2^) 2 + 2
-- 6
```

**Quotients and remainders**

```hs
(quot x y)*y + (rem x y) == x
(rem x y)*y + (mod x y) == x
```

##Strings

Strings are lists of characters

```hs
let x = 'c'
let y = "c"
```

x is of type Char where as y is of type list of Chars

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

##Tuples

Tuples store collections of values ``` (1, 2) ``` and are used when you know how many values a collection will store. It's type depends on how many elements it stores (pair, triple, 4-tuple) and the types of those elements.

Tuples can be compared to each other like lists

A list containing 2 pair tuples

```hs
[(1, 2), (3, 4)]
```

A tuple can contain values of different types

```hs
("hello", 9)
```

A list cannot contain tuples of different types

```hs
[(1, 2), (3, 4, 5), (6, 7)]
-- error: tuples of different length are different types
[(1, 2), (3, "four)]
-- error: tuples with elements of different types are different types
```

###Tuple functions

The ``` fst ``` function returns the first element in a pair tuple. Can only be used on a pair

```hs
fst (1, 2)
-- 1
```

The ``` lst ``` function returns the last element in a pair tuple. Can only be used on a pair

```hs
lst (1, 2)
--- 2
```

The ``` zip ``` function takes 2 lists and zips them together into 1 list of pair tuples

```hs
zip [1, 2, 3, 4, 5] ["one", "two", "three", "four", "five"]
-- [(1, "one"), (2, "two"), (3, "three"), (4, "four"), (5, "five")]
```

If one list is longer than the other the longer list gets cut off to match the length of the shorter list

Tuples can be used in list comprehensions

```hs
[(a, b, c) | a <- [1..10], b <- [1..10], c <- [1..10], a^2 + b^2 == c^2, a+b+c == 24]
-- [(6, 8, 10)]
```
(list of tuples containing right angled triangles with a perimeter of 24)

##Types

Haskell has static (types known at compile time) and strong (no type coercion) typing. Types are inferred and don't have to be explicitly declared.

Functions and variables can be declared with explicit types as such:

```hs
main :: IO ()
str :: String
i :: Integer
c :: Char
```

- main has type IO. 
- str has type String
- i has type Integer

Applying ``` :t ``` in the interpreter to an expression yields it's type

```hs
:t 'a'
-- 'a' :: Char
:t "string"
-- "string" :: [Char]
:t True
-- True :: Bool
:t 5
-- 5 :: Num
:t 5.5
-- 5.5 :: Fractional
:t (True, 'a')
-- (True, 'a') :: (Bool, Char)
```

Each tuple length has it's own type.

```hs
let x = 'c'
let y = "c"
```

x is of type Char where as y is of type list of Chars

###Common types

 - Bounded integer ``` Int ```
 - Unbounded integer ``` Integer ```
 - Floating point (single precision) ``` Float ```
 - Floating point (double precision) ``` Double ```
 - Boolean ``` Bool ```
 - Char ``` Char ```

###Functions

It is considered good practice to explicitly declare function types.

```hs
removeUpperCase :: [Char] -> [Char]
removeUpperCase st = [c | c <- str, c `elem` ['a'..'z']]
```

Parameters and return types are separated with ``` -> ```. There is no distinction made between parameters and return type.
A function that takes 3 integers and returns an integer would be declared ``` Int -> Int -> Int -> Int ```

###Type variables

Functions that can be applied to variables of different types are called polymorphic functions. Their types are expressed using variables.

```hs
[a] -> a
-- takes a list of any type and returns a value of that same type
(a, b) -> a
-- takes a pair tuple and returns a value that is of the same type as the first element
```

###Typeclasses

If a type is part of a typeclass it implements the behavior a typeclass describes.

**Basic typeclasses**

 - ``` Eq ``` used for testing equality. Members implement ``` == ``` and ``` /= ``` functions. ``` :t (==) ```
 - ``` Ord ``` used for types that have an ordering. Members implement ``` > ```, ``` < ```, ``` >= ``` and ``` <= ```. ``` :t (>) ```
 - ``` Show ``` Members implement ``` show ```

##Pattern matching

Pattern matching specifies patterns that data should conform to, checking that it does and then deconstructing that data.

 - Separate function bodies can be defined for different patterns.
 - Any data type can be pattern matched
 - The separate function bodies are checked from the first to the last using the first encountered that conforms to the pattern
 - A catch all pattern should be included to prevent crashes from unexpected input
 - The ``` x:xs ``` pattern is often used in pattern matching against lists

```hs
dayOfWeek :: (Integral a) => a -> String
dayOfWeek 1 = "Monday"
dayOfWeek 2 = "Tuesday"
dayOfWeek 3 = "Wednesday"
dayOfWeek 4 = "Thursday"
dayOfWeek 5 = "Friday"
dayOfWeek 6 = "Saturday"
dayOfWeek 7 = "Sunday"
dayOfWeek x = "Not a day"
```

```hs
factorial :: (Integral a) => a -> a
factorial 0 = 1
factorial n = n * factorial (n - 1)
```

```hs
length' :: (Num b) => [a] -> b
length' [] = 0
length' (_:xs) = 1 + length' xs
```

```hs
sum' :: (Num a) => [a] -> a
sum' [] = 0
sum' (x:xs) = x + sum' xs
```

###As patterns
As patterns are used to break up data according to a pattern. They are bound to a name using the ``` @ ``` symbol behind the name and in front of the pattern

```hs
first :: String -> String
first all@(x:xs) = [x] ++ " is the first letter of " ++ all
```

##Guards

 - Guards test whether a property or value are true or false
 - They are written as pipes following the functions name and parameters
 - They are usually indented and lined up
 - The function body that matches the first guard as true is executed
 - ``` otherwise ``` always evaluates as true

```hs
dayOfWeek :: (Num a) => a -> String
dayOfWeek n
    | n < 0 = "Try a bigger number"
    | n == 1 = "Monday"
    | n == 2 = "Tuesday"
    | n == 3 = "Wednesday"
    | n == 4 = "Thursday"
    | n == 5 = "Friday"
    | n == 6 = "Saturday"
    | n == 7 = "Sunday"
    | otherwise = "Try a smaller number"
```

##Where

The ``` where ``` keyword binds variables and functions to a name at the end of a function whilst still being visible to the whole function.
 - The bindings are not visible outside the function
 - the ``` where ``` is usually indented along with the pipes
 - the names we bind to are usually aligned with each other

```hs
bmi :: (RealFloat a) => a -> a -> String
bmi weight height
    | bmi <= skinny = "underweight"
    | bmi <= normal = "normal"
    | bmi <= fat = "fat"
    | otherwise = "very fat"
    where bmi = weight / height ^ 2
          skinny = 18.5
          normal = 25.0
          fat = 30.0
```

Pattern matching can be used within ``` where ``` bindings

```hs
bmi :: (RealFloat a) => a -> a -> String
bmi weight height
    | bmi <= skinny = "underweight"
    | bmi <= normal = "normal"
    | bmi <= fat = "fat"
    | otherwise = "very fat"
    where bmi = weight / height ^ 2
          (skinny, normal, fat) = (18.5, 25.0 30.0)
```

##Let

The ``` let ``` keyword binds expressions anywhere but are local and don't span across guards.
The form ``` let <bindings> in <expression> ``` is used to scope the ``` <bindings> ``` to the ``` <expression> ```.

```hs
surfaceAreaOfCylinder :: (RealFloat a) => a -> a -> a
surfaceAreaOfCylinder r h =
    let sideArea = 2 * pi * r * h
        topArea = pi * r ^ 2
    in  sideArea + 2 * topArea
```

##Case expressions

Case expressions can be evaluate based on possible cases of the value of a variable and pattern matching.
Case expressions can be done anywhere, not just pattern matching on function parameters.

These functions are interchangable

```hs
head' :: [a] -> a
head' [] = error "empty list"
head' (x:_) = x
```

```hs
head' :: [a] -> a
head' xs = case xs of [] -> error "empty list"
                      (x:_) -> x
```

The syntax is

```
case expression of pattern -> result
                   pattern -> result
                   pattern -> result
```

##Recursion

**Method for designing recursive functions**
 - Define an edge case (usually where a recursive function doesn't make sense, eg. empty list when operating on lists or a node without children on a tree)
 - Define a function that does something between some element and the function that is applied to the rest
 - Think about identites when applying edge cases (eg. 1 for multiplication, 0 for addition, empty list for lists)

###Implementing standard library functions using recursion

Consider edge conditions that allow a recursion function to terminate

**Recursing over a list**

```hs
maximum' :: (Ord a) => [a] -> a
maximum' [] = error "empty list"
maximum' [x] = x
maximum' (x:xs)
    | x > maxTail = x
    | otherwise = maxTail
    where maxTail = maximum' xs
```

```hs
maximum' :: (Ord a) => [a] -> a
maximum' [] = error "empty list"
maximum' [x] = x
maximum' (x:xs) = max x (maximum' xs)
```

**Creating lists**

```hs
replicate' :: (Num i, Ord i) => i -> a -> [a]
replicate' n x
    | n <= 0 = []
    | otherwise = x:replicate' (n-1) x
```

```hs
take' :: (Num i, Ord i) => i -> [a] -> [a]
take' n _
    | n <= 0 = []
take' _ [] = []
take' n (x:xs) = x : take' (n-1) xs
```

```hs
zip' :: [a] -> [b] -> [(a, b)]
zip' _ [] = []
zip' [] _ = []
zip' (x:xs) (y:ys) = (x, y):zip' xs ys
```

###Quick sort

```hs
quicksort :: (Ord a) => [a] -> [a]
quicksort [] = []
quicksort (x:xs) =
    let smallerSorted = quicksort [a | a <- xs, a <= x]
        biggerSorted = quicksort [a | a <- xs, a > x]
    in smallerSorted ++ [x] ++ biggerSorted
```

##Curried functions

Functions in haskell are applied to 1 argument. Functions that appear to be invoked using multiple arguments have been curried.

```hs
max 4 5
-- 5
(max 4) 5
-- 5
```

In the example above ``` (max 4) ``` returns a partially applied function that later accepts the final argument.

```hs
takeFour = take 4
takeFour [1, 2, 3, 4, 5, 6, 7, 8]
-- [1, 2, 3, 4]
```

Infix functions and operators can be partially applied using parentheses

```hs
addTen = (+10)
addTen 2
-- 12
```

As negation is written ``` (-4) ``` to write a partially applied subtraction function you have to use the ``` subtract ``` function. eg. ``` subtract 4 ```

##Type declarations

When expecting a function as a parameter that function is indicated using parentheses in the type declaration.

```hs
applyTwice :: (a -> a) -> a -> a
applyTwice f x = f (f x)
```

In the example above the first parameter is a function that takes an element and returns an element of the same type ``` (a -> a) ``` it then takes another element of the same type and returns an element of the same type.

```hs
applyTwice (+3) 10
-- 16
```

In the example above ``` (+3) ``` takes a number and returns a number ``` (a -> a) ``` and is then supplied a number (10) and returns a number (10)

```hs
zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith' _ [] _ = []
zipWith' _ _ [] = []
zipWith' f (x:xs) (y:ys) = f x y : zipWith f xs ys

zipWith' (+) [1, 2, 3] [1, 2, 3]
-- [2, 4, 6]
zipWith' max [1, 2, 3] [1, 1, 4]
-- [1, 2, 4]
```

##Maps and filters

**Map**

The type declaration for ``` map ``` is ``` (a -> b) -> [a] -> [b] ``` 

```hs
map (*2) [1, 2, 3]
-- [2, 4, 6]
```

The ``` map ``` function is sometimes more readable than using a list comprehension

```hs
[a*2 | a <- [1, 2, 3]]
-- [2, 4, 6]
```

There is no fixed rule when to use which, readability depends on context

**Filter**

The type declaration for ``` filter ``` is ``` (a -> Bool) -> [a] -> [a] ```

```hs
filter (>3) [2, 3, 4]
-- [4]
```

This could also be replaced using a list comprehension with predicates. Readability depends on context

**Implementing quicksort with filter**

```hs
quicksort :: (Ord a) => [a] -> [a]
quicksort [] = []
quicksort (x:xs) =
    let smallerSorted = quicksort (filter (<=x) xs)
        biggerSorted = quicksort (filter (>x) xs)
    in smallerSorted ++ [x] ++ biggerSorted
```

The ``` takeWhile ``` function takes a predicate and a list returning elements of the list that hold true to the predicate until an element is found that doesn't hold.

```hs
takeWhile (>3) [6, 5, 4, 3, 2, 1]
-- [6, 5, 4]
```

The sum of all odd squares that are less than 10,000

```hs
sum (takeWhile (<10000) (filter odd (map (^2) [1..])))
-- 166650
```

List of partially applied functions

```hs
listOfFuns = map (*) [0..]
(listOfFuns !! 4) 5
-- 20
```

##Lambdas

**Syntax**

```hs
(\x -> x * 2) 2
-- 4

let square = \x -> x * x
square 3
-- 9
```

 - Lambdas are anonymous functions that are only needed once
 - They are often used to be passed to a higher order function
 - They are usually surrounded by parentheses unless expected to extend all the way to the right

```hs
filter (\xs -> length xs > 2) [[1, 2, 3], [1, 2], [1, 2, 3, 4]]
-- [[1, 2, 3], [1, 2, 3, 4]]
```

Don't use lambdas when partial application will do

```hs
map (+3) [1, 2, 3]
-- [4, 5, 6]
map (\x -> x + 3) [1, 2, 3]
-- [4, 5, 6]
```

##Folds

Folds are like ``` map ``` but reduce a list to a single value

Folds take a function, an accumulator (starting value) and a list. The function is applied to 2 arguments (the accumulator and the first or last element) producing a new accumulator. The function is then applied again to the new accumulator and the next first or last element.

The left fold function ``` foldl ``` works from the first element to the last and the right fold function ``` foldr ``` works from the last element to the first.

```hs
foldl (\acc x -> acc + x) 0 [1, 2, 3, 4, 5]
-- 15
```

```hs
foldr (\x acc -> x + acc) 0 [1, 2, 3]
-- 6
```

```hs
sum' = foldl (+) 0
sum' [1, 2, 3, 4, 5]
-- 15
```

Generally if you have a function like ``` foo a = bar b a ``` you can rewrite it as ``` foo = bar b ``` because of currying

Right folds can be used on unbounded lists whereas left folds cannot

The ``` foldl1 ``` and ``` foldr1 ``` functions work the same as ``` foldl ``` and ``` foldr ``` except they assume the first or last element to be the accumulator and start with the next element

The functions ``` scanl ```, ``` scanr ```, ```scanl1 ``` and ``` scanr1 ``` are like folds except they return the intermediate accumulator states in a list

```hs
scanl1 (+) [1, 2, 3, 4, 5]
-- [3, 6, 10, 15]
```

##Function application with $

**$ a convenience operator with the lowest possible precedence**

Used to remove the need for some parentheses. Always evaluated last.

```hs
(2^) $ 2 + 2
-- 16

(2^) (2 + 2)
-- 16

(2^) 2 + 2
-- 6
```

Function application has a very high precendence but apart from removing parentheses ``` $ ``` allows function application to be treated like another function

```hs
map ($ 3) [(4+), (10*), succ]
-- [7, 30, 4]
```

##Composition

Function composition is done using the ``` . ``` function

```hs
map (negate . abs) [1, -2, 3, -4]
-- [-1, -2, -3, -4]
```

```hs
fn x = ceiling (negate (tan (cos (max 50 x))))
```

Can be written as

```hs
fn = ceiling . negate . tan . cos . max 50
```

##Loading modules

 - Modules must be imported before defining any functions
 - Modules are imported using the following syntax ``` import <module name> ```
 - When importing a module all the functions that module exports are available in the global namespace
 - Modules can imported into ghci using the ``` :m ``` keyword. ``` :m + Data.list Data.Map Data.Set ```
 - Functions can be imported without the rest of the module ``` import Data.List (nub, sort) ```
 - Functions can be excluded whilst the rest of the module is imported ``` import Data.List hiding (nub) ```

###Qualified names

Qualified imports can be used to avoid name clashes.

```hs
import qualified Data.Map
Data.Map.filter (>3) [1, 2, 4]
```

Qualified names can be mapped to something shorter

```hs
import qualified Data.Map as Map
Map.filter (>3) [1, 2, 4]
```

###Data.List

The ``` intersperse ``` function takes an element and a list and returns a list with the supplied element inserted between each element of the supplied list

```hs
intersperse 0 [1, 2, 3]
-- [1, 0, 2, 0, 3]
```

The ``` intercalculate ``` function takes a list of lists and a list and returns a flattened list of the supplied lists with the supplied list interspersed

```hs
intercalculate [0, 0, 0] [[1, 2, 3], [4, 5, 6], [7, 8, 9]]
-- [1, 2, 3, 0, 0, 0, 4, 5, 6, 0, 0, 0, 7, 8, 9]
```

The ``` transpose ``` function transposes a list of lists making the columns become rows and vice versa

```hs
transpose [[1, 2, 3], [4, 5, 6], [7, 8, 9]]
-- [[1, 4, 7], [2, 5, 8], [3, 6, 9]]
```

The ``` concat ``` function flattens a list of lists by one level of nesting

```hs
concat [[1, 2, 3], [4, 5, 6]]
-- [1, 2, 3, 4, 5, 6]
```

The ``` concatMap ``` function maps a function to a list returning a concatenated list

```hs
concatMap (replicate 4) [1..3]
-- [1, 1, 1, 2, 2, 2, 3, 3, 3]
```

The ``` and ``` function takes a list of boolean values and returns true if all are true

```hs
and $ map (>4) [5, 6, 7, 8]
-- True
```

The ``` or ``` function returns true if any items in the list are true

```hs
or $ map (==1) [1, 2, 3]
-- True
```

The ``` any ``` and ``` all ``` functions take a predicate and a list and return a boolean if any or all of the elements in the list satisfy the predicate

```hs
any (==4) [1, 2, 3, 4]
-- True
all (==4) [1, 2, 3, 4]
-- False
```

The ``` iterate ``` function takes a function and an initial value applying the function to the starting value and then continuing to apply the function to successive return values of the function. It returns an unbounded list

```hs
take 10 $ iterate (*2) 1
-- [1, 2, 4, 8, 16, 32, 64, 128, 256, 512]
```

The ``` splitAt ``` function takes a number and a list returning a tuple of 2 lists made up from splitting the supplied list at the element specified

```hs
splitAt 2 [0, 1, 2, 3, 4]
-- ([0, 1], [2, 3, 4])
```

The ``` takeWhile ``` function takes elements from a list for as long as the predicate holds

```hs
takeWhile (>3) [6, 5, 4, 3, 2, 1]
-- [6, 5, 4]
```

The ``` dropWhile ``` function drops elements from a list whilst the predicate holds returning the remainder of the list once the predicate fails

```hs
dropWhile (>3) [6, 5, 4, 3, 2, 1, 9]
-- [3, 2, 1, 9]
```

The ``` span ``` function takes elements from a list as long until the predicate fails. A two list tuple is returned with the first list containing the elements that would have been returned calling ``` takeWhile ``` and the second list containing the remainder of elements

```hs
span (>3) [5, 4, 3, 2, 1, 9, 8]
-- ([5, 4], [3, 2, 1, 9, 8])
```

The ``` break ``` function works in a similar fashion to ``` span ``` but splits the list when the predicate holds true for the first time

```hs
break (<3) [5, 4, 3, 2, 1, 9, 8]
-- ([5, 4, 3], [2, 1, 9, 8])
```

The ``` sort ``` function returns a sorted list

```hs
sort [3, 2, 4, 5, 1]
-- [1, 2, 3, 4, 5]
```

The ``` group ``` function takes a list and returns lists of grouped elements

```hs
group [1, 2, 3, 4, 3, 2, 3, 1]
-- [[1, 1], [2, 2], [3, 3, 3], [4]]
```

The functions ``` inits ``` and ``` tails ``` are like ``` init ``` and ``` tail ``` except they recursively apply that to a list

```hs
inits "hello"
-- ["", "h", "he", "hel", "hell", "hello"]
tails "hello"
-- ["hello", "hell", "hel", "he", "h", ""]
```

Searching for a sublist within a list

```hs
search :: (Eq a) => [a] -> [a] -> Bool
search needle haystack =
    let nlen = length needle
    in foldl (\acc x -> if take nlen x == needle then True else acc) False (tails haystack)
```

The ``` isInfixOf ``` function behaves like the search function defined above

```hs
isInfixOf [3, 4, 5] [1, 2, 3, 4, 5, 6]
-- True
```

The ``` isPrefixOf ``` and ``` isSuffixOf ``` functions search for a sublist at the beginning and at the end of a list

```hs
isInfixOf "hello" "hello world"
-- True
isSuffixOf "world" "hello world"
-- True
```

The ``` elem ``` and ``` notElem ``` functions check if an element exists within a list

The ``` partition ``` function takes a list and a predicate and returns a pair of lists where the first list contains all elements that hold for the predicate and the second list contains the elements that do not satisfy the predicate

```hs
partition (`elem` ['a'..'z']) "Hello World"
-- ("elloorld", "H W")
```

The ``` find ``` function takes a list and a predicate returning the first element that satisfies the predicate wrapped in a Maybe value

```hs
find (>4) [1, 2, 3, 4, 5, 6, 7]
-- Just 5
find (>7) [1, 2, 3, 4, 5, 6, 7]
-- Nothing
```

The ``` elemIndex ``` function is like ``` elem ``` but Maybe returns the index of the element if found or ``` Nothing ``` if not

```hs
elemIndex 4 `elemIndex` [1, 2, 3, 4, 5]
-- Just 3
```

The ``` elemIndices ``` works like ``` elemIndex ``` except it returns a list of indices and does not use a Maybe

```hs
' ' `elemIndices` "Good morning world"
-- [4, 12]
```

The functions ``` zip3 ```, ``` zip4 ```, ``` zipWith3 ```, ``` zipWith4 ``` up to ``` zip7 ``` and ``` zipWith7 ``` work the same way as ``` zip ``` and ``` zipWith ``` but take 3, 4, up to 7 list

```hs
zip3 [1, 1, 1] [2, 2, 2] [3, 3, 3]
-- [(1, 2, 3), (1, 2, 3), (1, 2, 3)]
```

The ``` lines ``` function takes a string and returns each line of the string in a separate list

```hs
lines "this is a string\ncontaining multiple lines\nend"
-- ["this is a string", "containing multiple lines", "end"]
```

The ``` unlines ``` function is the inverse of ``` lines ```, joining them together using ``` \n ```

```hs
unlines ["this is a string", "containing multiple lines", "end"]
-- "this is a string\ncontaining multiple lines\nend\n"
```

The ``` words ``` and ``` unwords ``` function split text into words or join lists of words into text

```hs
words "this is a string"
-- ["this", "is", "a", "string"]
unwords ["this", "is", "a", "string"]
-- "this is a string"
```

The ``` nub ``` function removes duplicate elements from a list

```hs
nub [1, 2, 1, 4, 3, 2, 4, 3, 3, 3]
-- [1, 2, 3, 4]
```

The ``` delete ``` function deletes the first instance of the supplied element from a list

```hs
delete ' ' "hello world"
-- "helloworld"
```

The ``` \ ``` function takes 2 lists and returns a list of elements in the first list that don't appear in the second list

```hs
[1, 2, 3, 4] \\ [1, 3, 5]
-- [2, 4]
```

The ``` union ``` function returns the union of 2 lists appending every element in the second list that doesn't appear in the first

```hs
[1, 2, 3, 4, 5] `union` [4, 5, 6, 7]
-- [1, 2, 3, 4, 5, 6, 7]
```

The ``` intersect ``` function returns only elements found in both supplied lists

```hs
[1, 2, 3, 4, 5] `intersect` [4, 5, 6, 7]
-- [4, 5]
```

The ``` insert ``` funtion takes an element and a list that can be sorted and inserts the element at the appropriate point

```hs
insert 4 [1, 2, 3, 5, 6]
-- [1, 2, 3, 4, 5, 6]
```

The ``` length ```, ``` take ```, ``` drop ```, ``` splitAt ```, ``` !! ``` and ``` replicate ``` functions all take an Int as one of their params. Data.List supplies generic versions that will take Integral or Num typeclasses. They are named ``` genericLength ```, ``` genericTake ```, ``` genericDrop ```, ``` genericSplitAt ```, ``` genericIndex ``` and ``` genericReplicate ```.

The ``` nub ```, ``` delete ```, ``` union ```, ``` intersect ``` and ``` group ``` functions have counterparts that take an equality function. The counterparts are named ``` nubBy ```, ``` deleteBy ```, ``` unionBy ```, ``` intersectBy ``` and ``` groupBy ```. ``` groupBy (==) ``` is the same as ``` group ```.

The ``` sort ```, ``` maximum ```, ``` insert ``` and ``` minimum ``` functions also have By counterparts, ``` sortBy ```, ``` insertBy ```, ``` maximumBy ``` and ``` minimumBy ```. The Data.Function ``` on ``` function is useful when using By functions

```hs
import Data.Function (on)
sortBy (compare `on` length) [[1, 2], [1, 2, 3], [2, 2], [2, 2, 3], [3]]
-- [[3], [1, 2], [2, 2], [1, 2, 3], [2, 2, 3]]
```

When using By functions that take an equality function you usually do ``` (==) `on` something ``` and when using By functions that take an ordering function you usually do ``` compare `on` something ```.


###Data.Char

**Predicate functions exported by Data.Char**

All these functions take a Char and return a Bool

 - ``` isControl ``` is the character a control character
 - ``` isSpace ``` checks against whitespace characters (space, tab, newline)
 - ``` isLower ```
 - ``` isUpper ```
 - ``` isAlpha ```
 - ``` isAlphaNum ```
 - ``` isPrint ``` is the character printable (control characters are not)
 - ``` isDigit ```
 - ``` isOctDigit ```
 - ``` isHexDigit ```
 - ``` isLetter ```
 - ``` isMark ``` checks against Unicode mark characters
 - ``` isNumber ```
 - ``` isPunctuation ```
 - ``` isSymbol ``` like math or currency
 - ``` isSeparator ``` Unicode space separators
 - ``` isAscii ```
 - ``` isLatin1 ```
 - ``` isAsciiUpper ```
 - ``` isAsciiLower ```

```hs
all isAlphaNum "howdy123"
-- True
```

**Conversion functions exported by Data.Char**

 - ``` toUpper ```
 - ``` toLower ```
 - ``` toTitle ``` usually the same as ``` toUpper ```
 - ``` digitToInt ``` character must be in the ranges of '0'..'9', 'a'..'f', or 'A'..'F'
 - ``` intToDigit ```
 - ``` ord ``` converts characters to their corresponding numbers (a == 97)
 - ``` chr ``` converts numbers to their corresponding characters (97 == a)

###Data.Map

Data.Map exports functions that clash with Prelude and Data.List so should be used as a qualified import.

```hs
import qualified Data.Map as Map
```

Association lists (dictionaries) are lists that store key-value pairs.
Data.Map requires keys to be orderable.

```hs
phoneBook =
    [("alice", "123-4567")
    ,("betty", "123-8912")
    ,("bonnie", "123-3456")
    ,("bonnie", "321-1234")
    ]
```

The ``` fromList ``` function takes an association list and returns a map. If there are duplicate keys the duplicates are discarded.

The ``` empty ``` function returns an empty map

The ``` insert ``` function inserts a key and a value as a pair into a map and returns the new map

```hs
Map.insert 3 100 Map.empty
-- fromList [(3, 100)]
```

The ``` null ``` function checks if a map is empty

```hs
Map.null Map.empty
-- True
```

The ``` size ``` function returns the size of a map

The ``` singleton ``` function takes a key and a value and creates a map that has this one mapping

The ``` lookup ``` function returns Just something if it finds something for the key and Nothing if it doesn't

The ``` member ``` is a predicate that takes a key and a map returning True if the key is in the map

The ``` map ``` and ``` filter ``` functions work as their list equivalents operating on the value

The ``` toList ``` function returns a list from a Map

The ``` keys ``` and ``` elems ``` functions return lists of keys and values

The ``` fromListWith ``` function acts like ``` fromList ``` but instead of discarding duplicates it takes a function that dictates what to do with the extra value

```hs
Map.fromListWith (\n1 n2 -> n1 ++ ", " ++ n2) phoneList
```

The ``` insertWith ``` like ``` fromListWith ``` inserts key-value pairs into map using the supplied function to deal with duplicates

###Data.Set

All elements in a set are unique and ordered so are much faster for checking membership, inserting and deleting elements than lists.

Many names in Data.Set clash with functions in Prelude and Data.List so the module should be imported qualified.

```hs
import qualified Data.Set as Set
```

 - ``` fromList ``` takes a list and converts it to a set
 - ``` intersection ``` takes 2 sets and returns a set of elements shared by both
 - ``` difference ``` takes 2 sets and returns a set of elements that are in the first set but not the second and vice versa
 - ``` union ``` takes 2 sets and returns a set of all unique elements
 - ``` isSubsetOf ``` can be used to check if set A is a subset of set B ``` Set.isSubsetOf Set.fromList [2, 3, 4] Set.fromList [1, 2, 3, 4, 5] ```
 - ``` map ```, ``` filter ```, ``` null ```, ``` size ```, ``` member ```, ``` empty ```, ``` singleton ```, ``` insert ``` and ``` delete ``` all work like their Data.Map counterparts

##Creating Modules

 - Create a file named after the module (ModuleName.hs)
 - At the beginning of the module the module name is specified ``` module ModuleName ```
 - Then specify the functions that are exported ``` ( f1, hello, anotherFunc ) where ```
 - Implement the functions

*ModuleName.hs*
```hs
module ModuleName
( f1
, hello
, anotherFunc
) where
f1 :: [a] -> [a]
f1 (x:xs) = x:x:xs
hello :: String -> String
hello name = "Hello " ++ name
anotherFunc :: Int -> Int
anotherFunc n = n + 1
```

To import the module from inside the same directory we do ``` import ModuleName ```

To create a module that contains submodules create a directory and move the submodule files into the collection. eg. CoolModules/ModuleName.hs

These are imported as ``` import CoolModules.ModuleName ```

##Algebraic data types

Use the ``` data ``` keyword to define data types.

``` data Bool = False | True ``` The part before ``` = ``` denotes the type (Bool) and the parts after are value constructors. The different values they specify that this type can have are separated by ``` | ```.

**Example - A shape type**

```hs
data Shape = Circle Float Float Float | Rectangle Float Float Float Float
```

The Circle value constructor has 3 parameters (the first and second are the coordinates of the centre, the third is it's radius).
``` Circle :: Float -> Float -> Float -> Shape ```

The Rectangle value has 4 parameters (the first and second are the top left coordinates, the third and fourth are the bottom right coordinates).
``` Rectangle :: Float -> Float -> Float -> Float -> Shape ```

```hs
surface :: Shape -> Float
surface (Circle _ _ r) = pi * r ^ 2
surface (Rectangle x1 y1 x2 y2) = (abs $ x2 - x1) * (abs $ y2 - y1)
```

To get a string representation from a data type ``` Circle 10 20 5 ``` we have to add a new type to the ``` Show ``` typeclass by using the ``` deriving ``` keyword otherwise it will just print an error:

```hs
data Shape = Circle Float Float Float | Rectangle Float Float Float Float deriving (Show)
```

```hs
data Point = Point Float Float deriving (Show)
data Shape = Circle Point Float | Rectangle Point Point deriving (Show)

surface :: Shape -> Float
surface (Circle _ r) = pi * r ^ 2
surface (Rectangle (Point x1 x2) (Point y1 y2)) = (abs $ x2 - x1) * (abs $ y2 - y1)
```

**Exporting data types**

```hs
module Shapes
( Point(..)
, Shape(..)
, surface
) where
```

Using ``` (..) ``` exports all the value constructors for that type. Writing ``` Shape(..) ``` is the same as writing ``` Shape (Rectangle, Circle) ```

###Record syntax

```hs
data Person = Person String String Int Float String String deriving (Show)
firstName :: Person -> String
firstName (Person firstname _ _ _ _ _) = firstname
lastName :: Person -> String
lastName (Person _ lastname _ _ _ _) = lastname
age :: Person -> Int
age (Person _ _ age _ _ _) = age
weight :: Person -> Float
weight (Person _ _ _ weight _ _) = weight
phone :: Person -> String
phone (Person _ _ _ _ phonenumber _) = phonenumber
food :: Person -> String
food (Person _ _ _ _ _ food) = food

let p = Person "Joe" "Doe" 43 175.5 "555-1234" "Cheese"

firstName p
-- "Joe"
food p
-- "Cheese"
```

An alternative method of implementing the above is to use record syntax:

```hs
data Person = Person { firstName :: String
                     , lastName :: String
                     , age :: Int
                     , weight :: Float
                     , phone :: String
                     , food :: String
                     } deriving (Show)

let p = Person "Joe" "Doe" 43 175.5 "555-1234" "Cheese"

firstName p
-- "Joe"
food p
-- "Cheese"
```

Using record syntax also yields a better output from deriving Show

```hs
data Car = Car String String Int deriving (Show)
Car "Ford" "Mustang" 1967
-- Car "Ford" "Mustang" 1967

data Car = Car {make :: String, model :: String, year :: Int} deriving (Show)
Car "Ford" "Mustang" 1967
-- Car {make = "Ford", model = "Mustang", year = 1967}
```

###Type parameters

A value constructor can take some values parameters and produce a new value (Car constructor takes 3 values and produces a car value). Type constructors can take types as parameters to produce new types. 

```hs
data Maybe a = Nothing | Just a
```

The ``` a ``` above is a type parameter and ``` Maybe ``` is a type constructor.

 - Type parameters are useful to make different types depending on what types are wanted to be contained in the data type
 - Type parameters are usually used when the type that's contained inside the data type's value constructors isn't important for the type to work

There is a strong convention to never add typeclass constraints in data declarations.

**Implementing a 3D vector type**

```hs
data Vector a = Vector a a a deriving (Show)
vplus :: (Num t) => Vector t -> Vector t -> Vector t
(Vector a, b, c) `vplus` (Vector x, y, z) = Vector (a + x) (b + y) (c + z)
vmult :: (Num t) => Vector t -> t -> Vector t
(Vector a, b, c) `vmult` m  = Vector (a * m) (b * m) (c * m)
```

vplus adds two vectors together, vmult multiplies a vector with a scalar.
A ``` Num ``` class constraint was not needed in the data declaration as it would only be repeated in the functions anyway.

###Derived instances

Haskell can automatically make a type an instance of any of the following typeclasses:
 - Eq
 - Ord
 - Enum
 - Bounded
 - Show
 - Read

Types can manually be made instances of typeclasses by implementing the functions defined by typeclasses.

###Type synonyms

The ``` [Char] ``` and ``` String ``` types are equivalent. This is implemented with type synonyms. There is no functional difference when using type synonyms but can make readability and documentation easier.

```hs
type String = [Char]
```

```hs
phoneBook :: [(String, String)]
phoneBook =
    [("alice", "123-4567")
    ,("betty", "123-8912")
    ,("bonnie", "123-3456")
    ,("bonnie", "321-1234")
    ]
```

Using type synonyms we can change the above code for with the code below. Functionally the code is the same but the code below conveys more information about what the Strings should be used for.

```hs
type PhoneNumber = String
type Name = String
type PhoneBook = [(Name, PhoneNumber)]
phoneBook :: PhoneBook
phoneBook =
    [("alice", "123-4567")
    ,("betty", "123-8912")
    ,("bonnie", "123-3456")
    ,("bonnie", "321-1234")
    ]
```

###Recursive data types

It is possible to make types whose constructors have fields that are of the same type.

**Implementing a binary search tree**

```hs
data Tree a = EmptyTree | Node a (Tree a) (Tree a) deriving (Show, Read, Eq)

singleton :: a -> Tree a
singleton x = Node x EmptyTree EmptyTree
treeInsert :: (Ord a) => a -> Tree a -> Tree a
treeInsert x EmptyTree = singleton x
treeInsert x (Node a left right)
    | x == a = Node x left right
    | x < a = Node a (treeInsert x left) right
    | x > a = Node a left (treeInsert x right)
treeElem :: (Ord a) => a -> Tree a -> Bool
treeElem x EmptyTree = False
treeElem x (Node a left right)
    | x == a = True
    | x < a = treeElem x left
    | x > a = treeElem x right
```

``` treeInsert ``` inserts an element into a tree. Smaller values walk left, larger values walk right.
``` treeElem ``` looks to see if an element is in a tree. Smaller values walk left, larger values walk right.

```hs
let nums = [8, 4, 6, 2, 7, 1, 2, 5]
let numsTree = foldr treeInsert EmptyTree nums

6 `treeElem` numsTree
-- True

9 `treeElem` numsTree
-- False
```

###Typeclasses

Typeclasses are defined using the ``` class ``` keyword

```hs
class Eq a where
    (==) :: a -> a -> Bool
    (/=) :: a -> a -> Bool
    x == y = not (x /= y)
    x /= y = not (x == y)
```

Once we create instances of a class we get some nice functionality

```hs
data TrafficLight = Red | Yellow | Green

instance Eq TrafficLight where
    Red == Red = True
    Green == Green = True
    Yellow == Yellow = True
    _ == _ = false
```

The ``` class ``` keyword is for defining new typeclasses and the ``` instance ``` keyword is for making our types instances of typeclasses.

Because we used mutual recursion in our class definition we only had to overwrite one of the (==) (/=) functions. This is know as the minimal complete definition.

```hs
instance show TrafficLight where
    show Red = "Red light"
    show Yellow = "Yellow light"
    show Green = "Green light"
```

We could have derived Eq and the effect would have been the same but deriving Show would have just translated the value constructors to strings whereas we added some more information ("Red light").

In ghci using the keyword ``` :info yourTypeClass ``` will show which functions the typeclass defines and all the typeclasses that it is an instance of.

###Functor typeclass

Functors are a type that can be mapped over.

```hs
class Functor f where
    fmap :: (a -> b) -> f a -> f b
```

```hs
instance Functor Tree where
    fmap f EmptyTree = EmptyTree
    fmap f (Node x leftsub rightsub) = Node (f x) (fmap f leftsub) (fmap f rightsub)

fmap (+4) (foldr treeInsert EmptyTree [1, 2, 3, 4])
```

Functors should not change the order of the collection they map over 

###Kinds

Type constructors take other types as parameters to eventually produce concrete types.
A kind is the type of a type.
The ``` :k ``` keyword reports the kind of a type. eg.
```hs
:k Int
-- Int :: *
```

``` * ``` means concrete type.

```hs
:k Maybe
-- Maybe :: * -> *
```

Maybe takes one concrete type and returns a concrete type.


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


