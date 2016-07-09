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

