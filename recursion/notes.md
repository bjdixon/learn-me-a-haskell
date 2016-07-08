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

