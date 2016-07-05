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

