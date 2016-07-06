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

If a type is part of a typeclassit implements the behavior a typeclass describes.

**Basic typeclasses**

 - ``` Eq ``` used for testing equality. Members implement ``` == ``` and ``` /= ``` functions. ``` :t (==) ```
 - ``` Ord ``` used for types that have an ordering. Members implement ``` > ```, ``` < ```, ``` >= ``` and ``` <= ```. ``` :t (>) ```
 - ``` Show ``` Members implement ``` show ```

