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

