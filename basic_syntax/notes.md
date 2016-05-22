##REPL

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

|============================================================|
|Operator | Name      | Purpose/application                  |
|============================================================|
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
|============================================================|

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


