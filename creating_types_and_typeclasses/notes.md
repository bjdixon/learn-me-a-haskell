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


