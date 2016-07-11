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

