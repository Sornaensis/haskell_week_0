module Main where

import Prelude hiding (map,filter,Maybe(..))

-- Recursive algorithms & Basic Syntax with Pattern Matching

-- Fibonacci sequence
fib :: Int -> Int
fib 1 =  0
fib 2 =  1
fib n | n < 1       = 0
      | otherwise   = fib (n-2) + fib (n-1)

-- Haskell does not allow for function overriding, however you will often see
-- functions defined multiple times, as above.
-- The reason for this is that function 'bindings', as we call them in haskell,
-- are able to do pattern matching over their arguments.
-- In the above function, if we give 1 or 2 as an input, we choose the corresponding
-- right hand side as the 'result'
-- These bindings are checked in order of appearance so if we put the third binding 'fib n = ...'
-- first, we would never hit them and our program would not work, because n binds to anything.
-- the '|' character begins pattern 'guards' where we can evaluate an expression, and if it's true we use the RHS of that equals sign
-- they allow us to use the pattern matched variables on the LHS in the expression.
-- 'otherwise' is just a synonym for True but it looks a bit nicer

-- Lists, finite or infintie

allEvens :: [Int]
allEvens = [2,4..]

noNumbers :: [Int]
noNumbers = []

-- Lists in haskell are written as comma-separated values in square brackets
-- Lists can be finite, or infinite and there is a lot of special syntax for
-- dealing with them in haskell. We can put whatever we want into them!

aList       = [Just 1, Just 2, Nothing, Just 5]
anotherList = [2.67, 1.345, 1.1]
listOfLists = [ [2, 3, 4], [1, 2, 5], [1, 1] ]
booleans    = [True, True, False, True]
functions   = [(+),(*),(/)]

-- Lists are arbitrary in size; the type of a list doesn't tell you
-- about how many elements it has.

-- The brackets are syntax sugar for explicit use of the list constructor (:)
-- with [] (The empty list) as a terminator

aList'        = Just 1 : Just 2 : Nothing : Just 5 : []
anotherList'  = 2.67 : 1.345 : 1.1 : []
listOfLists'  = (2 : 3 : 4 : []) : (1 : 2 : 5 : []) : (1 : 1 : []) : []
booleans'     = True : True : False : True : []
functions'    = (+) : (*) : (/) : []

-- The above are the de-sugared versions of the comma list syntax from the previous set of functions
-- In haskell, Boolean values are not primitives, they are a normal datatype defined in the standard library similar to:

data Bool' = True' | False' deriving (Show,Eq,Read)

-- Just without the '
-- The '|' here is pronounced 'Or'; as in, a value of Bool' can be either True', OR, False'. But nothing else.
-- There is no null reference in haskell.

-- To get a better sense of datatypes, let's make our own List Data Type, and briefly introduce the concept of Algebraic datatypes 

-- In haskell, when we want to define a new type, we also write it as an equation; the same as a function:

data List a = Cons a (List a) | Nil deriving (Show,Read,Eq,Ord)

-- As defined by GHC
-- data [] a = a : [a] | [] deriving (Show,Read,Eq,Ord)

-- There are two things happening above: the first is the definition
-- of a new TYPE, called List. The lower case 'a' is a type variable, it means that List isn't a type all on its own, it's a composite type.
-- This is somewhat similar to List<T> in java-- T is a generic parameter and we don't know more about it. It could be any type.
-- On the RHS of the equals sign we define 'constructors' for this type.
-- Constructors are just functions.
-- The first constructor is called 'Cons', which is short for 'construct', because
-- we use this to construct our lists. The lower case 'a' and the 'List a' are _parameters_
-- to this function. so if we write:

ourList :: List Int
ourList = Cons 2 (Cons 4 Nil)

-- You can see we have a function of type 'List Int', similar to 'allEvens'
-- Except here we aren't using special, convenient syntax to write it. (and it isn't infinite)
-- 'Nil' is a constructor that takes no arguments, yet produces a 'List a'
-- Cons is similar to List( T t, List<T> end ); in java while Nil is similar to List();
-- They are both constructors, but they take different parameters. We use Nil to terminate
-- our linked lists because it takes no parameters.

-- Algebraic datatypes are datatypes that are constructred by combining other types together.
-- More or less, you can think of them as trees, or linked lists, where we can 'hook' things together to build
-- our datatypes.

-- Pattern matching on lists

-- Add the first three elements of a list, if possible
addFirstThree :: [Int] -> Int
addFirstThree []        = error "Empty list!"
addFirstThree [a]    = a
addFirstThree [a,b]  = a+b
addFirstThree (a:b:c:_)   = a + b + c

-- In addition to numbers, we can also pattern match on any datatype, as shown above.
-- When we write [a] on the LHS of an equals sign, it matches a list with a single element.
-- The syntax (a:as) is the basic list pattern match. a is the first element of the list, and bs is the rest of the list.
-- This means if you have the type [Int], and you match on (a:as), a is an Int, and bs is a [Int], because lists are linked lists.
-- The underscore, _, is an un-named variable. We use it when we want to ignore whatever value we are matching on so we don't have any un-used variables.
-- 'where' is a keyword that begins a nested block of bindings in a top level binding, more or less subfunctions.

-- Concatenate and add elements to lists:

twoLists    = [1,3..9] ++ [2,4..10]  
addElement  = 2 : [4,6,8]

-- append to end?

-- More datatypes

-- Let's create a new data type to make our addFirstThree a bit nicer instead of throwing an exception.
-- In functional languages we can create types to encapsulate the concept of whether something exists,
-- or does not. This is similar to 'null' in java, but it's more type safe.

data Maybe a = Just a | Nothing deriving (Show,Eq,Ord,Read)

-- Here our constructor Just takes a single parameter-- the value we will store inside,
-- and Nothing is similar to Nil or []; it's just a value by itself, and doesn't take any
-- parameters

addFirstThreeSafe :: [Int] -> Maybe Int
addFirstThreeSafe [a]        = Just a 
addFirstThreeSafe [a,b]      = Just (a+b)
addFirstThreeSafe (a:b:c:_)  = Just (a+b+c)
addFirstThreeSafe _          = Nothing

-- Now that we have changed the type, you see we can return a sensible value for any
-- input to this function.


-- Type signatures in more detail

-- name     type signature
function :: Int -> Int 
function a = a + 1

-- Type signatures are written with -> because they are very general
-- Int -> Int   means that the function is a mapping from Int to Int, aka
-- its input is an Int, and its 'output' or result is an Int as well

function2 :: Int -> Int -> Int
function2 a b = a + b

-- Multi-argument functions in haskell are a syntactical convenience
-- Int -> Int -> Int is actually read as Int -> (Int -> Int)   meaning
-- that this function takes an Int, and returns a function that takes
-- and Int and gives you an Int as a result. When providing multiple
-- arguments to a function, they are treated like they take
-- multiple arguments naturally.
-- e.g. function2 1 2 == (function 1) 2


-- What does this mean?
function3 :: Int -> (Int -> Int) -> Int
function3 a f = f a 

-- ex. function3 5 function == 6

-- It means that functions are naturally higher-order in haskell.
-- As you can see here, we have parenthesized the inner 'argument' of this function.
-- Doing so means that we have defined a function which takes an Int, and
-- a *function* from Int -> Int, to produce our resulting Int

-- Recognizable higher-order functions

map :: (a -> b) -> [a] -> [b]

-- Map is probably the most recognizable higher-order function.
-- It appears in some form in almost all programs where data is processed somehow, 
-- because it is such a simple but powerful tool.
-- Here, we begin with a function (note the parentheses) which can 
-- provide our 'mapping' from some type 'a' to some other type 'b',
-- and a list whose elements are of the first type. 
-- We then apply this function to each element, and construct a 'new' 
-- list, all of whose elements are of type 'b'.

-- Polymorphism

id :: a -> a
id a = a

-- Whenever we see a lower-case letter in a type signature, that indicates a type-variable.
-- This is similar to generics in java where we can have a function like <T> doSomething( T t ); and so on.

-- Both map, and id are polymorphic. They can operate on any arbitrary a's or b's, because the actual type
-- is unimportant. If you have any a, I can give it back to you without knowing what it is, that's id;
-- and similarly if you give me a function that can turn an a into a b, then I can take a list containing a bunch of a's
-- and give you back a list containing b's, without the actual types of these things concerning me. 

-- This idea is very important to programming in haskell, where we are more concerned with the structure of things,
-- in the abstract, rather than the specific details of concrete types.
-- In java we accomplish this by using Interfaces to abstract away how implementations should behave.

-- Other higher order combinators you might recognize from FF-core

filter :: (a -> Bool) -> [a] -> [a]
flatMap :: (a -> [b]) -> [a] -> [b]

map f []      = []
map f (a:as)  = f a : map f as

filter f []      = []
filter f (a:as)  | f a        = a : filter f as 
                 | otherwise  = filter f as

flatMap f []     = []
flatMap f (a:as) = f a ++ flatMap f as

-- List Processing

aBunchaNumbers = [1..1000]

-- Above, we have a big list of numbers. Imagine we want to do stuff with this list,
-- for instance: summing all the numbers. How could we do that?
-- In haskell we find ourselves concerned with abstract patterns that apply to lots
-- of cases, including trivial ones like adding a bunch of numbers together.

-- For example if we have this list of numbers we can imagine that summing them up
-- is a process of 'building up' a result from the first number, onwards, or from some
-- starting point, such as 0 in the case of addition.

-- We call this abstraction 'folding' in haskell, because it applies to anything we can
-- fold up, including lists.

foldUp :: (a -> b -> b) -> b -> [a] -> b

-- This is a slightly more complicated type signature so let's walk through it slowly.
-- Our first argument here is a function from TWO values, an a and a b, to another b.
-- a is the type of thing in the input list, and b is the type of the 'result', or what we 'fold' the list into. 
-- We then give an initial 'b' value to start us off, and the list we want to 'fold up'

-- So you can imagine, we apply our function to the first element of the list and the starting
-- 'b' value. We then feed the result of this back with the next value, and so on, until we 
-- have done this with all the elements of the list.

foldUp f b []      = b
foldUp f b (a:as)  = foldUp f (f a b) as

sumUp = foldUp (+) 0 

sumTo100 = sumUp [1..100]

-- Quick sort, ascending order 
sort :: Ord a => [a] -> [a]
sort []     = []
sort (x:xs) = lt ++ [x] ++ gte
      where
      gte = sort (filter (>= x) xs)
      lt  = sort (filter (< x) xs)

-- Here we use pattern matching, guards, and recursive functions to implement a sorting function


main :: IO ()
main = do
   let numbers = [1..20]
   print $ function3 5 function
   print $ fmap fib numbers
   print $ map (+1) numbers
   print $ filter even numbers
   print $ flatMap (\n -> replicate n n) numbers
   print $ foldUp (+) 0 aBunchaNumbers
   print $ sort [2,5,10,23,4,111,3,2,18,2]
