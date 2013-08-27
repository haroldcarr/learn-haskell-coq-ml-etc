{-
Created       : 2011 Dec 29 (Thu) 19:40:46 by carr.
Last Modified : 2013 Aug 26 (Mon) 18:08:18 by carr.
-}

import Data.List
import System.Random

import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString as S

import Geometry -- defined in same directory as this current file.

{- ======================================================================== -}

-- 1.3

{-

-- interpreter
ghci

-- load/resoad a file
:l <file>.hs
:r

-}

{- ======================================================================== -}

-- 2.2 function definition

doubleMe x = x + x

doubleUs x y = doubleMe x + doubleMe y

myConanO'BrienConstant = "It's a-me, Conan O’Brien!"

-- 2.3 lists

listOfNumbers = [4 ,8 ,15 ,16 ,23 ,48]
appendedNumbers = [1,2,3,4] ++ [9,10,11,12]
appendedStrings = "hello" ++ " " ++ "world"
consing = 1 : [2, 3]
getB = "Steve Buscemi" !! 6
isFourIn = 4 `elem` [2,3,4]

-- 2.4 range

oneThruTwenty = [1..20]
littleAThruZ = ['a'..'z']
evenToTwenty = [2,4..20]
everyThirdToTwenty = [3,6..20]
twentyToOne = [20,19..1]
-- infinite list
first24MultiplesOf13 = take 24 [13, 26..]
infiniteCycle = take 10 (cycle [1,2,3])
repeatingSingle = take 10 (repeat 'a')

-- 2.5 list comprehension

first10even = [x*2 | x <- [1..10]]
even12 = [x*2 | x <- [1..10], x*2 >= 12]
rem3 = [ x | x <- [50..100], x `mod` 7 == 3]
multiplePredicates = [ x | x <- [10..20], x /= 13, x /= 15, x /= 19]
multipleInputs = [ x*y | x <- [2,5,10], y <- [8,10,11]]
multipleInputsWithPredicate = [ x*y | x <- [2,5,10], y <- [8,10,11], x*y > 50]
length' xs = sum [1 | _ <- xs]
onlyUppercase st = [ c | c <- st, c `elem` ['A'..'Z']]
xxs = [[1,3,5,2,3,1,2,4,5],
       [1,2,3,4,5,6,7,8,9],
       [1,2,4,2,1,6,3,1,3,2,3,6]]
nestedComprehension = [ [ x | x <- xs, even x ] | xs <- xxs]

-- 2.6 tuples

first = fst (1,2)
second= snd (1,2)
zipListToTuples = zip [1,2,3,4,5] [5,5,5,5,5]
diffTypesLength = zip [5,3,2,6,2,7,2,5,4,6,6] ["im","a","turtle"]
infiniteZipFinite = zip [1..] ["apple", "orange", "cherry", "mango"]
tuplesAndComprehension = [ (a,b,c) | c <- [1..10], b <- [1..10], a <- [1..10], a^2 + b^2 == c^2]

{- ======================================================================== -}
-- 3 Types and Typeclasses

{-
:t 'a'
:t True
:t "HELLO!"
:t (True, 'a')
:t 4 == 5
:t [1,2,3]
:t onlyUppercase
-}

onlyUppercaseWithTypeDeclaration :: [Char] -> [Char]
onlyUppercaseWithTypeDeclaration st = [ c | c <- st, c `elem` ['A'..'Z']]

--          x      y      z      return
addThree :: Int -> Int -> Int -> Int
addThree x y z = x + y + z

-- 3.2 type variables

{-
:t head
:t fst
-}

-- 3.3 Typeclasses

{-

Similar to an interface that defines some behavior.
If type is a part of a typeclass, then it implements the behavior the typeclass describes.

:t (==)

Everything before => is called a class constraint

Read (==) type declaration as:
- the equality function takes any two values that are of the same type and returns a Bool.
- The type of those two values must be a member of the Eq class (the class constraint).
- The Eq typeclass provides an interface for testing for equality.

:t elem

:t show
show 5.3
read "5.3" + 4.0

:t read
read "5.3" :: Float

-}

{- ======================================================================== -}
-- 4 syntax in functions

-- 4.1 pattern matching
-- making sure a value conforms to some form and deconstructing it

lucky :: (Integral a) => a -> String
lucky 7 = "LUCKY NUMBER SEVEN!"
lucky x = "Sorry, you’re out of luck, pal!"

sayMe :: (Integral a) => a -> String
sayMe 1 = "One!"
sayMe 2 = "Two!"
sayMe x = "Not between 1 and 2"

factorial :: (Integral a) => a -> a
factorial 0 = 1
factorial n = n * factorial (n - 1)

-- non exhaustive
charName :: Char -> String
charName 'a' = "Albert"
charName 'b' = "Broseph"
charName 'c' = "Cecil"

addTuples :: (Num a) => (a, a) -> (a, a) -> (a, a)
addTuples (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)

xs = [(1,3), (4,3), (2,4), (5,3), (5,6), (3,1)]
patternMatchInComprehensions = [ a + b | (a,b) <- xs]

head' :: [a] -> a
head' [] = error "Can’t call head on an empty list, dummy!"
head' (x:_) = x

tell :: (Show a) => [a] -> String
tell [] = "The list is empty"
tell (x:[]) = "The list has one element: " ++ show x
tell (x:y:[]) = "The list has two elements: " ++ show x ++ " and " ++ show y
tell (x:y:_) = "This list is long. The first two elements are: " ++ " and " ++ show y

mylength :: (Num b) => [a] -> b
mylength [] = 0
mylength (_:xs) = 1 + mylength xs

-- patterns p 37
-- a handy way of breaking something via pattern and binding to names
-- while keeping a reference to the whole thing

capital :: String -> String
capital "" = "Empty string , whoops!"
capital all@(x:xs) = "The first letter of " ++ all ++ " is " ++ [x]

-- 4.2 guards
-- testing if property of a value(s) are true

bmiTell :: (RealFloat a) => a -> String
bmiTell bmi
    | bmi <= 18.5 = "Skinny"
    | bmi <= 25.0 = "Normal"
    | bmi <= 30.0 = "Fat"
    | otherwise   = "Whale"

bmiTell' :: (RealFloat a) => a -> a -> String
bmiTell' weight height
    | weight / height ^ 2 <= 18.5 = "Skinny"
    | weight / height ^ 2 <= 25.0 = "Normal"
    | weight / height ^ 2 <= 30.0 = "Fat"
    | otherwise                   = "Whale"

-- if all guards of evaluate to False and no otherwise, evaluation goes to next pattern
max' :: (Ord a) => a -> a -> a
max' a b | a > b = a
max' a b = b

-- 4.3 where
-- bind variables (global to function) at end of function

bmiTellWhere :: (RealFloat a) => a -> a -> String
bmiTellWhere weight height
    | bmi <= skinny = "skinny"
    | bmi <= normal = "normal"
    | bmi <= fat    = "fat"
    | otherwise     = "whale"
  where bmi = weight / height ^ 2
        skinny = 18.5
        normal = 25.0
        fat = 30.0

bmiTellWherePattern :: (RealFloat a) => a -> a -> String
bmiTellWherePattern  weight height
    | bmi <= skinny = "skinny"
    | bmi <= normal = "normal"
    | bmi <= fat    = "fat"
    | otherwise     = "whale"
  where bmi = weight / height ^ 2
        (skinny, normal, fat) = (18.5, 25.0, 30.0)

-- can do things in patterns or where bindings
initials :: String -> String -> String
initials firstname lastname =
    [f] ++ ". " ++ [l] ++ "."
  where (f:_) = firstname
        (l:_) = lastname

-- functions are values so can be declared in where
calcBmis :: (RealFloat a) => [(a, a)] -> [a]
calcBmis xs = [bmi w h | (w, h) <- xs]
    where bmi weight height = weight / height ^ 2
-- calcBmis [(200, 50), (100, 50), (50, 50)]

-- 4.4 let expressions
-- bind variables locally

cylinder :: (RealFloat a) => a -> a -> a
cylinder r h =
    let sideArea = 2 * pi * r * h
        topArea = pi * r ^2
    in sideArea + 2 * topArea

-- include let inside comprehension like a predicate
-- it does not filter
-- it binds to names
-- names visible to output function (part before |)
-- and all predicates and sections that come after the binding
calcBmisComprehension :: (RealFloat a) => [(a, a)] -> [a]
calcBmisComprehension xs = [bmi | (w, h) <- xs, let bmi = w / h ^ 2]

-- cannot use in (w, h) <- xw part - not defined yet
calcBmisFatOnly :: (RealFloat a) => [(a, a)] -> [a]
calcBmisFatOnly xs = [bmi | (w, h) <- xs, let bmi = w / h ^ 2, bmi >= 25.0]

-- 4.5 case expressions
-- pattern matching on parameters in function definitions is syntactic sugar for case expressions

headF :: [a] -> a
headF []    = error "No head for empty lists!"
headF (x:_) = x

headC :: [a] -> a
headC xs = case xs of []    -> error "No head for empty lists!"
                      (x:_) -> x

{- ======================================================================== -}
-- 5 recursion

maximum' :: (Ord a) => [a] -> a
maximum' [] = error "maximum of empty list"
maximum' [x] = x
maximum' (x:xs)
    | x > maxTail = x
    | otherwise   = maxTail
    where maxTail = maximum' xs -- note: non-strict call here

maximumM :: (Ord a) => [a] -> a
maximumM [] = error "maximum of empty list"
maximumM [x] = x
maximumM (x:xs) = max x (maximumM xs)

replicate' :: (Num i, Ord i) => i -> a -> [a]
replicate' n x
    | n <= 0    = []
    | otherwise = x:replicate' (n-1) x

take' :: (Num i, Ord i) => i -> [a] -> [a]
take' n _
    | n <= 0   = []
take' _ []     = []
take' n (x:xs) = x : take' (n-1) xs

reverse' :: [a] -> [a]
reverse' []     = []
reverse' (x:xs) = reverse' xs ++ [x]

repeat' :: a -> [a]
repeat' x = x:repeat' x

zip' :: [a] -> [b] -> [(a,b)]
zip' _ [] = []
zip' [] _ = []
zip' (x:xs) (y:ys) = (x,y):zip' xs ys

elem' :: (Eq a) => a -> [a] -> Bool
elem' a [] = False
elem' a (x:xs)
    | a == x    = True
    | otherwise = a `elem'` xs

-- 5.4 quick sort

quicksort :: (Ord a) => [a] -> [a]
quicksort [] = []
quicksort (x:xs) =
    let smallerSorted = quicksort [a | a <- xs, a <= x]
        biggerSorted  = quicksort [a | a <- xs, a > x]
    in smallerSorted ++ [x] ++ biggerSorted
-- quicksort [10,2,5,3,1,6,7,4,2,3,4,8,9]
-- quicksort "the quick brown fox jumps over the lazy dog"

{- ======================================================================== -}
-- 6 higher order functions

-- 6.1 curried functions

{-
Haskell functions only take one parameter.
Functions of several parameters are curried, e.g.,
max :: (Ord a) => a -> a -> a
is really
max :: (Ord a) => a -> (a -> a)

If call with too few parameters, we get a partially applied function,
a useful  way to create functions seeded with some data.
-}

multThree :: (Num a) => a -> a -> a -> a
multThree x y z = x * y * z

multTwoWithNine = multThree 9
-- multTwoWithNine 2 3
multWithEighteen = multTwoWithNine 2
-- multWithEighteen 10

-- Infix functions partially applied using sections.
divideByTen :: (Floating a) => a -> a
divideByTen = (/10)

-- must use 'subtract' to curry minus ('-')
subtractFour = (subtract 4)

-- 6.2 higher order

-- first parameter is a function of type a -> a
applyTwice :: (a -> a) -> a -> a
applyTwice f x = f (f x)
-- applyTwice (+3) 10
-- applyTwice (++ " HAHA") "HEY"
-- applyTwice ("HAHA " ++) "HEY"
-- applyTwice (multThree 2 2) 9
-- applyTwice (3:) [1]

zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith' _  []  _        = []
zipWith' _  _   []       = []
zipWith' f (x:xs) (y:ys) = f x y : zipWith' f xs ys

-- zipWith' (+) [4,2,5,6] [2,6,2,3]
-- zipWith' max [6,3,2,1] [7,3,1,5]
-- zipWith' (++) ["foo ", "bar ", "baz "] ["fighters", "hoppers", "aldrin"]
-- zipWith' (*) (replicate 5 2) [1..]

flip' :: (a -> b -> c) -> b -> a -> c
flip' f y x = f x y
-- flip' zip [1,2,3,4,5] "hello"
-- zipWith (flip' div) [2,2..] [10,8,6,4,2]

-- 6.3 maps and filters

map' :: (a -> b) -> [a] -> [b]
map' _ [] = []
map' f (x:xs) = f x : map' f xs

filter' :: (a -> Bool) -> [a] -> [a]
filter' _ [] = []
filter' p (x:xs)
    | p x       = x : filter' p xs
    | otherwise = filter' p xs

-- 6.4 lambdas (anonymous functions)

-- equivalent functionality
mapPlus3 = map (+3)
mapPlus3'= map (\x -> x + 3)

-- 6.5 fold
-- case for the empty list
-- x:xs pattern/action
-- fold encapsulates this pattern
-- folds, maps, filters - primary FP tools

-- foldr f z [3,4,5,6]
-- f 3 (f 4 (f 5 (f 6 z)))

-- foldl g z [3,4,5,6]
-- g (g (g (g z 3) 4) 5) 6

sum' :: (Num a) => [a] -> a
sum' xs = foldl (\acc x -> acc + x) 0 xs

-- better (using curried + and curried definition)
sumAgain :: (Num a) => [a] -> a
sumAgain = foldl (+) 0

elemFL :: (Eq a) => a -> [a] -> Bool
elemFL y ys = foldl (\acc x -> if x == y then True else acc) False ys

-- use fold right when building new lists from given list (performance)
mapFR :: (a -> b) -> [a] -> [b]
mapFR f xs = foldr (\x acc -> f x : acc) [] xs

-- foldl1/foldr1 do not need explicit starting value
-- use first/last element as start
-- fold begins with the element next to start

sumF1 = foldl1 (+)

maximumF :: (Ord a) => [a] -> a
maximumF = foldr1 (\x acc -> if x > acc then x else acc)

reverseF :: [a] -> [a]
reverseF = foldl (\acc x -> x : acc) []

productF :: (Num a) => [a] -> a
productF = foldr1 (*)

filterF :: (a -> Bool) -> [a] -> [a]
filterF p = foldr (\x acc -> if p x then x : acc else acc) []

headFR1 :: [a] -> a
headFR1 = foldr1 (\x _ -> x)

lastF :: [a] -> a
lastF = foldl1 (\_ x -> x)

-- scanl/scanr like foldl/foldr
-- return intermediate accumulator states as list
-- scanl : final result in last element
-- scanr : final result in first element

-- How many elements does it take for the sum of the roots of all natural numbers to exceed 1000?
-- takeWhile (<10) (scanl1 (+) (map sqrt [1..]))
sqrtSums :: Int
sqrtSums = length (takeWhile (<1000) (scanl1 (+) (map sqrt [1..]))) + 1
-- sum (map sqrt [1..131])
-- sum (map sqrt [1..130])

-- 6.6 $ (aka function application)
{-

 ($) :: (a -> b) -> a -> b
 f $ x = f x

 apply :: (a -> b) -> a -> b
 apply f x = f x

 normal function application
     has high precedence
     left-associative : f a b c same as ((f a) b) c))
 $
     has lowest precedence
     right-associative

 convenience function to avoid parentheses

 sum (map sqrt [1..130])
 sum $ map sqrt [1..130]

 sqrt 3 + 4 + 9
 sqrt (3 + 4 + 9)
 sqrt $ 3 + 4 + 9

 sum  (filter (> 10)  (map (*2) [2..10]))
 sum $ filter (> 10) $ map (*2) [2..10]

 $ is function so can be mapped
 map ($ 3) [(4+), (10*), (^2), sqrt]
-}

-- 6.7 function composition : (f ◦ g)(x) = f (g(x))
-- (.) :: (b -> c) -> (a -> b) -> a -> c
-- f . g = \x -> f (g x)

-- function given number, multiplies it by 3 and then negates it
mul3negate :: Integer -> Integer
mul3negate = negate . (* 3)

-- use: making functions to pass to other functions
-- can use lambdas, but often function composition more concise

negateListL = map (\x -> negate (abs x)) [5,-3,-6,7,-3,2,-19,24]
negateListC = map (negate . abs) [5,-3,-6,7,-3,2,-19,24]

-- right-associative
-- f (g (z x)) is (f . g . z) x

negateSumTail = map (negate . sum . tail) [[1..5],[3..6],[1..7]]

-- with functions of several parameters
-- partially apply such that each function takes one parameter

sum1 = sum (replicate 5 (max 6.7 8.9))
sum2 = (sum . replicate 5 . max 6.7) 8.9
sum3 = sum . replicate 5 . max 6.7 $ 8.9

-- use: defining functions in "point free" style (aka "pointless")

sumPoint     :: (Num a) => [a] -> a
sumPoint     xs = foldl (+) 0 xs

sumPointFree :: (Num a) => [a] -> a
sumPointFree    = foldl (+) 0

fnPoint     x = ceiling (negate (tan (cos (max 50 x))))
fnPointFree   = ceiling . negate . tan . cos . max 50

{- ======================================================================== -}
-- 7 modules

-- 7.1 loading modules

-- Prelude module imported by default

-- import Data.List  -- done at top of file
numUniques :: (Eq a) => [a] -> Int
numUniques = length . nub -- nub is remove duplicates

{-
To load into ghci

:m + Data.List
:m + Data.List Data.Map Data.Set

-- selective include
import Data.List (nub, sort)

-- selective exclude
import Data.List hiding (nub)

import qualified Data.Map
-- then must call as: Data.Map.filter

-- import with rename
import qualified Data.Map as M
-- then must call as: M.filter

Use
    http://www.haskell.org/hoogle/
to search for modules.
-}

-- 7.2 Data.List
-- 7.3 Data.Char
-- 7.4 Data.Map
-- 7.5 Data.Set

-- 7.6 making a module

-- import Learn_You_a_Haskell_modules.Geometry -- must run in directory that contains - at top

{- ======================================================================== -}
-- 8 defining types and typeclasses

-- 8.1 Algebraic data types

-- e.g.,
-- data Bool = False | True

{-
module Shapes
( Point (..)
, Shape (..)
, surface
, nudge
, baseCircle
, baseRect ) where

-- Shape(..) exports all value constructors
-- same as Shape (Rectangle, Circle)

-- Could not export value
-- Then only get via "factory" functions (e.g., baseCircle and baseRect)
-- Not exporting constructors makes them more abstract : hide impl
-- Users can’t pattern match against the value constructors.
-}

data Shape = Circle Float Float Float | Rectangle Float Float Float Float
    deriving (Show)

{-
Circle 1 2 3

:t Circle
Circle :: Float -> Float -> Float -> Shape
:t Rectangle
Rectangle :: Float -> Float -> Float -> Float -> Shape

map (Circle 10 20) [4,5,6,6]
-}

surface :: Shape -> Float
surface (Circle _ _ r)          = pi * r ^ 2
surface (Rectangle x1 y1 x2 y2) = (abs $ x2 - x1) * (abs $ y2 - y1)
-- surface (Circle 1 2 3)
-- surface $ Rectangle 1 2 3 4

-- note: type and constructor have same name: common idiom when only one constructor
data Point = Point Float Float deriving (Show)
data ShapeWP = CircleWP Point Float | RectangleWP Point Point deriving (Show)
surfaceWP :: ShapeWP -> Float
surfaceWP (CircleWP _ r) = pi * r ^ 2
surfaceWP (RectangleWP (Point x1 y1) (Point x2 y2)) = (abs $ x2 - x1) * (abs $ y2 - y1)
-- surfaceWP $ RectangleWP (Point 1 2) (Point 3 4)

nudge :: ShapeWP -> Float -> Float -> ShapeWP
nudge (CircleWP (Point x y) r) a b =
    CircleWP    (Point (x+a) (y+b)) r
nudge (RectangleWP (Point x1 y1) (Point x2 y2)) a b =
    RectangleWP (Point (x1+a) (y1+b)) (Point (x2+a) (y2+b))
-- nudge (CircleWP (Point 34 34) 10) 5 10

baseCircle :: Float -> ShapeWP
baseCircle r = CircleWP (Point 0 0) r

baseRect :: Float -> Float -> ShapeWP
baseRect width height = RectangleWP (Point 0 0) (Point width height)

-- 8.2 Record syntax

data Person = Person {
      firstName :: String
    , lastName  :: String
    , age       :: Int
} deriving (Show)

-- :t age

harold = Person {firstName="Harold", lastName="Carr", age=25}

-- 8.3 Type parameters
{-
 take types as parameters : produces new types

-- 'a' is type param
-- 'Maybe'' is a type constructor
-- constructor (not a type) constructs types: Maybe Int, Maybe String, ...
-- depending on type of argument
data Maybe' a = Nothing' | Just' a deriving (Show)

 use: when type works regardless of type of value it holds
 whenever type acts as box, then  good to use
 when type contained inside not important for type to work (like list: [1,2] ['a', 'b']

Convention typeclass constraints in data declarations.
No benefit.
If add constraint then all functions used must have constraint in definition.
-}

-- do not add a Num class constraint : Num constraint used in functions below
data Vector a = Vector a a a deriving (Show)

vplus :: (Num t) => Vector t -> Vector t -> Vector t
(Vector i j k) `vplus` (Vector l m n) =
    Vector (i+l) (j+m) (k+n)

vectMult :: (Num t) => Vector t -> t -> Vector t
(Vector i j k) `vectMult` m =
    Vector (i*m) (j*m) (k*m)

scalarMult :: (Num t) => Vector t -> Vector t -> t
(Vector i j k) `scalarMult` (Vector l m n) =
    i*l + j*m + k*n

{-
Vector 3 5 8 `vplus` Vector 9 2 8
Vector 3 5 8 `vplus` Vector 9 2 8 `vplus` Vector 0 2 3
Vector 3 9 7 `vectMult` 10
Vector 4 9 5 `scalarMult` Vector 9.0 2.0 4.0
Vector 2 9 3 `vectMult` (Vector 4 9 5 `scalarMult` Vector 9 2 4)
-}

-- 8.4 Derived instances
{-
A type can be made an instance of a typeclass if it supports that behavior.

E.G., Int is instance of Eq typeclass because the Eq typeclass defines equality behavior.

Eq contains ==, /= so can be used on instances.

Typeclasses are like interfaces.

We don’t make data from typeclasses.

We define data type and then consider its behavior.

If it acts like something that can be equated, make it instance of Eq typeclass.

If it acts like something that can be ordered, make it instance of Ord typeclass.

Automatically make type an instance of: Eq, Ord, Enum, Bounded, Show, Read ia "deriving".
All fields must be part of Eq too.
-}

data Peep = Peep {
      firstPeep :: String
    , lastPeep  :: String
    , agePeep   :: Int
} deriving (Eq, Show, Read)

flavia = Peep {firstPeep = "Flavia", lastPeep = "Cervino-Wood", agePeep = 53}
-- flavia == flavia

-- parameterless constructors
-- Enum typeclass for things hat have predecessors and successors
-- Bounded typeclass for things that have lowest and highest possible value
data Day = Monday | Tuesday | Wednesday | Thursday | Friday | Saturday | Sunday
    deriving (Eq, Ord, Show, Read, Bounded, Enum)
{-
:t Monday
show Monday
read (show Monday) :: Day
Monday > Tuesday
minBound :: Day
maxBound :: Day
succ Monday
pred Saturday
[Thursday .. Sunday]
[minBound .. maxBound] :: [Day]
-}

-- 8.5 Type synonyms

-- [Char] and String types are equivalent
-- implemented with type synonyms
-- type String = [Char]

type PhoneNumber = String
type Name = String
type PhoneBook = [(Name,PhoneNumber)]
-- phoneBook :: [(String,String)] - replace with next: more readable
phoneBook :: PhoneBook
phoneBook =
    [("betty","555-2938")
    ,("bonnie","452-2928")
    ,("patsy","493-2928")
    ,("lucille","205-2928")
    ,("wendy","939-8282")
    ]
inPhoneBook :: Name -> PhoneNumber -> PhoneBook -> Bool
inPhoneBook name pnumber pbook = (name,pnumber) `elem` pbook

-- Type synonyms can be parameterized with type variables
type AssocList k v = [(k,v)]

-- can partially apply type parameters and get new type constructors
-- (just like partial application of "normal" functions)
-- type IntMap v = Map Int v
-- type IntMap = Map Int

{-
diversion:

Maybe a results of computations that can "fail" or give real result.
Somtimes need to convey more info on "failure".
Use:
  data Either a b = Left a | Right b deriving (Eq, Ord, Read, Show)
Erors use the Left value constructor
Results use Right
-}

-- 8.6 Recursive data structures

-- types whose constructors have fields that are of the same type
data List  a = Empty  | Cons a (List a) deriving (Show, Read, Eq, Ord)
data List' a = Empty' | Cons' { listHead :: a, listTail :: List' a} deriving (Show, Read, Eq, Ord)

{-
Define infix functions or constructors comprised of special characters.

Fixity states how tightly the operator binds and whether left or right associative.

e.g., infixl 7 *
      infixl 6 +

-}

infixr 5 :-:
data ListI a = EmptyI | a :-: (ListI a) deriving (Show, Read, Eq, Ord)

-- binary search tree

data Tree a = EmptyTree | Node a (Tree a) (Tree a) deriving (Show, Read, Eq)

singleton :: a -> Tree a
singleton x = Node x EmptyTree EmptyTree
treeInsert :: (Ord a) => a -> Tree a -> Tree a
treeInsert x EmptyTree = singleton x
treeInsert x (Node a left right)
    | x == a = Node x left right
    | x <  a = Node a (treeInsert x left) right
    | x >  a = Node a left (treeInsert x right)

treeElem :: (Ord a) => a -> Tree a -> Bool
treeElem x EmptyTree = False
treeElem x (Node a left right)
    | x == a = True
    | x <  a = treeElem x left
    | x >  a = treeElem x right

nums = [8,6,4,1,7,3,5]
numsTree = foldr treeInsert EmptyTree nums
-- 8 `treeElem` numsTree
-- 9 `treeElem` numsTree

-- 8.7 Typeclasses (continued, see 3.3)
-- how to make typeclasses and make types instances of them

{-
Recap:

typeclasses are like interfaces: defines some behavior

types that have that behavior are made instances of a typeclass

behavior via defining functions or (just) type declarations

a type is an instance of a typeclass means we can use the functions
that the typeclass defines with that type

Typeclasses have pretty much nothing to do with classes in languages like Java or Python.

-- Example typeclass definition
class Eq a where
    (==) :: a -> a -> Bool
    (/=) :: a -> a -> Bool
    x == y = not (x /= y)
    x /= y = not (x == y)
-}

data TrafficLight = Red | Yellow | Green
    -- deriving (Eq) - could do this, but will by hand below instead

instance Eq TrafficLight
  where
    Red    == Red    = True
    Green  == Green  = True
    Yellow == Yellow = True
    _ == _           = False

-- "class"    for defining new typeclasses
-- "instance" for making types an instance of a typeclass

{-
Because == defined in terms of /= and vice versa in class
declaration, then only need to overwrite one in instance
declaration.

If Eq was defined:

class Eq a where
    (==) :: a -> a -> Bool
    (/=) :: a -> a -> Bool

then would need to implement BOTH functions in instance.
-}

instance Show TrafficLight
  where
    show Red    = "Red light"
    show Yellow = "Yellow light"
    show Green  = "Green light"

-- Red == Red
-- Red `elem` [Red, Yellow, Green]
-- [Red, Yellow, Green]


-- typeclasses that are subclasses of other typeclasses

{-
-- subclassing via class constraint on a class declaration (constraints can go many places)
class (Eq a) => Num a where

The above says type a must be an instance of Eq: make a type an
instance of Eq before we can make it an instance of Num.


To make type constructors (e.g., Maybe, list) as instances of typeclasses:

instance (Eq m) => Eq (Maybe m)
  where
    Just x  == Just y  = x == y
    Nothing == Nothing = True
    _       == _       = False


Usually, class constraints in class declarations used
to make a typeclass a subclass of another typeclass.

Usually, class constraints in instance declarations are used to
express requirements about the contents of some type.

:info Eq
:info Num
:info Maybe
-}

-- 8.8 a yes-no typeclass example (like CL or Javascript concept of trueness)

class YesNo a
  where
    yesno :: a -> Bool

instance YesNo Int
  where
    yesno 0 = False
    yesno _ = True

instance YesNo [a]
  where
    yesno [] = False
    yesno _  = True

instance YesNo Bool
  where
    yesno = id --  standard library function that takes a parameter and returns the same thing

instance YesNo (Maybe a)
  where
    yesno (Just _) = True
    yesno Nothing  = False

instance YesNo (Tree a)
  where
    yesno EmptyTree = False
    yesno _         = True

instance YesNo TrafficLight
  where
    yesno Red = False
    yesno _   = True

yesnoIf :: (YesNo y) => y -> a -> a -> a
yesnoIf yesnoVal yesResult noResult =
    if yesno yesnoVal
    then yesResult
    else noResult

-- yesnoIf [2,3,4] "YEAH!" "NO!"
-- yesnoIf []      "YEAH!" "NO!"


-- 8.9 The Functor typeclass
-- for things that can be mapped over

{-
class Functor f
  where
    fmap :: (a -> b) -> f a -> f b

In typeclasses above, the type variable was a concrete type (a type
that a value can hold, like Int, Bool or Maybe).

Here f is type constructor that takes one type parameter.

(Remember: Maybe Int is concrete type, Maybe is type constructor that takes type as parameter).

fmap takes a function from one type to another and a functor applied
with one type and returns a functor applied with another type.

-- takes a function from one type to another and a list of one type
-- returns a list of another type
-- a functor: map is fmap that works only on lists
map :: (a -> b) -> [a] -> [b]

instance Functor []
  where
    fmap = map

:t map
:t fmap

map  (*2) [1..3]
fmap (*2) [1..3]

Types that can act like a box can be functors.

TODO : finish this section
-}

-- 8.10 Kinds and some type-foo
-- TODO: do this section


{- ======================================================================== -}
-- 9 input and output

{-
9.1 hello world

ghc --make helloworld
./helloworld

:t putStrLn
:t putStrLn "hello, world"

putStrLn
  takes String
  returns an I/O action that has result type () (i.e. the empty tuple, also know as unit)

I/O action
  carries out an action with a side-effect
  contain a return value inside it

I/O action performed when given name "main" then execute program

:t do "foo"
"do" syntax glues several I/O actions into one

main :: IO <concrete type>

by convention, do not specify  type declaration for main

:t getLine

Use <- to get data inside IO action (only when inside another I/O action)

in a do block, the last action cannot be bound to a name

putStrLn "in GHCI"

runhaskell ./helloworld.hs
runhaskell ./reverseinput.hs

:t return
"return" is opposite of "<-"
"return" puts stuff in the IO box
"<-" gets stuff out of the IO box

In I/O actions "return" makes an I/O action out of a pure value.
Haskell "return" nothing like other languages.
"return" does not cause the I/O do block to end execution.

main = do
    return ()
    return "HAHAHA"
    line <- getLine
    return "BLAH BLAH BLAH"
    return 4
    putStrLn line

The above returns make I/O actions that do not do anything except have
an encapsulated result (which is thrown away because not bound)

Can use return in combination with <- to bind stuff to names.

:t putStr
:t putStrLn
:t getLine
:t putChar
:t getChar
-- equivalent to printStrLn . show
:t print


:t Control.Monad.when
import Control.Monad
main = do
    c <- getChar
    when (c /= ’ ’) $ do
        putChar c
        main


:t sequence
main = do
    a <- getLine
    b <- getLine
    c <- getLine
    print [a,b,c]

main = do
    rs <- sequence [getLine , getLine , getLine]
    print rs

-- to print each element in list
sequence (map print [1,2,3,4,5])
-- common so use:
:t Control.Monad.mapM
:t Control.Monad.mapM_

:t Control.Monad.forever
:t Control.Monad.forM
-}


-- 9.2 files and streams

{-
-- get until EOF
:t getContents
runhaskell ./capslocker
cat ./capslocker.hs | runhaskell ./capslocker.hs



:t interact
  pattern: get string from input, transform, output
  takes a function of type String -> String as a parameter
  returns I/O action that take input, run function on it, print function result
cat ./Learn_You_a_Haskell.hs | runhaskell ./shortlinesonly.hs


:t System.IO.openFile
:info System.IO.FilePath
:info System.IO.IOMode
:info System.IO.Handle
:t System.IO.hGetContents
:t System.IO.hClose

:t System.IO.withFile

:t System.IO.hGetLine
:t System.IO.hGetChar
:t System.IO.hPutStr
:t System.IO.hPutStrLn

:t System.IO.readFile
runhaskell ./girlfriend.hs




:t System.IO.writeFile
runhaskell ./girlfriendcaps.hs


:t System.IO.appendFile
runhaskell ./appendtodo.hs
cat ./todo.txt


:t System.IO.hSetBuffering
:t System.IO.hFlush


:t System.Directory.getTemporaryDirectory
:t System.IO.openTempFile
:t unlines
:t Data.List.delete
runhaskell ./deletetodo.hs
cat ./todo.txt
-}


-- 9.3 command line arguments

{-
:t System.Environment.getArgs
:t System.Environment.getProgName
runhaskell ./argtest.hs first second 3 "fourth position"
runhaskell ./todo.hs
runhaskell ./todo.hs view
runhaskell ./todo.hs view ./todo.txt
runhaskell ./todo.hs view ./todo.txt bar
runhaskell ./todo.hs add ./todo.txt "Spank the Monkey"
runhaskell ./todo.hs remove ./todo.txt 3
runhaskell ./todo.hs bad ./todo.txt
-}


-- 9.4 randomness

{-
-- takes a random generator (source of randomness)
-- returns a random value and a new random generator
:t System.Random.random
:info System.Random.Random
:info System.Random.RandomGen
:info System.Random.StdGen
:t System.Random.mkStdGen

import System.Random
random (mkStdGen 100) :: (Int, StdGen)
random (mkStdGen 100) :: (Int, StdGen)
-- random (mkStdGen 949488) :: (Float, StdGen) -- causes a seg fault on Lion
random (mkStdGen 949488) :: (Bool, StdGen)
random (mkStdGen 949488) :: (Integer, StdGen)
-}

threeCoins :: StdGen -> (Bool , Bool , Bool)
threeCoins gen =
    let (firstCoin,  newGen)   = random gen
        (secondCoin, newGen')  = random newGen
        (thirdCoin,  newGen'') = random newGen'
    in (firstCoin , secondCoin , thirdCoin)

{-
threeCoins (mkStdGen 21)
threeCoins (mkStdGen 22)
threeCoins (mkStdGen 943)
threeCoins (mkStdGen 944)

-- randoms returns infinite sequence
take 5 $ randoms (mkStdGen 11) :: [Int]
take 5 $ randoms (mkStdGen 11) :: [Bool]
take 5 $ randoms (mkStdGen 11) :: [Float] -- always same value - wrong

-- random in range
randomR (1,6) (mkStdGen 359353)
take 10 $ randomRs ('a','z') (mkStdGen 3) :: [Char]


When program starts, it asks the system for a good random number
generator and stores that in a so called global generator. getStdGen
fetches that global random generator when you bind it to something.
(Note: calling getStdGen more than once will get the same global
generator.)

:t getStdGen
getStdGen

runhaskell ./randomstring.hs
-}

-- 9.5 bytestrings

{-
Processing files as strings tends to be slow.
Bytestrings sort of like lists.
Each element is one byte (8 bits).
Strict : Data.ByteString
Lazy   : Data.ByteString.Lazy -- lazy in chunks, not elements
  Has many of same functions as Data.List such as
  head, tail, init, null, length, map, reverse, foldl, foldr, concat, takeWhile, filter, ...

  Also has functions like System.IO, where Strings ByteStrings.

:t Data.ByteString.readFile

-- I get error when doing this in ghci
-- I put at top of this file and did :l <file> - that works
import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString as S

B.pack [99,97,110]
B.pack [97..122]
B.unpack $ B.pack [97..122]
B.fromChunks [S.pack [40,41,42], S.pack [43,44,45], S.pack [46,47,48]]
B.cons  85 $ B.pack [80,81,82,84]
B.cons' 85 $ B.pack [80,81,82,84] -- strict version
foldr B.cons  B.empty [50..60]
foldr B.cons' B.empty [50..60]

runhaskell ./bytestringcopy.hs ./Learn_You_a_Haskell.hs /tmp/JUNK
cat /tmp/JUNK
-}

{- ======================================================================== -}
-- 10 functionally solving problems

-- think about type declaration of a function before its implementation

-- 10.1 Reverse Polish notation calculator

-- import Data.List

solveRPN :: String -> Float
solveRPN = head . foldl foldingFunction [] . words
    where foldingFunction (x:y:ys) "*"    = (x *  y):ys
          foldingFunction (x:y:ys) "+"    = (x +  y):ys
          foldingFunction (x:y:ys) "-"    = (y -  x):ys
          foldingFunction (x:y:ys) "/"    = (y /  x):ys
          foldingFunction (x:y:ys) "^"    = (y ** x):ys
          foldingFunction (x:xs) "ln"     = log x:xs
          foldingFunction xs "sum"        = [sum xs] -- [sum [1,2,3]] is [6]
          foldingFunction xs numberString = read numberString:xs

{-
solveRPN "2.7 ln"
solveRPN "10 10 10 10 sum 4 /"
solveRPN "10 10 10 10 10 sum 4 /"
solveRPN "10 2 ^"
-}


-- 10.2 Heathrow to London (shortest path)

-- TODO - do this section

{- ======================================================================== -}
-- 11 Functors, Applicative Functors and Monoids
-- http://learnyouahaskell.com/functors-applicative-functors-and-monoids

-- typeclasses enable high-level polymorphism
-- do not need type to belong to hierarchy of types
-- instead, go by what types can act like and then connect them with the appropriate typeclasses

-- functors are things that can be mapped over (e.g., lists, trees)




-- TODO (continue)

{- ======================================================================== -}
-- 12 A Fistful of Monads
-- TODO (only online version)

{- ======================================================================== -}
-- 13 For a Few Monads More
-- TODO (only online version)

{- ======================================================================== -}
-- 14 Zipper
-- TODO (only online version)















