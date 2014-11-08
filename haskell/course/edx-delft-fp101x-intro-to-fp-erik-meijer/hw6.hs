{-
Created       : 2014 Nov 07 (Fri) 17:05:07 by Harold Carr.
Last Modified : 2014 Nov 07 (Fri) 21:03:08 by Harold Carr.
-}

import           Test.HUnit      as T
import           Test.HUnit.Util as U

-- HOMEWORK 6

------------------------------------------------------------------------------
-- EXERCISE 0

e00 f p xs = [f x | x <- xs , p x]
e01 f p xs =    map p (map f xs)
e02 f p xs = filter p (map f xs)
e03 f p xs = map f (filter p xs)
e04 f p xs = map f (takeWhile p xs)

e0 :: [Test]
e0 = U.tt "e0"
     [
       e00 (*10) even [1..8::Int]
     ---------------
--     , e01 (*10) even [1..8::Int] -- wrong result type
--     , e02 (*10) even [1..8::Int] -- wrong result because function applied in wrong order
     , e03 (*10) even [1..8::Int]
--     , e04 (*10) even [1..8::Int] -- doesn't consider all possible xs
     ]
     [20,40,60,80]

------------------------------------------------------------------------------
-- EXERCISE 1

e1 :: [Test]
e1 = U.tt "e1"
     [
            all even [2,4,6::Int]
     , not (all even [2,3,6::Int])
     -----------------
     -- 1
     ,      and (map even [2,4,6::Int])
     , not (and (map even [2,3,6::Int]))
     -- 2
     -- , map even (and [2,4,6::Int]) -- wrong type to and
     -- 3
     ,      (and . map even) [2,4,6::Int]
     , not ((and . map even) [2,3,6::Int])
     -- 4
     ,      (not . any (not . even)) [2,4,6::Int]
     , not ((not . any (not . even)) [2,3,6::Int])
     -- 5
     --   , (map even . and) [2,4,6::Int] -- wrong type to and
     -- 6
     ,      foldl (&&) True (map even [2,4,6::Int])
     , not (foldl (&&) True (map even [2,3,6::Int]))
     -- 7 - incorrect base value
     -- 8
     ,      (foldl (&&) True . map even) [2,4,6::Int]
     , not ((foldl (&&) True . map even) [2,3,6::Int])
     ]
     True

------------------------------------------------------------------------------
-- EXERCISE 2

e2 :: [Test]
e2 = U.tt "e2"
     [      any odd [2,3,6::Int]
     , not (any odd [2,4,6::Int])
     --------------
     -- 1
     --  , (map odd . or) [2,3,6::Int] -- wrong type to or
     -- 2
     ,      (or . map odd) [2,3,6::Int]
     , not ((or . map odd) [2,4,6::Int])
     -- 3
     ,      length (filter odd [2,3,6::Int]) > 0
     , not (length (filter odd [2,4,6::Int]) > 0)
     -- 4
     ,      (not . null. dropWhile (not . odd)) [2,3,6::Int]
     , not ((not . null. dropWhile (not . odd)) [2,4,6::Int])
     -- 5
     -- , (null . filter odd) [2,3,6::Int] -- wrong result
     -- 6
     ,      not (all (\x -> not (odd x)) [2,3,6::Int])
     , not (not (all (\x -> not (odd x)) [2,4,6::Int]))
     -- 7
     ,      foldr (\x acc -> (odd x) || acc) False [2,3,6::Int]
     , not (foldr (\x acc -> (odd x) || acc) False [2,4,6::Int])
     -- 8
     ,      foldr (||) True (map odd [2,3,6::Int])
     -- , not (foldr (||) True (map odd [2,4,6::Int])) -- wrong base
     ]
     True

------------------------------------------------------------------------------
-- EXERCISE 3

e34 p = foldl (\acc x -> if p x then x : acc else acc) []

e3 :: [Test]
e3 = U.tt "e3"
     [ takeWhile odd [1,3,4,5::Int]
     ------------
--     , e34 odd [1,3,4,5::Int] -- wrong result
     ]
     [1,3]

------------------------------------------------------------------------------
-- EXERCISE 4

e41 _ [] = []
e41 p (x:xs)
    | p x       = e41 p xs
    | otherwise = x:xs

e42 _ [] = []
e42 p (x:xs)
    | p x       = e42 p xs
    | otherwise = xs

e4 :: [Test]
e4 = U.tt "e4"
     [ dropWhile even [2,4,6,7,8,9,10::Int]
     -----------
     , e41 even [2,4,6,7,8,9,10::Int]
     -- , e42 even [2,4,6,7,8,9,10::Int] -- misses element
     ]
     [7,8,9,10]

------------------------------------------------------------------------------
-- EXERCISE 5

e5 :: [Test]
e5 = U.tt "e5"
     [ map (*10) [1,2,3::Int]
     --------
     -- 1
     -- , foldr (\x xs -> xs ++ [(*10) x]) [] [1,2,3::Int] -- reverse order
     -- 3
     -- , foldl (\xs x -> (*10) x : xs) [] [1,2,3::Int] -- reverse order
     -- 4
     , foldl (\xs x -> xs ++ [(*10) x]) [] [1,2,3::Int]
     ]
     [10,20,30]

------------------------------------------------------------------------------
-- EXERCISE 6

e6 :: [Test]
e6 = U.tt "e6"
     [ filter even [1,2,3::Int]
     --------
     -- 1
     , foldl (\xs x -> if even x then x : xs else xs) [] [1,2,3::Int]
     -- 2
     , foldr (\x xs -> if even x then x : xs else xs) [] [1,2,3::Int]
     -- 3
     -- , foldr (\x xs -> if even x then xs ++ x else [x]) [] [1,2,3::Int]  -- fails typecheck
     -- 4
     -- , foldl (\x xs -> if even x then xs ++ [x] else xs) [] [1,2,3::Int] -- fails type chck
     ]
     [2]

------------------------------------------------------------------------------
-- EXERCISE 7

e71 = foldr (\x y -> 10 * x + y) 0
e72 = foldl (\x y -> x + 10 * y) 0
e73 = foldl (\x y -> 10 * x + y) 0
e74 = foldr (\x y -> x + 10 * y) 0

e7t f = map f [ [2,3,4,5::Int], [], [0,0,0,0] ]

e7 :: [Test]
e7 = U.tt "e7"
     [
--       e7t e71 -- wrong
--       e7t e72 -- wrong
       e7t e73
--       e7t e74 -- reversed
     ]
     [2345,0,0]

------------------------------------------------------------------------------
-- EXERCISE 8

-- does not type check because functions of different types
-- sumSqEven = compose [sum, map (^2), filter even]

compose :: [a -> a] -> (a -> a)
compose = foldr (.) id

------------------------------------------------------------------------------
-- EXERCISE 9

fc :: Show a => (a,b) -> String
fc (x,_) = show x

c93 :: ((a, b) -> c) -> a -> b -> c
c93 f = \x y -> f (x,y)
-- c94 f = \(x,y) -> f x y

e9 :: [Test]
e9 = U.tt "e9"
     [ (curry fc) (1::Int) 'c'
     , (c93   fc) (1::Int) 'c'
     ]
     "1"

------------------------------------------------------------------------------
-- EXERCISE 10

uc101 :: (a -> b -> c) -> (a, b) -> c
uc101 f = \(x,y) -> f x y

e10 :: [Test]
e10 = U.tt "e10"
     [ uncurry (+) (1,2::Int)
     , uc101   (+) (1,2::Int)
     ]
     3

------------------------------------------------------------------------------
-- EXERCISE 11

-- ***** TODO

e11 :: [Test]
e11 = U.tt "e11"
     [ ()
     ]
     ()

------------------------------------------------------------------------------
-- EXERCISE 12

-- ***** TODO

e12 :: [Test]
e12 = U.tt "e12"
     [ ()
     ]
     ()

------------------------------------------------------------------------------
-- EXERCISE 13

-- ***** TODO

e13 :: [Test]
e13 = U.tt "e13"
     [ ()
     ]
     ()

------------------------------------------------------------------------------
-- EXERCISE 14

-- associativity

------------------------------------------------------------------------------
-- EXERCISE 15

-- 4th choice is bad

------------------------------------------------------------------------------
-- EXERCISE 16

e16 :: [Test]
e16 = U.tt "e16"
     [
     -- 1
{-
       (==)
       ((filter even . map (*10)) [1,2,3,4::Int])
       ((map (*10) . filter even) [1,2,3,4::Int])
     -- 2
     , (==)
       (filter        even  [1,2,3,4::Int])
       (filter (not . even) [1,2,3,4::Int])
-}
     -- 3
       (==)
       ((filter even . filter even) [1,2,3,4::Int])
       ( filter even                [1,2,3,4::Int])
     ]
     True

------------------------------------------------------------------------------
-- EXERCISE 17

-- reverse (map f xs) = map f (reverse xs)

------------------------------------------------------------------------------
-- EXERCISE 18

-- reverse (xs ++ ys) = reverse ys ++ reverse xs

------------------------------------------------------------------------------
-- EXERCISE 19

-- produces a finite list
-- take 10 [1 ..]

------------------------------------------------------------------------------
-- EXERCISE 20

-- sum is NOT a higher-order function

------------------------------------------------------------------------------
-- EXERCISE 21

-- map is NOT an overloaded function
-- I got this wrong - I was thinking of fmap
-- so I chose that map is not a function with two arguments
-- which I think is a correct choice - all haskell functions take ONE argument

------------------------------------------------------------------------------
-- EXERCISE 22

-- foldr is NOT an overloaded function

------------------------------------------------------------------------------
-- EXERCISE 23

-- take is a polymorphic function
-- I got this wrong - I said length is a curried function
-- I think I'm getting the definition of curry mixed up

------------------------------------------------------------------------------
-- EXERCISE 24

-- f x = x > 3 is overloaded
-- I got this wrong - I said f = \x -> x

------------------------------------------------------------------------------
-- EXERCISE 25

-- take 4 (iterate (+1) 1) == [1,2,3,4]

------------------------------------------------------------------------------
-- EXERCISE 26

-- takeWhile even [2, 4, 5, 6, 7, 8] == [2,4]

------------------------------------------------------------------------------
-- EXERCISE 27

-- zip [1, 2] ['a', 'b', 'c'] == [(1,'a'),(2,'b')]

------------------------------------------------------------------------------
-- EXERCISE 28

-- foldr (-) 0 [1, 2, 3, 4] == (-2)

------------------------------------------------------------------------------
-- EXERCISE 29

-- filter even (map (+1) [1..5]) == [2,4,6]

------------------------------------------------------------------------------
-- EXERCISE 30

e30 :: [Test]
e30 = U.tt "e30"
     [ filter even (map (+1) [1,2,3,4,5::Int])
     ----------------
     -- 4
     , [(+1) x|x <- [1,2,3,4,5::Int], even ((+1) x)]
     ]
     [2,4,6]

------------------------------------------------------------------------------
-- EXERCISE 31

-- ***** TODO

e31 :: [Test]
e31 = U.tt "e31"
     [ ()
     ]
     ()

------------------------------------------------------------------------------

main :: IO Counts
main =
    T.runTestTT $ T.TestList $ e0 ++ e1 ++ e2 ++ e3 ++ e4 ++ e5 ++ e6 ++ e7 ++ e9 ++
                              e10 ++e11 ++e12 ++e13 ++            e16 ++
                              e30 ++e31

-- End of file.
