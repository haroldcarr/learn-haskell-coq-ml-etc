{-
Created       : 2014 Oct 26 (Sun) 09:22:44 by Harold Carr.
Last Modified : 2014 Oct 26 (Sun) 09:22:44 by Harold Carr.
-}

import           Prelude         hiding ((||))

import           Test.HUnit      as T
import           Test.HUnit.Util as U

-- HOMEWORK 2

------------------------------------------------------------------------------
-- EXERCISE 0

h2,h3,h6,h8 :: [a] -> ([a], [a])
{-
h1 xs = (take n xs, drop n xs)
  where n = length xs / 2 --     No instance for (Fractional Int) arising from a use of `/'
-}
h2 xs = splitAt (length xs `div` 2) xs

h3 xs = (take (n `div` 2) xs, drop (n `div` 2) xs)
  where n = length xs
{-
h4 xs = splitAt (length xs `div` 2) -- Couldn't match expected type `([a], [a])'

=> ([1,2,3],[5,6])
h5 xs = (take n xs, drop (n + 1) xs)
  where n = length xs `div` 2
-}
h6 xs = splitAt (div (length xs) 2) xs
{-
-- No instance for (Fractional Int) arising from a use of `/'
h7 xs = splitAt (length xs / 2) xs
-}
h8 xs = (take n xs, drop n xs)
  where n = length xs `div` 2

e0 :: [Test]
e0 = U.tt "e0"
     [ h2 [1,2,3,4,5,6::Int]
     , h3 [1,2,3,4,5,6]
     , h6 [1,2,3,4,5,6]
     , h8 [1,2,3,4,5,6]
     ]
     ([1,2,3],[4,5,6])

------------------------------------------------------------------------------
-- EXERCISE 1

t1 :: [a] -> [a]
t1 xs = if null xs then [] else tail xs

-- t2, ...

e1a :: [Test]
e1a = U.tt "e1a"
     [ t1 [1,2::Int]
     ]
     [2]

e1b :: [Test]
e1b = U.tt "e1b"
     [ t1 []::[Int]
     ]
     []

------------------------------------------------------------------------------
-- EXERCISE 2 - not shown

------------------------------------------------------------------------------
-- EXERCISE 3 - not shown

------------------------------------------------------------------------------
-- EXERCISE 4 - not shown

------------------------------------------------------------------------------
-- EXERCISE 5 - not shown

------------------------------------------------------------------------------
-- EXERCISE 6 - not shown

------------------------------------------------------------------------------
-- EXERCISE 7

r1,r2,r3,r4 :: Int -> [a] -> [a]
r1 n xs = take n xs ++ drop n xs
r2 n xs = drop n xs ++ take n xs
r3 n xs = take (n + 1) xs ++ drop n xs
r4 n xs = take n xs ++ drop (n + 1) xs

-- t2, ...

e7a :: [Test]
e7a = U.tt "e7a"
     [
--       r1 0 [1,2::Int]
--     , r2 0 [1,2::Int]
--      r3 0 [1,2::Int]
       r4 0 [1,2::Int]
     ]
     [2]

e7b :: [Test]
e7b = U.tt "e7b"
     [
--     r1 1 [1,2::Int]
--     , r2 1 [1,2::Int]
--     , r3 1 [1,2::Int]
       r4 1 [1,2::Int]
     ]
     [1]

e7c :: [Test]
e7c = U.tt "e7c"
     [ r1 10 [1,2::Int]
     , r2 10 [1,2::Int]
     , r3 10 [1,2::Int]
     , r4 10 [1,2::Int]
     ]
     [1,2]

------------------------------------------------------------------------------
-- EXERCISE 8

funct :: Int -> [a] -> [a]
funct x xs = take (x + 1) xs ++ drop x xs

e8 :: [Test]
e8 = U.t "e8"
     (funct 3 [1, 2, 3, 4, 5, 6, 7::Int])
     [1,2,3,4,4,5,6,7]

------------------------------------------------------------------------------

main :: IO Counts
main =
    T.runTestTT $ T.TestList $ e0 ++ e1a ++ e1b ++
                               e7a ++ e7b ++ e7c ++
                               e8

-- End of file.


