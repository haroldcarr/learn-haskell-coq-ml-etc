{-
Created       : 2014 Oct 26 (Sun) 09:22:44 by Harold Carr.
Last Modified : 2014 Oct 26 (Sun) 09:22:44 by Harold Carr.
-}

import           Data.List       (sort)

import           Test.HUnit      as T
import           Test.HUnit.Util as U

-- HOMEWORK 1

------------------------------------------------------------------------------
-- EXERCISE 0 - not shown

------------------------------------------------------------------------------
-- EXERCISE 1

{-
N = a 'div' length xs
   where
      a = 10
     xs = [1, 2, 3, 4, 5]
-}

n :: Int
n = a `div` length xs
  where
      a = 10
      xs = [1, 2, 3, 4, 5::Int]

------------------------------------------------------------------------------
-- EXERCISE 2

l2, l5, l7 :: [a] -> a
-- l1 xs = drop (length xs - 1) xs
l2 xs = head (drop (length xs - 1) xs)
-- l3 xs = tail (reverse xs)
-- l4 xs = reverse (head xs)
l5 xs = xs !! (length xs - 1)
-- l6 xs = head (drop (length xs) xs) -- *** Exception: Prelude.head: empty list
l7 xs = head (reverse xs)
-- l8 xs = reverse xs !! (length xs - 1) -- => 1

e2 :: [Test]
e2 = U.tt "e2"
     [ last [1,2,3::Int]
     , l2   [1,2,3]
     , l5   [1,2,3]
     , l7   [1,2,3]
     ]
     3

------------------------------------------------------------------------------
-- EXERCISE 3

i5 :: [a] -> [a]
-- i1 xs = tail (reverse xs) -- => [2,1]
-- i2 xs = reverse (head (reverse xs)) -- Couldn't match type `a' with `[a]'
-- i3 xs = reverse (tail xs) -- => [3,2]
-- i4 xs = take (length xs) xs -- => [1,2,3]
i5 xs = reverse (tail (reverse xs))
-- i6 xs = take (length xs -1) (tail xs) -- => [2,3]
-- i7 xs = drop (length xs -1) xs -- => [3]

e3 :: [Test]
e3 = U.tt "e3"
     [ init [1,2,3::Int]
     , i5   [1,2,3]
     ]
     [1,2]

------------------------------------------------------------------------------
-- EXERCISE 4 - not shown

------------------------------------------------------------------------------
-- EXERCISE 5 - not shown

------------------------------------------------------------------------------
-- EXERCISE 6 - not shown

------------------------------------------------------------------------------
-- EXERCISE 7

q1,q5,q6,q7 :: Ord a => [a] -> [a]

q1 [] = []
q1 (x:xs) = q1 larger ++ [x] ++ q1 smaller
  where smaller = [a | a <- xs, a <= x]
        larger = [b | b <- xs, b > x]
{-
-- [9,1,8,3,7]
q2 [] = []
q2 (x:xs) = reverse (q2 smaller ++ [x] ++ q2 larger)
  where smaller = [a | a <- xs, a <= x]
        larger = [b | b <- xs, b > x]

-- infinite loop : because nothing is ever smaller than minimum
q3 [] = []
q3 xs = q3 larger ++ q3 smaller ++ [x]
  where x = minimum xs
        smaller = [a | a <- xs, a <= x]
        larger = [b | b <- xs, b > x]

-- [7,3,8,1,9]
q4 [] = []
q4 (x:xs)
  = reverse (q4 smaller) ++ [x] ++ reverse (q4 larger)
  where smaller = [a | a <- xs, a <= x]
        larger = [b | b <- xs, b > x]
-}

q5 [] = []
q5 (x:xs) = q5 larger ++ [x] ++ q5 smaller
  where larger = [a | a <- xs, a > x || a == x]
        smaller = [b | b <- xs, b < x]

-- this works on my sample input, but not on all inputs
q6 [] = []
q6 (x:xs) = q6 larger ++ [x] ++ q6 smaller
  where smaller = [a | a <- xs, a < x]
        larger = [b | b <- xs, b > x]

q7 [] = []
q7 (x:xs)
  = reverse
      (reverse (q7 smaller) ++ [x] ++ reverse (q7 larger))
  where smaller = [a | a <- xs, a <= x]
        larger = [b | b <- xs, b > x]
{-
-- infinite loop
q8 [] = []
q8 xs = x : q8 larger ++ q8 smaller
  where x = maximum xs
        smaller = [a | a <- xs, a < x]
        larger = [b | b <- xs, b >= x]
-}

e7 :: [Test]
e7 = U.tt "e7"
     [ reverse $ sort [9, 1, 8, 3, 7::Int]
     , q1             [9, 1, 8, 3, 7]
     , q5             [9, 1, 8, 3, 7]
     , q6             [9, 1, 8, 3, 7]
     , q7             [9, 1, 8, 3, 7]
     ]
     [9,8,7,3,1]

------------------------------------------------------------------------------
-- EXERCISE 8

{-
replacing <= by < in qsort above
(i,e, q6 compared to q1)

- Duplicate elements are removed from the sorted list.
- Will only work correctly on some inputs
-}

------------------------------------------------------------------------------

main :: IO Counts
main =
    T.runTestTT $ T.TestList $ e2 ++ e3 ++ e7

-- End of file.


