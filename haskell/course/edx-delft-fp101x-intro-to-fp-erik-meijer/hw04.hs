{-
Created       : 2014 Oct 26 (Sun) 09:22:44 by Harold Carr.
Last Modified : 2014 Nov 02 (Sun) 21:50:27 by Harold Carr.
-}

import           Data.Char

import           Test.HUnit      as T
import           Test.HUnit.Util as U

-- HOMEWORK 4

------------------------------------------------------------------------------
-- EXERCISE 0


e0 :: [Test]
e0 = U.tt "e0"
     [ -- sum           [[x * x]    | x <- [1 .. 100]]
       sum           [ x ^ 2     | x <- [1 .. 100::Int]]
     -- , sum           [ const 2 x | x <- [1 .. 100]]
     -- , foldl (+) (1) [ x ^ 2     | x <- [1 .. 100]]
     ]
     338350

------------------------------------------------------------------------------
-- EXERCISE 1

r4 :: Int -> a -> [a]
-- r1 n a = [True | _ <- [1 .. n]] -- doesn't type check
-- r2 n a = [   n | _ <- [1 .. n]] -- ditto
-- r3 n a = [   a | _ <- [1 .. a]] -- ditto
r4 n a = [   a | _ <- [1 .. n]]

e1 :: [Test]
e1 = U.tt "e1"
     [ r4 3 True
     ]
     [True,True,True]

------------------------------------------------------------------------------
-- EXERCISE 2

p3 :: Int -> [(Int,Int,Int)]
-- p1 n = [(x,y,z) | x <- [1 .. n], y <- [1 .. x], z <- [1 .. y],
--        x^2 + y^2 == z^2]
-- p2 n = [(x,y,z) | x <- [1..n], y <- [x..n], z <- [y..n],
--         x^2 + y^2 == z^2]
p3 n = [(x,y,z) | x <- [1..n], y <- [1..n], z <- [1..n],
        x^2 + y^2 == z^2]
-- p4 n = [(x,y, (x^2 + y^2)) | x <- [1..n], y <- [1..n]]

e2 :: [Test]
e2 = U.tt "e2"
     [ -- p1 10
     -- p2 10
     p3 10
--     , p4 10
     ]
     [(3, 4, 5), (4, 3, 5), (6, 8, 10), (8, 6, 10)]


------------------------------------------------------------------------------
-- EXERCISE 3

factors :: Int -> [Int]
factors n = [x | x <- [1 .. n], n `mod` x == 0]

pr2 :: Int -> [Int]

-- pr1 n = [ x | x <- [1 .. n], isPerfect x]
--   where isPerfect num = sum (factors num) == num

pr2 n = [ x | x <- [1 .. n], isPerfect x]
  where isPerfect num = sum (init (factors num)) == num

-- pr3 n = [ isPerfect x | x <- [1 .. n]]
--   where isPerfect num = sum (init (factors num)) == num

-- pr4 n = [ x | x <- [1 .. n], isPerfect x]
--  where isPerfect num = init (factors num) == num

e3 :: [Test]
e3 = U.tt "e3"
     [
--        pr1 500
       pr2 500
--     , pr4 500
     ]
     [6, 28, 496]

------------------------------------------------------------------------------
-- EXERCISE 4

e4 :: [Test]
e4 = U.tt "e4"
     [ [ (x,y) | x <- [1,2,3], y <- [4,5,6]]
--     , [ z | z <- [[(x,y) | y<- [4,5,6]] | x <- [1,2,3]]]
--     , concat [[[(x,y)] | x <- [1,2,3]] |  y <- [4,5,6]]
--     , concat [(x,y) | y <- [4,5,6]] |  x <- [1,2,3]
     , concat [[(x,y) | y <- [4,5,6]] |  x <- [1,2,3]]
     ]
     [(1,4),(1,5),(1,6),(2,4),(2,5),(2,6),(3,4),(3,5),(3,6)]

------------------------------------------------------------------------------
-- EXERCISE 5

pol,po1 :: Eq a => a -> [a] -> [Int]

pol x xs = [ i | (x', i) <- zip xs [0 .. n], x == x']
  where n = length xs - 1

find :: Eq a => a -> [(a,b)] -> [b]
find k t = [v | (k', v) <- t, k == k']

po1 x xs = find x (zip xs [0 .. n])
  where n = length xs - 1

po2 :: Eq a => a -> [(a, b)] -> [b]
po2 x xs = find x xs

-- po3 x xs = find x (zipWith (+) xs [0 .. n])
--  where n = length xs -1

po4 x xs = find n (zip xs [0 .. n])
  where n = length xs -1

e5 :: [Test]
e5 = U.tt "e5"
     [ pol 0 [1,0,0,1,0,1,1,0]
     , po1 0 [1,0,0,1,0,1,1,0]
--     , p2 0 [1,0,0,1,0,1,1,0]
--     , po4 0 [1,0,0,1,0,1,1,0]
     ]
     [1,2,4,7]

------------------------------------------------------------------------------
-- EXERCISE 6

sp2 :: [Int] -> [Int] -> Int

-- sp1 xs ys = sum [ x * y | x <- xs, y <- ys]
sp2 xs ys = sum [ x * y | (x,y) <- xs `zip` ys ]

e6 :: [Test]
e6 = U.tt "e6"
     [
     -- sp1 [1,2,3] [4,5,6]
     sp2 [1,2,3] [4,5,6]
     ]
     32
------------------------------------------------------------------------------
-- EXERCISE 7

let2int :: Char -> Int
let2int c = ord c - ord 'a'

int2let :: Int -> Char
int2let n = chr (ord 'a' + n)

shift :: Int -> Char -> Char
shift n c
    | isLower c = int2let ((let2int c + n) `mod` 26)
    | isUpper c = toUpper (int2let ((let2int (toLower c) + n) `mod` 26))
    | otherwise = c

encode :: Int -> String -> String
encode n xs = [shift n x | x <- xs]

e7 :: [Test]
e7 = U.t "e7"
     (encode 13
      "Think like a Fundamentalist Code like a Hacker")
     "Guvax yvxr n Shaqnzragnyvfg Pbqr yvxr n Unpxre"

------------------------------------------------------------------------------
-- EXERCISE 8

e8 :: [Test]
e8 = U.t "e8"
     [(x,y) | x <- [1,2], y <- [1,2]]
     [(1,1),(1,2),(2,1),(2,2)]


-- 9: [x | x <- [1,2,3], y <- [1 .. x]]

-- 10: sum [x | x <- [1 .. 10], even x]

-- 11: 1 : [x + 1 | x <- [1,2,3, ...]]

------------------------------------------------------------------------------
-- EXERCISE 12

ri2 :: [a] -> [a] -> [a]

-- ri1 xs ys = concat [[x,y] | x <- xs, y <- ys]
ri2 xs ys = concat [[x,y] | (x,y) <- xs `zip` ys]

e12 :: [Test]
e12 = U.tt "e12"
     [
       ri2 [1,2,3] [4,5,6]
     ]
     [1,4,2,5,3,6]

------------------------------------------------------------------------------
-- EXERCISE 13

divides :: Int -> Int -> Bool
divides x y = x `mod` y == 0

div1 :: Int -> [Int]

div1 x = [d | d <- [1 .. x], divides x d]
div2 x = [d | d <- [1 .. x], divides d x]
div3 x = [d | d <- [2 .. x], divides x d]
--div4 x = [d | d <- [1 .. x], divides x d]

e13 :: [Test]
e13 = U.tt "e13"
     [
       div1 15
--     , div2 15
--     , div3 15
     ]
     [1,3,5,15]

------------------------------------------------------------------------------

main :: IO Counts
main =
    T.runTestTT $ T.TestList $ e0 ++ e1 ++ e2 ++ e3 ++ e4 ++
                               e5 ++ e6 ++ e7 ++ e8 ++
                               e12 ++ e13

-- End of file.


