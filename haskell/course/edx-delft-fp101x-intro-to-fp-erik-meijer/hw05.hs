{-
Created       : 2014 Oct 26 (Sun) 09:22:44 by Harold Carr.
Last Modified : 2014 Nov 03 (Mon) 20:20:06 by Harold Carr.
-}

import           Test.HUnit      as T
import           Test.HUnit.Util as U

-- HOMEWORK 5

------------------------------------------------------------------------------
-- EXERCISE 0

-- ex1,ex2,ex3,ex4 :: (Integral b, Num a) => a -> b -> a

ex1 _ 0 = 0
ex1 m n = m * m `ex1` (n - 1)

ex2 _ 0 = 1
ex2 m n = m * m `ex2` (n - 1)

ex3 _ 0 = 1
ex3 m n = m * m `ex3` n - 1

ex4 _ 0 = 1
ex4 m n = n * n `ex4` (m - 1)

ex5 _ 0 = 1
ex5 m n = m * (ex5) m (n - 1)

ex6 _ 0 = 1
ex6 m n = m * m * m `ex4` (n - 2)

ex7 _ 0 = 1
ex7 m n = (m * m) `ex7` (n - 1)

ex8 m 1 = m
ex8 m n = m * m `ex8` (n - 1)

-- emap :: (Integral b, Num a) => (a -> b -> a) -> [a]
emap f = [ (x,y, f x y) | x <- [0 .. 3], y <- [0 .. 3]]

e0 :: [Test]
e0 = U.tt "e0"
     [ emap (^)
--     , emap ex1 -- wrong base case
     , emap ex2
--     , emap ex3 -- infinite loop
--      , emap ex4 -- wrong
     , emap ex5
--      , emap ex6 -- wrong on neg numbers
--     , emap ex7
--     , emap ex8 -- wrong on zero
     ]
     [(0,0,1),(0,1,0),(0,2,0),(0,3,0)
     ,(1,0,1),(1,1,1),(1,2,1),(1,3,1)
     ,(2,0,1),(2,1,2),(2,2,4),(2,3,8)
     ,(3,0,1),(3,1,3),(3,2,9),(3,3,27)
     ]

------------------------------------------------------------------------------
-- EXERCISE 1 -- not shown

------------------------------------------------------------------------------
-- EXERCISE 2 -- not shown

------------------------------------------------------------------------------
-- EXERCISE 3 -- not shown

------------------------------------------------------------------------------
-- EXERCISE 4

a1 [] = True
a1 (b:bs) = b && a1 bs

a2 [] = True
a2 (b:bs)
    | b = and bs
    | otherwise = False

a5 [] = True
a5 (b:bs)
    | b == False = False
    | otherwise = a5 bs

a7 [] = True
a7 (b:bs) = a7 bs && b

tl = [True, True,  True]
fl = [True, False, True]

e4t :: [Test]
e4t = U.tt "e4t"
     [ a1 tl
     , a2 tl
     , a5 tl
     , a7 tl
     ]
     True

e4f :: [Test]
e4f = U.tt "e4f"
     [ a1 fl
     , a2 fl
     , a5 fl
     , a7 fl
     ]
     False

------------------------------------------------------------------------------
-- EXERCISE 5

c1 [] = []
c1 (xs : xss) = xs : c1 xss

c2 [] = []
c2 (xs : xss) = xs ++ c2 xss

cl = [[1],[1,2],[],[1,2,3],[]]

e5 :: [Test]
e5 = U.tt "e5"
     [ concat cl
--      , c1 cl - wrong type
     , c2 cl
     ]
     [1,1,2,1,2,3]

------------------------------------------------------------------------------
-- EXERCISE 6 - not shown

------------------------------------------------------------------------------
-- EXERCISE 7 -- not shown

------------------------------------------------------------------------------
-- EXERCISE 8 -- not show

------------------------------------------------------------------------------
-- EXERCISE 9

m1 [] ys = ys
m1 xs [] = xs
m1 (x:xs) (y:ys) =
    if x <= y then x : m1 xs ys else y  : m1 xs ys

m2 [] ys = ys
m2 xs [] = xs
m2 (x:xs) (y:ys) =
    if x <= y then y : m2 xs (y:ys) else x : m2 (x:xs) ys

m3 [] ys = ys
m3 xs [] = xs
m3 (x:xs) (y:ys) =
    if x <= y then y : m3 (x:xs) ys else x : m3 xs (y:ys)

m4 [] ys = ys
m4 xs [] = xs
m4 (x:xs) (y:ys) =
    if x <= y then x : m4 xs (y:ys) else y : m4 (x:xs) ys

e9 :: [Test]
e9 = U.tt "e9"
     [ -- m1 [2,5,6] [1,3,4] -- loses elements
--       m2 [2,5,6] [1,3,4] -- wrong
--     , m3 [2,5,6] [1,3,4] -- wrong
       m4 [2,5,6] [1,3,4] -- wrong
     ]
     [1,2,3,4,5,6]

------------------------------------------------------------------------------
-- EXERCISE 10

halve :: [a] -> ([a],[a])
halve xs = splitAt (length xs `div` 2) xs

ms1 [] = []
ms1 xs = m4 (ms1 zs) (ms1 ys)
  where (ys,zs) = halve xs

ms2 [] = []
ms2 [x] = [x]
ms2 xs = m4 (ms2 zs) (ms2 ys)
  where (ys,zs) = halve xs

ul = [9,5,1,10,4,2,7,3,8,6]

e10 :: [Test]
e10 = U.tt "e10"
     [ -- ms1 ul -- infinite loop
       ms2 ul
     ]
     [1,2,3,4,5,6,7,8,9,10]

------------------------------------------------------------------------------

main :: IO Counts
main =
    T.runTestTT $ T.TestList $ e0 ++ e4t ++ e4f ++ e5 ++ e9 ++ e10

-- End of file.


