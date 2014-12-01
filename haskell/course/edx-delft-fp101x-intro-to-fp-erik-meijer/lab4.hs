{-
Created       : 2014 Dec 01 (Mon) 12:03:06 by Harold Carr.
Last Modified : 2014 Dec 01 (Mon) 13:10:06 by Harold Carr.
-}

module Lab4 where

import           Data.Char
import           Test.HUnit      as T
import           Test.HUnit.Util as U

------------------------------------------------------------------------------------------------------------------------------
-- RECURSIVE FUNCTIONS
------------------------------------------------------------------------------------------------------------------------------

-- ===================================
-- Ex. 0
-- ===================================

triangle :: Integer -> Integer
triangle 0 = 0
triangle n = n + triangle (n - 1)

e0 :: [Test]
e0 = U.tt "e0"
     [ triangle 500
     , sum [0..500]
     ]
     125250

-- ===================================
-- Ex. 3
-- ===================================

count :: Eq a => a -> [a] -> Int
count  _     [] = 0
count a0 (a:as) = count a0 as + if a0 == a then 1 else 0

xs = [1,2,35,2,3,4,8,2,9,0,5,2,8,4,9,1,9,7,3,9,2,0,5,2,7,6,92,8,3,6,1,9,2,4,8,7,1,2,8,0,4,5,2,3,6,2,3,9,8,4,7,1,4,0,1,8,4,1,2,4,56,7,2,98,3,5,28,4,0,12,4,6,8,1,9,4,8,62,3,71,0,3,8,10,2,4,7,12,9,0,3,47,1,0,23,4,8,1,20,5,7,29,3,5,68,23,5,6,3,4,98,1,0,2,3,8,1]
ys = map (\x -> ((x + 1) * 3) ^ 3 - 7) xs

e3 :: [Test]
e3 = U.t "e3"
     (count 722 ys)
     14

poem = [ "Three Types for the Lisp-kings under the parentheses,"
       , "Seven for the Web-lords in their halls of XML,"
       , "Nine for C Developers doomed to segfault,"
       , "One for the Dark Lord on his dark throne"
       , "In the Land of Haskell where the Monads lie."
       , "One Type to rule them all, One Type to find them,"
       , "One Type to bring them all and in the Lambda >>= them"
       , "In the Land of Haskell where the Monads lie."
       ]


e4 :: [Test]
e4 = U.t "e4"
     (count 101 (poem >>= \x -> map (ord . \x -> chr (ord x + 4)) x))
     16

-- ===================================
-- Ex. 5
-- ===================================

euclid :: (Int, Int) -> Int
euclid (x, y) | x == y    = x
              | x <  y    = euclid (y-x, x)
              | otherwise = euclid (x-y, y)

e5 = U.t "e5"
     (euclid (13404, 8832))
     12

-- ===================================
-- Ex. 7
-- ===================================

funkyMap :: (a -> b) -> (a -> b) -> [a] -> [b]
funkyMap f g xs0 = fm 0 xs0
  where
    fm _ [] = []
    fm n (x:xs) | even n    = f x : fm (n+1) xs
                | otherwise = g x : fm (n+1) xs

-- funkyMap (+10) (+100) [1, 2, 3, 4, 5] == [(+10) 1, (+100) 2, (+10) 3, (+100) 4, (+10) 5]

e7 = U.t "e7"
     (sum $ funkyMap (+10) (+100) ys)
     112319712

e7b = U.t "e7b"
      (sum $ funkyMap (\c -> if c == 'e' then 1 else 0) ord (poem >>= id))
      16805

-- ===================================
-- Ex. 12
-- ===================================

frid :: (b -> ([(b -> b)] -> b))
frid = foldr id

-- ===================================
-- Ex. 13
-- ===================================

ffrc :: (a -> (c -> (b -> c)) -> c -> (b -> c)) -> [a] -> c -> (b -> c)
ffrc = flip foldr const

-- ===================================
-- Ex. 14
-- ===================================

-- dup :: t -> (t, t)
dup a = (a, a)

-- td :: a -> (((a, a), (a, a)), ((a, a), (a, a)))
td :: (a) -> ((((a), (a)), ((a), (a))), (((a), (a)), ((a), (a))))
td = dup . dup . dup

-- ===================================
-- Ex. 15
-- ===================================

-- h :: ((b -> c) -> b) -> (b -> c) -> c
h :: ((a -> b) -> a) -> ((a -> b) -> b)
h g f = (f . g) $ f

-- ===================================
-- Ex. 16
-- ===================================

fix :: (a -> a) -> a
fix = h fix

-- ===================================
-- Ex. 18
-- ===================================

-- f :: (Integer -> Integer) -> Integer -> Integer
f :: (Eq a, Num a) => (a -> a) -> a -> a
f = \f n -> if (n == 0) then 1 else n * f (n - 1)

-- ===================================
-- Ex. 19
-- ===================================

k :: (Eq a, Num a) => a -> a
k = fix $ f

e19 = U.tt "e19"
     [ (k 42)
     , 1405006117752879898543142606244511569936384000000000
     ]
     1405006117752879898543142606244511569936384000000000

------------------------------------------------------------------------------

main :: IO Counts
main =
    T.runTestTT $ T.TestList $ e0 ++ e3 ++ e4 ++ e5 ++ e7 ++ e7b ++ e19


