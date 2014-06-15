{-
Created       : Jun 14 (Sat) 19:50:18 by Harold Carr.
Last Modified : 2014 Jun 14 (Sat) 21:23:00 by Harold Carr.
-}

module HW06_HC where

import           Data.List       (genericIndex, unfoldr)

import qualified Test.HUnit      as T
import qualified Test.HUnit.Util as U

------------------------------------------------------------------------------
-- Exercise 1

fib :: Integer -> Integer
fib 0 = 0
fib 1 = 1
fib n = fib (n - 1) + fib (n - 2)

fibs1 :: [Integer]
fibs1 = [ fib n | n <- [0 ..] ]

ex1 :: T.Test
ex1 = T.TestList
    [
      U.teq "e10" (take 15 fibs1)  [0,1,1,2,3,5,8,13,21,34,55,89,144,233,377]
    ]

------------------------------------------------------------------------------
-- Exercise 2

fibs2 :: [Integer]
fibs2 = [0,1] ++ unfoldr (\(x1:x2:[]) -> Just (x1+x2, [x2,x1+x2])) [0,1]

fib2 :: Integer -> Integer
fib2 n = fibs2 `genericIndex` n

ex2 :: T.Test
ex2 = T.TestList
    [
      U.teq "e20" (take 15 fibs1)       (take 15 fibs2)
    , U.teq "e21" (map fib [25,26,27])  (map fib2 [25,26,27])
    ]

------------------------------------------------------------------------------
-- Exercise 3

data Stream a = S a (Stream a)

streamToList :: Stream a -> [a]
streamToList (S h t) = h : streamToList t

instance Show a => Show (Stream a) where
    show (S h0 (S h1 (S h2 (S h3 (S h4 (S h5 (S h6 (S h7 (S h8 (S h9 (S h10 (S h11 (S h12 (S h13 (S h14 (S h15 _)))))))))))))))) =
        "Stream" ++ concatMap (\x -> " " ++ show x) [h0,h1,h2,h3,h4,h5,h6,h7,h8,h9,h10,h11,h12,h13,h14,h15] ++ " .. "

s :: Stream Integer
s = (S 1 s)

ex3 :: T.Test
ex3 = T.TestList
    [
      U.teq "e30" (show s) "Stream 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 .. "
    ]

------------------------------------------------------------------------------
-- Exercise 4

ex4 :: T.Test
ex4 = T.TestList
    [
    ]

------------------------------------------------------------------------------
-- Exercise 5

ex5 :: T.Test
ex5 = T.TestList
    [
    ]

------------------------------------------------------------------------------
-- Exercise 6

ex6 :: T.Test
ex6 = T.TestList
    [
    ]

------------------------------------------------------------------------------

hw06 :: IO T.Counts
hw06 = do
    T.runTestTT ex1
    T.runTestTT ex2
    T.runTestTT ex3
    T.runTestTT ex4
    T.runTestTT ex5
    T.runTestTT ex6

-- End of file.
