{-
Created       : Jun 14 (Sat) 19:50:18 by Harold Carr.
Last Modified : 2014 Jun 15 (Sun) 17:45:12 by Harold Carr.
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
fibs1 = [ fib n | n <- [0 .. ] ]

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
    show a = "Stream" ++ init (show $ take 20 $ streamToList a) ++ " .. ]"

es :: Stream Integer
es = (S 1 es)

ex3 :: T.Test
ex3 = T.TestList
    [
      U.teq "e30" (show es) "Stream[1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1 .. ]"
    ]

------------------------------------------------------------------------------
-- Exercise 4

streamRepeat :: a -> Stream a
streamRepeat a = s where s = (S a s)

-- TODO : do not use explicit recursion
streamMap :: (a -> b) -> Stream a -> Stream b
streamMap f s = go (streamToList s)
  where
    go (x:xs) = (S (f x) (go xs))

streamFromSeed :: (a -> a) -> a -> Stream a
streamFromSeed f a0 = (S a0 (go a0))
  where
    go a = (S (f a) (go (f a)))

ex4 :: T.Test
ex4 = T.TestList
    [
      -- TODO: fix show of Stream
      U.teq "e30" (show (streamRepeat   'a'))            "Stream\"aaaaaaaaaaaaaaaaaaaa .. ]"
    , U.teq "e31" (show (streamMap      (+1) es))        "Stream[2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2 .. ]"
    , U.teq "e32" (show (streamFromSeed (+1) (1::Int)))  "Stream[1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20 .. ]"
    ]

------------------------------------------------------------------------------
-- Exercise 5

nats :: Stream Integer
nats = streamFromSeed (+1) 0

interleaveStreams :: Stream a -> Stream a -> Stream a
interleaveStreams s10 s20 = go (streamToList s10) (streamToList s20)
  where
    go (l:ls) (r:rs) = (S l (S r (go ls rs)))

-- 2/2^1; 4/2^2; 6/2^1; 8/2^3; 10/2^1; 12/2^2; 14/2^1; 16/2^4
-- http://books.google.com/books?id=ZZMjbAYwGPcC&pg=PA291&lpg=PA291&dq=%22ruler+function%22&source=bl&ots=d4Z2vz1okS&sig=AsLZ7x66QyD5wkvd5HX1ttYEusw&hl=en&sa=X&ei=ODqeU6T_IJKyyATM3oC4Cg&ved=0CBwQ6AEwADgo#v=onepage&q=%22ruler%20function%22&f=false

ruler :: Stream Integer
ruler = interleaveStreams (streamFromSeed (id) 0) (streamMap rul (streamFromSeed (+2) 2))
  where
    rul :: Integer -> Integer
    rul n | odd n     = 0
          | otherwise = let k = n `div` 2 in rul k + 1

ex5 :: T.Test
ex5 = T.TestList
    [
      U.teq "e50" (show nats)                        "Stream[0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19 .. ]"
    , U.teq "e51" (show (interleaveStreams nats es)) "Stream[0,1,1,1,2,1,3,1,4,1,5,1,6,1,7,1,8,1,9,1 .. ]"
    , U.teq "e52" (show ruler)                       "Stream[0,1,0,2,0,1,0,3,0,1,0,2,0,1,0,4,0,1,0,2 .. ]"
    ]

------------------------------------------------------------------------------
-- Exercise 6 - TODO

ex6 :: T.Test
ex6 = T.TestList
    [
    ]

------------------------------------------------------------------------------
-- Exercise 7 - TODO

ex7 :: T.Test
ex7 = T.TestList
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
    T.runTestTT ex7

-- End of file.
