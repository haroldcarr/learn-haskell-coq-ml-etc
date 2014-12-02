{-
Created       : 2014 Dec 01 (Mon) 19:11:55 by Harold Carr.
Last Modified : 2014 Dec 01 (Mon) 19:37:21 by Harold Carr.
-}

import           Test.HUnit      as T
import           Test.HUnit.Util as U

-- HOMEWORK 11

------------------------------------------------------------------------------
-- EXERCISE 6

fibs = 0 : 1 : [x + y | (x,y) <- zip fibs (tail fibs)]

e6 :: [Test]
e6 = U.t "e6"
     (take 10 fibs)
     [0,1,1,2,3,5,8,13,21,34]

------------------------------------------------------------------------------
-- EXERCISE 7

fib0 n = last (take n fibs)
fib1 n = head (drop (n - 1) fibs)
fib2 n = fibs !! n
-- fib3 n = index fibs n

e7 :: [Test]
e7 = U.t "e7"
     (fib2 10)
     55

------------------------------------------------------------------------------
-- EXERCISE 8

largeFib = head (dropWhile (<= 1000) fibs)

e8 :: [Test]
e8 = U.t "e8"
     largeFib
     1597

------------------------------------------------------------------------------
-- EXERCISE 9

data Tree a = Leaf
            | Node (Tree a) a (Tree a)
            deriving (Eq, Show)

repeatTree :: a -> Tree a
repeatTree x = Node t x t
  where
    t = repeatTree x

takeTree :: Int -> Tree a -> a
takeTree _ Leaf = error "no way"
takeTree 0 (Node _ x _) = x
takeTree n (Node l _ _) = takeTree (n - 1) l

e9 :: [Test]
e9 = U.t "e9"
     (takeTree 100 (repeatTree 3))
     3

------------------------------------------------------------------------------

main :: IO Counts
main =
    T.runTestTT $ T.TestList $ e6 ++ e7 ++ e8 ++ e9

-- End of file.


