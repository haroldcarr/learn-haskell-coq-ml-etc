{-
Created       : 2015 Apr 23 (Thu) 11:44:31 by Harold Carr.
Last Modified : 2015 Apr 23 (Thu) 11:44:53 by Harold Carr.
-}

import           Test.HUnit      as T
import           Test.HUnit.Util as U

-- 13.9 Exercises

-- TODO

------------------------------------------------------------------------------
-- 1

double :: Num a => a -> a
double x = x + x

e1 :: [Test]
e1 = U.tt "e1"
     [ double 2::Int
     , 2 + 2
     ]
     4

------------------------------------------------------------------------------
-- 2

e2 :: [Test]
e2 = U.tt "e2"
     [ sum [3::Int]
     , 3 + sum []
     , 3 + 0
     ]
     3

------------------------------------------------------------------------------
-- 3

product' :: Num a => [a] -> a
product' [] = 1
product' (x:xs) = x * product' xs

e3 :: [Test]
e3 = U.tt "e3"
     [ product' [2,3,4]
     , 2 * product' [3,4]
     , 2 * 3 * product' [4]
     , 2 * 3 * 4 * product' []
     , 2 * 3 * 4 * 1
     ]
     24

------------------------------------------------------------------------------
-- 4

qsreverse :: Ord a => [a] -> [a]
qsreverse [] = []
qsreverse (x:xs) =  qsreverse larger ++ [x] ++ qsreverse smaller
  where smaller = [a | a <- xs, a <= x]
        larger  = [b | b <- xs, b > x]

e4 :: [Test]
e4 = U.t "e4"
     (qsreverse [2,2,3,1,1::Int])
     [3,2,2,1,1]

------------------------------------------------------------------------------
-- 5

qsrmdups :: Ord a => [a] -> [a]
qsrmdups [] = []
qsrmdups (x:xs) = qsrmdups smaller ++ [x] ++ qsrmdups larger
  where smaller = [a | a <- xs, a < x]
        larger  = [b | b <- xs, b > x]

e5 :: [Test]
e5 = U.t "e5"
     (qsrmdups [2,2,3,1,1::Int])
     [1,2,3]

------------------------------------------------------------------------------

main :: IO Counts
main =
    T.runTestTT $ T.TestList $ e1 ++ e2 ++ e3 ++ e4 ++ e5

-- End of file.


