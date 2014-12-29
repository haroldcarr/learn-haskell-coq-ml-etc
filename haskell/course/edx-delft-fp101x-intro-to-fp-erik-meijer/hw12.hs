{-
Created       : 2014 Dec 27 (Sat) 14:14:39 by Harold Carr.
Last Modified : 2014 Dec 27 (Sat) 14:26:18 by Harold Carr.
-}

import           Test.HUnit      as T
import           Test.HUnit.Util as U

-- HOMEWORK 12

------------------------------------------------------------------------------
-- EXERCISE 0

l0 :: [a] -> a
l0 [x] = x
l0 (_:xs) = last xs

fr0 :: (a -> b -> b) -> b -> [a] -> b
fr0 _ v [] = v
fr0 f v (x : xs) = f x (fr0 f v xs)

i0 :: [a] -> [a]
i0 [_] = []
i0 (x:xs) = x : i0 xs

d0 :: Int -> [a] -> [a]
d0 0 xs = xs
d0 n [] = []
d0 n (_ : xs) = d0 (n-1) xs

a0 :: [a] -> [a] -> [a]
a0 [] ys = ys
a0 (x : xs) ys = x : (a0 xs ys)

fl0 :: (a -> b -> a) -> a -> [b] -> a
fl0 _ v [] = v
fl0 f v (x : xs) = fl0 f (f v x) xs

e0 :: [Test]
e0 = U.t "e0"
     (l0 [1::Int])
     1

------------------------------------------------------------------------------

main :: IO Counts
main =
    T.runTestTT $ T.TestList $ e0

-- End of file.


