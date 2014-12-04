{-
Created       : 2014 Dec 03 (Wed) 22:20:06 by Harold Carr.
Last Modified : 2014 Dec 03 (Wed) 22:48:21 by Harold Carr.
-}

-- HOMEWORK 13

import           Test.HUnit      as T
import           Test.HUnit.Util as U

foldl_0 :: (b -> a -> b) -> b -> [a] -> b
foldl_0 f a bs = foldr (\b -> \g -> (\a -> g (f a b))) id bs a

foldl_1 :: (b -> a -> b) -> b -> [a] -> b
foldl_1 f a bs = foldr (\a b -> f b a) a bs

foldl_2 :: (b -> a -> b) -> b -> [a] -> b
foldl_2 f      = flip $ foldr (\a b g -> b (f g a)) id

foldl_3 :: (b -> a -> b) -> b -> [a] -> b
foldl_3        = foldr . flip

f :: (Integer, String) -> Integer -> (Integer, String)
f (al, ar) b = (b + al, (show b) ++ ar)

q :: [Test]
q = U.tt "q"
     [ foldl   f (0, "") [1..5] -- 0
     , foldl_0 f (0, "") [1..5] -- 1
     , foldl_1 f (0, "") [1..5] -- 2
     , foldl_2 f (0, "") [1..5] -- 3
     , foldl_3 f (0, "") [1..5] -- 4
     ]
     (15,"54321")

------------------------------------------------------------------------------

main :: IO Counts
main =
    T.runTestTT $ T.TestList $ q
