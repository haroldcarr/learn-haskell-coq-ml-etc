{-
Created       : 2015 Apr 20 (Mon) 09:20:01 by Harold Carr.
Last Modified : 2015 Apr 20 (Mon) 13:01:58 by Harold Carr.
-}

import           Test.HUnit      as T
import           Test.HUnit.Util as U

-- 6.8 Exercises

------------------------------------------------------------------------------
-- 1

exp1 :: (Num a, Integral b) => a -> b -> a
exp1 n 0  = 1
exp1 n e' = n * exp1 n (e' - 1)

e1 :: [Test]
e1 = U.tt "e1"
     [             exp1 2 3
     ,         2 * exp1 2 2
     ,     2 * 2 * exp1 2 1
     , 2 * 2 * 2 * exp1 2 0
     , 2 * 2 * 2 * 1
     ]
     8

------------------------------------------------------------------------------
-- 2

e2l :: [Test]
e2l = U.tt "e2l"
     [             length [1,2,3]
     ,         1 + length   [2,3]
     ,     1 + 1 + length     [3]
     , 1 + 1 + 1 + length []
     , 1 + 1 + 1 + 0
     ]
     3

e2d :: [Test]
e2d = U.tt "e2d"
     [ drop 3 [1,2,3,4,5]
     , drop 2   [2,3,4,5]
     , drop 1     [3,4,5]
     , drop 0       [4,5]
     ,              [4,5]
     ]
     [4,5]

e2i :: [Test]
e2i = U.tt "e2i"
     [         init [1,2,3]
     , 1 :     init   [2,3]
     , 1 : 2 : init     [3]
     , 1 : 2 : []
     ]
     [1,2]

------------------------------------------------------------------------------
-- 3

and1 :: [Bool] -> Bool
and1 []         = True
and1 (False:xs) = False
and1 (_:xs)     = and1 xs

and2 :: [Bool] -> Bool
and2 = foldr (&&) True

e3at :: [Test]
e3at = U.tt "e3at"
     [ and1 []
     , and1 [True, True]
     , and2 []
     , and2 [True, True]
     ]
     True

e3af :: [Test]
e3af = U.tt "e3af"
     [ and1 [False, True]
     , and1 [True, False]
     , and2 [False, True]
     , and2 [True, False]
     ]
     False

concat1 :: [[a]] -> [a]
concat1 [] = []
concat1 (x:xs) = x ++ concat xs

concat2 :: [[a]] -> [a]
concat2 = foldr (++) []

e3ce :: [Test]
e3ce = U.tt "e3ce"
     [ concat1 []::[Int]
     , concat2 []::[Int]
     ]
     []

e3c :: [Test]
e3c = U.tt "e3c"
     [ (concat1 [[1],[2,3],[4]])::[Integer]
     , (concat2 [[1],[2,3],[4]])::[Integer]
     ]
     [1,2,3,4]

replicate1 :: Int -> a -> [a]
replicate1 0 a = []
replicate1 n a = a : replicate1 (n - 1) a

replicate2 :: Int -> a -> [a]
replicate2 n a = [ a | _ <- [1 .. n]]

e3re :: [Test]
e3re = U.tt "e3re"
      [ replicate1 0 1
      , replicate2 0 1
      ]
      []

e3r :: [Test]
e3r = U.tt "e3r"
      [ replicate1 3 1
      , replicate2 3 1
      ]
      [1,1,1]

index1 :: [a] -> Int -> a
index1     [] 0 = error "no long enough"
index1 (x:xs) 0 = x
index1 (x:xs) n = index1 xs (n - 1)

e3i :: [Test]
e3i = U.t "e3i"
     (index1 [1,2,3,4] 2)
     3

elem1 :: Eq a => a -> [a] -> Bool
elem1 _ [] = False
elem1 a (x:xs) | a == x    = True
               | otherwise = elem1 a xs

e3ef :: [Test]
e3ef = U.t "e3ef"
      (elem1 'a' "not")
      False

e3et :: [Test]
e3et = U.t "e3et"
      (elem1 'a' "harold")
      True

-----------------------------------------------------------------------------
-- 4

merge :: Ord a => [a] -> [a] -> [a]
merge       []        ys  = ys
merge       xs        []  = xs
merge ax@(x:xs) ay@(y:ys) =
    if x < y
    then x : merge xs ay
    else y : merge ax ys

e4 :: [Test]
e4 = U.t "e4"
     (merge [2,5,6] [1,3,4])
     [1,2,3,4,5,6]

------------------------------------------------------------------------------
-- 5

msort :: Ord a => [a] -> [a]
msort  [] = []
msort [x] = [x]
msort xs0 = merge (msort l1) (msort l2)
  where
    n = length xs0 `div` 2
    halve xs = (take n xs, drop n xs)
    (l1, l2) = halve xs0

e5 :: [Test]
e5 = U.t "e5"
     (msort [30, (-2), 6,  25, (-100)])
     [(-100), (-2), 6, 25, 30]

------------------------------------------------------------------------------
-- 6

sum1 :: Num a => [a] -> a
sum1 [] = 0;
sum1 (x:xs) = x + sum xs
sum2 :: Num a => [a] -> a
sum2 = foldr (+) 0

e6sum = U.tt "e6sum"
     [ sum1 [1,2,3]
     , sum2 [3,2,1]
     ]
     6

take1 :: Int -> [a] -> [a]
take1 _    []  = []
take1 0 (x:_)  = [x]
take1 n (x:xs) = x : take (n - 1) xs

e6take = U.t "e6take"
     (take1 3 [1,2,3,4,5,6])
     [1,2,3]

last1 :: [a] -> a
last1    []  = error "empty list"
last1   [x]  = x
last1 (_:xs) = last1 xs

e6last = U.t "e6last"
     (last1 [1,2,3,4,5,6])
     6

------------------------------------------------------------------------------

main :: IO Counts
main =
    T.runTestTT $ T.TestList $ e1 ++
                               e2l ++ e2d ++ e2i ++
                               e3at ++ e3af ++
                               e3ce ++ e3c ++
                               e3re ++ e3r ++
                               e3i ++
                               e3ef ++ e3et ++
                               e4 ++
                               e5 ++
                               e6sum ++ e6take ++ e6last

-- End of file.
