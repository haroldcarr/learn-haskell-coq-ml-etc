{-
Created       : 2014 Jun 05 (Thu) 20:29:15 by Harold Carr.
Last Modified : 2014 Jun 07 (Sat) 11:22:30 by Harold Carr.
-}

module HW04_HC where

import qualified Test.HUnit      as T
import qualified Test.HUnit.Util as U

------------------------------------------------------------------------------
-- Exercise 1

-- 1

fun1 :: [Integer] -> Integer
fun1 [] = 1
fun1 (x:xs)
    | even x    = (x - 2) * fun1 xs
    | otherwise =           fun1 xs

fun1' :: [Integer] -> Integer
fun1' = foldr (\x acc -> (if even x then (x - 2) else 1) * acc) 1

-- 2

fun2 :: Integer -> Integer
fun2 1 = 0
fun2 n
    | even n    = n + fun2 (n `div` 2)
    | otherwise =     fun2 (3 * n + 1)

-- TODO : use takeWhile and interate
fun2' :: Integer -> Integer
fun2' 1 = 0
-- fun2' n = takeWhile (even n)

e1 :: T.Test
e1 = T.TestList
    [
      -- if a 2 is anywhere in the list result is 0
      U.teq "fun10"    (fun1  [1::Integer, 2 .. 10])      0
    , U.teq "fun1'0"   (fun1' [1::Integer, 2 .. 10])      0
    , U.teq "fun11"    (fun1  [1::Integer, 3 .. 11])      1
    , U.teq "fun1'1"   (fun1' [1::Integer, 3 .. 11])      1
    , U.teq "fun148"   (fun1     [4::Integer, 6, 8]) (2*4*6)
    , U.teq "fun1'48"  (fun1'    [4::Integer, 6, 8]) (2*4*6)
    , U.teq "fun148'"  (fun1   [3::Integer, 4 .. 8]) (2*4*6)
    , U.teq "fun1'48'" (fun1'  [3::Integer, 4 .. 8]) (2*4*6)

    , U.teq "fun2" (map fun2 [1::Integer, 2 .. 10]) [0,2,40,6,30,46,234,14,276,40]
    ]

------------------------------------------------------------------------------
-- Exercise 2

-- TODO

------------------------------------------------------------------------------
-- Exercise 3

-- 1

xor :: [Bool] -> Bool
xor = odd . foldr (\x acc -> (if x then (+1) else (*1)) acc) (0::Integer)

-- 2

map' :: (a -> b) -> [a] -> [b]
map' f = foldr (\x acc -> f x : acc) []

-- 3

-- BEGIN for stepping and watching reductions
myId :: x -> x
myId x = x

myFoldr            :: (a -> b -> b) -> b -> [a] -> b
myFoldr _ z []     =  z
myFoldr f z (x:xs) =  f x (myFoldr f z xs)

myPlus :: Integer -> Integer -> Integer
myPlus x y = x + y
-- END for stepping and watching reductions

myFoldl :: (a -> b -> a) -> a -> [b] -> a
myFoldl stepL zeroL xs = (myFoldr stepR id xs) zeroL
  where stepR lastL accR accInitL = accR (stepL accInitL lastL)

foo :: Integer -> (Integer -> t) -> Integer -> t
foo = \lastL accR accInitL -> accR (myPlus accInitL lastL)

e3 :: T.Test
e3 = T.TestList
    [
      U.teq "xor0" (xor                                []) False
    , U.teq "xor1" (xor              [False, True, False]) True
    , U.teq "xor2" (xor [False, True, False, False, True]) False

    , U.teq "map0" (map' (*2)      [1::Integer ..9])
                   (map  (*2)      [1::Integer ..9])
    , U.teq "map1" (map' fun2 [1::Integer, 2 .. 10])
                   (map  fun2 [1::Integer, 2 .. 10])

    , U.teq "fll0" (foldl   (\x a -> x ++ a) [] ["foo","bar"])
                   (myFoldl (\x a -> x ++ a) [] ["foo","bar"])
    , U.teq "fll1" (foldl   (\x a -> x ++ a) [] [[1::Integer],[2],[3]])
                   (myFoldl (\x a -> x ++ a) [] [[1::Integer],[2],[3]])
    , U.teq "fll1" (foldl   myPlus           0  [1::Integer,2,3])
                   (myFoldl myPlus           0  [1::Integer,2,3])
    ]

mf :: [T.Test]
mf = U.tt "mf"
  [myFoldl myPlus                                                                                                                       0 [1,2]
  ,                                                                                                                    (myFoldr foo myId  [1,2]) 0
  ,foo                                                    1                                                            (myFoldr foo myId    [2]) 0
  ,foo                                                    1 (foo                                                     2 (myFoldr foo myId    [])) 0
  ,foo                                                    1 (foo                                                     2              myId)        0
  ,foo                                                    1 ((\lastL accR accInitL  -> accR (myPlus accInitL lastL)) 2              myId)        0
  ,foo                                                    1             (\accInitL  -> myId (myPlus accInitL         2))                         0
  ,(\lastL accR accInitL -> accR (myPlus accInitL lastL)) 1             (\accInitL' -> myId (myPlus accInitL'        2))                         0
  ,      (\accR accInitL -> accR (myPlus accInitL 1))                   (\accInitL' -> myId (myPlus accInitL'        2))                         0
  ,           (\accInitL -> (\accInitL' -> myId (myPlus accInitL' 2)) (myPlus accInitL 1))                                                       0
  ,                         (\accInitL' -> myId (myPlus accInitL' 2)) (myPlus 0        1)
  ,                                        myId (myPlus (myPlus 0 1) 2)
  ,                                        myId 3
  ]
  3

------------------------------------------------------------------------------

hw04 :: IO T.Counts
hw04 = do
    _ <- T.runTestTT e1
    _ <- T.runTestTT e3
    T.runTestTT $ T.TestList $ mf

-- End of file.
