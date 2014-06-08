{-
Created       : 2014 Jun 05 (Thu) 20:29:15 by Harold Carr.
Last Modified : 2014 Jun 08 (Sun) 12:24:53 by Harold Carr.
-}

module HW04_HC where

import qualified Test.HUnit      as T
import qualified Test.HUnit.Util as U

------------------------------------------------------------------------------
-- Exercise 1

-------------------------
-- 1

fun1 :: [Integer] -> Integer
fun1 [] = 1
fun1 (x:xs)
    | even x    = (x - 2) * fun1 xs
    | otherwise =           fun1 xs

fun1' :: [Integer] -> Integer
fun1' = foldr (\x acc -> (if even x then x - 2 else 1) * acc) 1

fun1'' :: [Integer] -> Integer
fun1'' [] = 1
fun1'' xs = product $ map (\x -> x - 2) $ filter even xs

e1fun1 :: T.Test
e1fun1 = T.TestList
    [
      -- if a 2 is anywhere in the list result is 0
      U.teq "fun10"    (fun1   [1::Integer, 2 .. 10])      0
    , U.teq "fun1'0"   (fun1'  [1::Integer, 2 .. 10])      0
    , U.teq "fun11"    (fun1   [1::Integer, 3 .. 11])      1
    , U.teq "fun1'1"   (fun1'  [1::Integer, 3 .. 11])      1
    , U.teq "fun1''1"  (fun1'' [1::Integer, 3 .. 11])      1
    , U.teq "fun148"   (fun1      [4::Integer, 6, 8]) (2*4*6)
    , U.teq "fun1'48"  (fun1'     [4::Integer, 6, 8]) (2*4*6)
    , U.teq "fun148'"  (fun1    [3::Integer, 4 .. 8]) (2*4*6)
    , U.teq "fun1'48'" (fun1'   [3::Integer, 4 .. 8]) (2*4*6)
    ]

-------------------------
-- 2

fun2 :: Integer -> Integer
fun2 n
    | n == 1    = 0
    | even n    = n + fun2 (n `div` 2)
    | otherwise =     fun2 (3 * n + 1)

-- use takeWhile and interate
-- iterate :: (a -> a) -> a -> [a]
-- iterate f x =  x : iterate f (f x)

fun2' :: Integer -> Integer
fun2' n0 = sum (map fst (takeWhile (\(n,_) -> n /= 1)
                                   (iterate (\(_, n) -> if n == 1
                                                        then (1, 1)
                                                        else if even n
                                                             then (n, n `div` 2)
                                                             else (0, 3 * n + 1))
                                            (0, n0))))

e1fun2 :: T.Test
e1fun2 = T.TestList
    [
      U.teq "fun2"   (map fun2  [1::Integer, 2 .. 10]) [0,2,40,6,30,46,234,14,276,40]
    , U.teq "fun2'"  (map fun2' [1::Integer, 2 .. 10]) [0,2,40,6,30,46,234,14,276,40]
    , U.teq "fun2eq" (map fun2  [1::Integer, 2 .. 1000])
                     (map fun2' [1::Integer, 2 .. 1000])
    ]

------------------------------------------------------------------------------
-- Exercise 2

-- TODO

------------------------------------------------------------------------------
-- Exercise 3

-------------------------
-- 1

xor :: [Bool] -> Bool
xor = odd . foldr (\x acc -> (if x then (+1) else (*1)) acc) (0::Integer)

xor' :: [Bool] -> Bool
xor' = odd . length. filter id

e3xor :: T.Test
e3xor = T.TestList
    [
      U.teq "xor0"  (xor                                 []) False
    , U.teq "xor'0" (xor'                                []) False
    , U.teq "xor1"  (xor               [False, True, False]) True
    , U.teq "xor'1" (xor'              [False, True, False]) True
    , U.teq "xor2"  (xor  [False, True, False, False, True]) False
    , U.teq "xor'2" (xor' [False, True, False, False, True]) False
    ]

-------------------------
-- 2

map' :: (a -> b) -> [a] -> [b]
map' f = foldr (\x acc -> f x : acc) []

e3map :: T.Test
e3map = T.TestList
    [
      U.teq "map0" (map' (*2)      [1::Integer ..9])
                   (map  (*2)      [1::Integer ..9])
    , U.teq "map1" (map' fun2 [1::Integer, 2 .. 10])
                   (map  fun2 [1::Integer, 2 .. 10])
    ]

-------------------------
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

-- the parens around myFoldr are redundant,
-- but helps pointing out it returns a function
myFoldl :: (a -> b -> a) -> a -> [b] -> a
myFoldl stepL zeroL xs = (myFoldr stepR id xs) zeroL
  where stepR lastL accR accInitL = accR (stepL accInitL lastL)

-- redundant lambda, but helps with intuition
foo :: Integer -> (Integer -> t) -> Integer -> t
foo = \lastL accR accInitL -> accR (myPlus accInitL lastL)

e3foldr :: T.Test
e3foldr = T.TestList
    [
      U.teq "fll0" (foldl   (++)   [] ["foo","bar"])
                   (myFoldl (++)   [] ["foo","bar"])
    , U.teq "fll1" (foldl   (++)   [] [[1::Integer],[2],[3]])
                   (myFoldl (++)   [] [[1::Integer],[2],[3]])
    , U.teq "fll1" (foldl   myPlus  0 [1::Integer,2,3])
                   (myFoldl myPlus  0 [1::Integer,2,3])
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
-- Exercise 4

cartProd :: [a] -> [b] -> [(a, b)]
cartProd xs ys = [(x,y) | x <- xs, y <- ys]

sieveSundaram :: Integer -> [Integer]
sieveSundaram n0 =
    let ns = [1::Integer .. n0]
        cp = cartProd ns ns
        ft = filter (\n -> all (\(i, j) -> i + j + (2*i*j) /= n) cp) ns
    in map (\x -> 2*x + 1) ft

e4 :: T.Test
e4 = T.TestList
    [
      U.teq "cp" (cartProd [1::Integer,2] ['a','b']) [(1,'a'),(1,'b'),(2,'a'),(2,'b')]
    , U.teq "si" (sieveSundaram 100) [3,5,7,11,13,17,19,23,29,31,37,41,43,47,53,59,61,67,71,73,79,83,89,97,101,103,107,109,113,127,131,137,139,149,151,157,163,167,173,179,181,191,193,197,199]
    ]

------------------------------------------------------------------------------

hw04 :: IO T.Counts
hw04 = do
    _ <- T.runTestTT e1fun1
    _ <- T.runTestTT e1fun2
    _ <- T.runTestTT e3xor
    _ <- T.runTestTT e3map
    _ <- T.runTestTT e3foldr
    _ <- T.runTestTT $ T.TestList $ mf
    T.runTestTT e4

-- End of file.
