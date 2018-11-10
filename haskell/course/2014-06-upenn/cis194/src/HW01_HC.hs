{-
Created       : 2014 May 23 (Fri) 14:32:50 by Harold Carr.
Last Modified : 2014 Sep 17 (Wed) 10:08:02 by Harold Carr.
-}

module HW01_HC where

import           Data.List       (unfoldr)
import qualified Test.HUnit      as T
import qualified Test.HUnit.Util as U

------------------------------------------------------------------------------
-- Exercise 1

lastDigit :: Integer -> Integer
lastDigit = (`mod` 10)

dropLastDigit :: Integer -> Integer
dropLastDigit = (`div` 10)

e1 :: T.Test
e1 = T.TestList
    [
      U.teq "123last" (lastDigit     123)   3
    , U.teq "0last"   (lastDigit     0)     0
    , U.teq "123drop" (dropLastDigit 123)  12
    , U.teq "5drop"   (dropLastDigit 5)     0
      ---------------------------------------
    , U.teq "397last" (lastDigit     397)   7
    , U.teq "397drop" (dropLastDigit 397)  39
    , U.teq "39last"  (lastDigit     39)    9
    , U.teq "39drop"  (dropLastDigit 39)    3
    , U.teq "3last"   (lastDigit     3)     3
    , U.teq "3drop"   (dropLastDigit 3)     0
    ]

------------------------------------------------------------------------------
-- Exercise 2

toDigits :: Integer -> [Integer]
toDigits = reverse . toDigitsRev

toDigitsRev :: Integer -> [Integer]
toDigitsRev = unfoldr (\n -> if n <= 0 then Nothing else Just (lastDigit n, dropLastDigit n))

e2 :: T.Test
e2 = T.TestList
    [
      U.teq "7" (toDigits    1234) [1,2,3,4]
    , U.teq "9" (toDigitsRev 1234) [4,3,2,1]
    , U.teq "9" (toDigits       0) []
    , U.teq "0" (toDigits   (-17)) []
    ]

------------------------------------------------------------------------------
-- Exercise 3

-- O(n)
-- correct type sig is:
-- doubleEveryOther :: Num a => [a] -> [a]
-- but pinning it at Integer to avoid "defaulting to" messages in test
doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther = snd . foldr (\n (b,xs) -> (not b, (if b then 2*n else n) : xs)) (False, [])

e3 :: T.Test
e3 = T.TestList
    [
      U.teq "deo8765"   (doubleEveryOther   [8,7,6,5]) [16,7,12,5]
    , U.teq "deo123"    (doubleEveryOther     [1,2,3])     [1,4,3]
      ----------------------
    , U.teq "deo1386"   (doubleEveryOther   [1,3,8,6])  [2,3,16,6]
    ]

-- http://stackoverflow.com/questions/23842473/what-to-use-instead-of-explicit-recursion-in-haskell
-- doubleEveryOtherLens = over (elements even) (*2) [8,7,6,5]

------------------------------------------------------------------------------
-- Exercise 4

sumDigits :: [Integer] -> Integer
sumDigits = foldr (\x acc -> dropLastDigit x + lastDigit x + acc) 0

e4 :: T.Test
e4 = T.TestList
    [
      U.teq "0" (sumDigits [16,7,12,5]) 22
    , U.teq "1" (sumDigits [2,3,16,6])  18
    ]

------------------------------------------------------------------------------
-- Exercise 5

validate :: Integer -> Bool
validate = (== 0) . (`mod` 10) . sumDigits . doubleEveryOther . toDigits

e5 :: T.Test
e5 = T.TestList
    [
      U.teq "vt" (validate 4012888888881881) True
    , U.teq "vf" (validate 4012888888881882) False
    ]

------------------------------------------------------------------------------
-- Exercise 6 - Towers of Hanoi

{-
0
-
--
---

1
--
---         -

2
---   --    -

3
      -
---   --

4
      -
      --    ---

5
-     --    ---

6
            --
-           ---

7
            -
            --
            ---
-}

type Peg = String
type Move = (Integer, Peg, Peg)
--                  src    tmp    dst
hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi n a b c =
    case n of
        0 -> []
        _ -> hanoi (n - 1) a c b ++ [(n, a, c)] ++ hanoi (n - 1) b a c

hN :: Integer -> [Move]
hN n = hanoi n "a" "b" "c"

checkLastMove :: [Move] -> Bool
checkLastMove m = case last m of
                      (1, _ ,"c") -> True
                      _           -> False

h3, h4, h5, h9, h15 :: [Move]
h3  = hN  3
h4  = hN  4
h5  = hN  5
h9  = hN  9
h15 = hN 15

-- to avoid "defaulting to" messages
twoToNMinusOne :: Int -> Int
twoToNMinusOne n= 2^n-1

e6 :: T.Test
e6 = T.TestList
    [
      U.teq "h3"                  h3   [(1,"a","c"),(2,"a","b"),(1,"c","b"),(3,"a","c"),(1,"b","a"),(2,"b","c"),(1,"a","c")]
    , U.teq "h3m"  (checkLastMove h3)  True
    , U.teq "h3l"         (length h3)  (twoToNMinusOne 3)
    , U.teq "h4"                  h4   [(1,"a","b"),(2,"a","c"),(1,"b","c"),(3,"a","b"),(1,"c","a"),(2,"c","b"),(1,"a","b"),(4,"a","c"),(1,"b","c"),(2,"b","a"),(1,"c","a"),(3,"b","c"),(1,"a","b"),(2,"a","c"),(1,"b","c")]
    , U.teq "h4m"  (checkLastMove h4)  True
    , U.teq "h4l"         (length h4)  (twoToNMinusOne 4)
    , U.teq "h5"                  h5   [(1,"a","c"),(2,"a","b"),(1,"c","b"),(3,"a","c"),(1,"b","a"),(2,"b","c"),(1,"a","c"),(4,"a","b"),(1,"c","b"),(2,"c","a"),(1,"b","a"),(3,"c","b"),(1,"a","c"),(2,"a","b"),(1,"c","b"),(5,"a","c"),(1,"b","a"),(2,"b","c"),(1,"a","c"),(3,"b","a"),(1,"c","b"),(2,"c","a"),(1,"b","a"),(4,"b","c"),(1,"a","c"),(2,"a","b"),(1,"c","b"),(3,"a","c"),(1,"b","a"),(2,"b","c"),(1,"a","c")]
    , U.teq "h5m"  (checkLastMove h5)  True
    , U.teq "h5l"         (length h5)  (twoToNMinusOne 5)
    , U.teq "h9m"  (checkLastMove h9)  True
    , U.teq "h9l"         (length h9)  (twoToNMinusOne 9)
    , U.teq "h15m" (checkLastMove h15) True
    , U.teq "h15l"        (length h15) (twoToNMinusOne 15)
    ]

------------------------------------------------------------------------------
-- Exercise 7
-- TODO
hanoi4p :: Integer -> Peg -> Peg -> Peg -> Peg -> [Move]
hanoi4p = undefined
-- hanoi4p 3 "a" "b" "c" "d"

------------------------------------------------------------------------------
hw01 :: IO [T.Counts]
hw01 = mapM T.runTestTT [e1,e2,e3,e4,e5,e6]

-- End of file.



