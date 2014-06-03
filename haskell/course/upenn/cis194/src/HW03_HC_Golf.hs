{-
Created       : 2014 Jun 01 (Sun) 19:54:26 by Harold Carr.
Last Modified : 2014 Jun 03 (Tue) 16:09:28 by Harold Carr.
-}

module HW03_HC_Golf where

import           Data.List       (tails, unfoldr)
import           Data.List.Split (chop)
import qualified Test.HUnit      as T
import qualified Test.HUnit.Util as U

------------------------------------------------------------------------------
-- Exercise 1

skips :: [a] -> [[a]]
skips xs0 = map f (zip ((init . tails) xs0) [1 ..])
  where
    f (xs, n) = unfoldr g 0
      where
        g i | i >= length xs = Nothing
            | otherwise      = Just (xs!!i, i+n)

e1 :: T.Test
e1 = T.TestList
    [
      U.teq "skips0" (skips "ABCD")       ["ABCD", "BD", "C", "D"]
    , U.teq "skips1" (skips "hello!")     ["hello!", "el!", "l!", "l", "o", "!"]
    , U.teq "skips2" (skips [1::Int])     [[1]]
    , U.teq "skips3" (skips [True,False]) [[True,False], [False]]
--  TODO: skips [] returns the correct value when typed into GHCi after loading this file,
--        but need to figure out how to get it to accept the following input in test
--    , U.teq "skips4" (skips [])           []
    ]

------------------------------------------------------------------------------
-- Exercise 2

localMaxima :: [Integer] -> [Integer]
localMaxima l@(x1:x2:x3:_) | x1 < x2 && x2 > x3 = x2 : lmt
                           | otherwise          =      lmt
  where lmt = localMaxima (tail l)
localMaxima _ = []

lm :: [Integer] -> [Integer]
lm xs0 = concat $ chop f xs0
  where f l@(x1:x2:x3:_) = if x1 < x2 && x2 > x3 then ([x2], tail l) else ([], tail l)
        f _              = ([], [])

e2 :: T.Test
e2 = T.TestList
    [
      U.teq "lm0" (localMaxima [2,9,5,6,1]) [9,6]
    , U.teq "lm0" (lm          [2,9,5,6,1]) [9,6]
    , U.teq "lm1" (localMaxima [2,3,4,1,5]) [4]
    , U.teq "lm1" (lm          [2,3,4,1,5]) [4]
    , U.teq "lm2" (localMaxima [1,2,3,4,5]) []
    , U.teq "lm2" (lm          [1,2,3,4,5]) []
    ]

------------------------------------------------------------------------------
hw03 :: IO T.Counts
hw03 = do
    _ <- T.runTestTT e1
    T.runTestTT e2

-- End of file.
