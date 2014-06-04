{-
Created       : 2014 Jun 01 (Sun) 19:54:26 by Harold Carr.
Last Modified : 2014 Jun 03 (Tue) 22:35:07 by Harold Carr.
-}

module HW03_HC_Golf where

import           Data.List       (foldr, intercalate, tails, unfoldr)
import           Data.List.Split (chop)
import           Data.Map        as M (Map, empty, fromList, insertWith, lookup,
                                       update)
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
  where f (x1:t@(x2:x3:_)) = (if x1 < x2 && x2 > x3 then [x2] else [], t)
        f _                = ([], [])

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
-- Exercise 3

histogram :: [Integer] -> String
histogram xs = [ f r c | r <- [9, 8 .. 1], c <- [0 .. 10] ] ++ "==========\n0123456789"
  where
    m = foldr (\k acc -> M.insertWith (+) k 1 acc) M.empty xs
    f _ 10 = '\n'
    f i  j = case M.lookup j m of
                (Just n) -> if n >= i then '*' else ' '
                Nothing  -> ' '

e3 :: T.Test
e3 = T.TestList
    [
      U.teq "h0" (histogram [1,1,1,5])
                 "          \n          \n          \n          \n          \n          \n *        \n *        \n *   *    \n==========\n0123456789"
    , U.teq "h1" (histogram [1,4,5,4,6,6,3,4,2,4,9])
                 "          \n          \n          \n          \n          \n    *     \n    *     \n    * *   \n ******  *\n==========\n0123456789"
    ]

------------------------------------------------------------------------------

hw03 :: IO T.Counts
hw03 = do
    _ <- T.runTestTT e1
    _ <- T.runTestTT e2
    T.runTestTT e3

-- End of file.
