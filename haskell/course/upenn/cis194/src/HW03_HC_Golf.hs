{-
Created       : 2014 Jun 01 (Sun) 19:54:26 by Harold Carr.
Last Modified : 2014 Jun 01 (Sun) 21:31:12 by Harold Carr.
-}

module HW03_HC_Golf where

import           Data.List       (init, tails, unfoldr)
import qualified Test.HUnit      as T
import qualified Test.HUnit.Util as U

------------------------------------------------------------------------------
-- Exercise 1

skips :: [a] -> [[a]]
skips xs0 = map f (zip ((init . tails) xs0) [1 ..])
  where
    f x = let (xs, n) = x
              g i | i >= length xs = Nothing
                  | otherwise      = Just (xs!!i, i+n)
          in unfoldr g 0

e1 :: T.Test
e1 = T.TestList
    [
      U.teq "skips0" (skips "ABCD")       ["ABCD", "BD", "C", "D"]
    , U.teq "skips1" (skips "hello!")     ["hello!", "el!", "l!", "l", "o", "!"]
    , U.teq "skips2" (skips [1])          [[1]]
    , U.teq "skips3" (skips [True,False]) [[True,False], [False]]
--  TODO: skips [] returns the correct value when typed into GHCi after loading this file,
--        but need to figure out how to get it to accept the following input in test
--    , U.teq "skips4" (skips [])           []
    ]

------------------------------------------------------------------------------
hw03 :: IO T.Counts
hw03 = do
    T.runTestTT e1

-- End of file.
