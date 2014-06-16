{-
Created       : 2014 Jun 15 (Sun) 17:51:15 by Harold Carr.
Last Modified : 2014 Jun 16 (Mon) 10:12:29 by Harold Carr.
-}

module HW07_HC_Scrabble where

import qualified Test.HUnit      as T
import qualified Test.HUnit.Util as U

------------------------------------------------------------------------------
-- Exercise 1

ex1 :: T.Test
ex1 = T.TestList
    [
    ]

------------------------------------------------------------------------------
-- Exercise 2

ex2 :: T.Test
ex2 = T.TestList
    [
    ]

------------------------------------------------------------------------------
-- Exercise 3

ex3 :: T.Test
ex3 = T.TestList
    [
    ]

------------------------------------------------------------------------------
-- Exercise 4

ex4 :: T.Test
ex4 = T.TestList
    [
    ]

------------------------------------------------------------------------------
-- Exercise 5

ex5 :: T.Test
ex5 = T.TestList
    [
    ]

------------------------------------------------------------------------------
-- Exercise 6 - TODO

ex6 :: T.Test
ex6 = T.TestList
    [
    ]

------------------------------------------------------------------------------
-- Exercise 7 - TODO

ex7 :: T.Test
ex7 = T.TestList
    [
    ]

------------------------------------------------------------------------------

hw07s :: IO T.Counts
hw07s = do
    T.runTestTT ex1
    T.runTestTT ex2
    T.runTestTT ex3
    T.runTestTT ex4
    T.runTestTT ex5
    T.runTestTT ex6
    T.runTestTT ex7

-- End of file.
