{-
Created       : 2014 Jun 19 (Thu) 17:22:43 by Harold Carr.
Last Modified : 2014 Jun 19 (Thu) 17:24:07 by Harold Carr.
-}

module HW11_HC where

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

hw11 :: IO T.Counts
hw11 = do

    T.runTestTT ex1
    T.runTestTT ex2
    T.runTestTT ex3

-- End of file.
