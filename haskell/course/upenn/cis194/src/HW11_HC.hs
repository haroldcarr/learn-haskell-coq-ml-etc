{-
Created       : 2014 Jun 19 (Thu) 17:22:43 by Harold Carr.
Last Modified : 2014 Jun 19 (Thu) 18:01:23 by Harold Carr.
-}

module HW11_HC where

import           Control.Applicative

import qualified Test.HUnit          as T
import qualified Test.HUnit.Util     as U

------------------------------------------------------------------------------
-- Examples from lecture

data Employee = Emp { name :: String, phone :: String } deriving (Eq, Show)

-- nondeterministic arithmetic
(.+), (.*) :: [Integer] -> [Integer] -> [Integer]
(.+) = liftA2 (+)    -- addition lifted to some Applicative context
(.*) = liftA2 (*)    -- same for multiplication

lec :: T.Test
lec = T.TestList
    [
      -- example of non-deterministic list applicative
      U.teq "l0" (Emp <$> ["A", "B"] <*> ["1", "2"])   [Emp "A" "1",Emp "A" "2",Emp "B" "1",Emp "B" "2"]

      -- (either 4 or 5) times 2, plus either 6 or 1
    , U.teq "l1" (([4,5] .* pure 2) .+ [6,1])      [14,9,16,11::Integer]

      -- possibly-failing arithmetic
--    , U.teq "l2" ((Just (3::Integer) .+ Just 5)  .* Just 8) []
--    , U.teq "l3" ((Just 3 .+ Nothing) .* Just 8) Nothing
    ]

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
    T.runTestTT lec
    T.runTestTT ex1
    T.runTestTT ex2
    T.runTestTT ex3

-- End of file.
