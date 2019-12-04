{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}

module TestDay01 where

import           Control.Lens
import           Control.Lens.Action (act, (^!))
import qualified Prelude
import           Protolude           hiding (to)
import           Test.HUnit          ((@?=))
import           Test.Tasty          hiding (Timeout)
import           Test.Tasty.HUnit    (testCase)

-- https://chrispenner.ca/posts/advent-of-optics-01
-- https://adventofcode.com/2019/day/1

------------------------------------------------------------------------------
-- Part 1

-- series of input numbers (mass of ship modules)
-- pass through pipeline of mathematic operations (fuel calculations)
-- summed together to get solution (total fuel required)

-- (note: could solve without lens via foldMap)

-- ^.. : infix and flipped toListOf

test_worded :: TestTree
test_worded  = testCase "test_day01_worded" $ do
  input <- Prelude.readFile "test/01.txt"
  take 4 (input^..worded) @?= ["50572","126330","143503","136703"]

-- parse strings into a numeric via '_Show"
-- uses Read instances to parse strings
-- skips elements which fail to parse
-- _Show :: (Read a, Show a) => Prism' String a

test_Show :: TestTree
test_Show  = testCase "test_day01_Show" $ do
  input <- Prelude.readFile "test/01.txt"
  take 4 (input ^.. worded . _Show @ Double) @?= [50572.0,126330.0,143503.0,136703.0]

-- steps:
-- Divide by 3
-- Round down
-- Subtract 2

test_steps :: TestTree
test_steps  = testCase "test_day01_steps" $ do
  input <- Prelude.readFile "test/01.txt"
  take 4 (input
          ^.. worded
            . _Show
            . to (/ 3)
            . to (floor @Double @Int)
            . to (subtract 2)) @?= [16855,42108,47832,45565]

-- refactor
calculateRequiredFuel :: Double -> Double
calculateRequiredFuel = fromIntegral . subtract 2 . floor @Double @Int . (/ 3)

test_CalculateRequiredFuel :: TestTree
test_CalculateRequiredFuel  =
  testGroup "test_day01_CalculateRequiredFuel"
  [ testCase "    12" $ calculateRequiredFuel     12.0 @?=     2.0
  , testCase "    14" $ calculateRequiredFuel     14.0 @?=     2.0
  , testCase "  1969" $ calculateRequiredFuel   1969.0 @?=   654.0
  , testCase "100756" $ calculateRequiredFuel 100756.0 @?= 33583.0
  ]

-- sum the stream

-- change aggregation from ^.. (a.k.a. toListOf) into sumOf

test_sumOf :: TestTree
test_sumOf  = testCase "test_day01_sumOf" $ do
  input <- Prelude.readFile "test/01.txt"
  (input & sumOf (worded . _Show . to calculateRequiredFuel )) @?= 3412531

-- alternative
-- computed entire thing in a fold by using lens-action to thread the readFile into IO
-- ^! : 'view' a result from a Fold which requires IO
-- act : lift a monadic action into a fold
-- viewing implicitly folds the output using it's Monoid (e.g., Sum)

solve' :: IO (Sum Double)
solve'  = "test/01.txt" ^! act Prelude.readFile . worded . _Show . to calculateRequiredFuel . to Sum

test_lens_action :: TestTree
test_lens_action  = testCase "test_day01_lens_action" $ do
  r <- solve'
  r @?= Sum {getSum = 3412531}
{-
------------------------------------------------------------------------------
Part 2

need to account for fuel required to transport all the fuel added (i.e., fuel itself requires fuel)

that fuel also requires fuel, and that fuel requires fuel, ...

Any mass that would require negative fuel should instead be treated as if it requires zero fuel

for each module mass
- calculate its fuel and add it to the total
- Then, treat the fuel just calculated as the input mass and repeat the process
- continuing until a fuel requirement is zero or negative.

For example:

mass 14 requires 2 fuel
- 2 fuel requires no further fuel because
- 2 divided by 3 and rounded down is 0, so the total fuel required is still just 2

mass 1969 requires 654 fuel
- 654 fuel requires 216 more fuel (654 / 3 - 2)
- 216 fuel requires 70 more fuel, ...
- total fuel required for module is 654 + 216 + 70 + 21 + 5 = 966

mass 100756 requires
- 33583 + 11192 + 3728 + 1240 + 411 + 135 + 43 + 12 + 2 = 50346

What is sum of fuel requirements for all modules, taking into account mass of added fuel?

the iteration needed is an unfold

lens : unfolds can be represented as a Fold that adds more elements when it runs

Lensy folds can focus an arbitrary number of focuses

a fold in lens that can be used: iterated :: (a -> a) -> Fold a a
-}
test_iterated :: TestTree
test_iterated  =
  testGroup "test_day01_iterated"
  [ -- Note: iterated emits first input without any iteration
    testCase "chris" $
    1 ^.. taking 10 (iterated (+1)) @?=
    [1,2,3,4,5,6,7,8,9,10::Int]

  , testCase "  1969 listOf" $
    1969 ^.. taking 7 (iterated calculateRequiredFuel) @?=
    [1969,654,216,70,21,5,-1]
  , testCase "  1969  sumOf" $
    (1969 & sumOf (takingWhile (>0) (dropping 1 (iterated calculateRequiredFuel)))) @?=
    966.0

  , testCase "100756 listOf" $
    100756 ^.. taking 12 (iterated calculateRequiredFuel) @?=
    [100756.0,33583.0,11192.0,3728.0,1240.0,411.0,135.0,43.0,12.0,2.0,-2.0,-3.0]
  , testCase "100756  sumOf" $
    (100756 & sumOf (takingWhile (>0) (dropping 1 (iterated calculateRequiredFuel)))) @?=
    50346.0
  ]

-- next
-- limit iteration while developing
-- switch back to toListOf so can observe what is happening
-- output shows fuel numbers getting smaller on each iteration, until they go negative
-- after 30 iterations fold moves onto the next input
--   so the numbers jump back up again as a new iteration starts
solve2 :: IO [Double]
solve2 =  do
  input <- Prelude.readFile "test/01.txt"
  pure $ input & toListOf (worded . _Show . taking 30 (iterated calculateRequiredFuel) )

test_solve2 :: TestTree
test_solve2  = testCase "test_day01_solve2" $ solve2 >>= \r -> take (length e) r @?= e
  where e = [50572.0,16855.0,5616.0,1870.0,621.0,205.0,66.0,20.0,4.0,-1.0,-3.0,-3.0,-3.0,-3.0,-3.0,-3.0,-3.0,-3.0,-3.0,-3.0,-3.0,-3.0,-3.0,-3.0,-3.0,-3.0,-3.0,-3.0,-3.0,-3.0,126330.0,42108.0,14034.0,4676.0,1556.0,516.0,170.0,54.0,16.0,3.0,-1.0,-3.0,-3.0,-3.0,-3.0,-3.0,-3.0,-3.0,-3.0,-3.0,-3.0,-3.0,-3.0,-3.0,-3.0,-3.0,-3.0,-3.0,-3.0,-3.0,143503.0,47832.0]

-- puzzle says to ignore everything past the point where numbers go negative
-- so stop iterating at that point via takingWhile
-- note: 0 is filtered, but since 0 has no effect on a sum it is OK
solve2' :: IO [Double]
solve2' =  do
  input <- Prelude.readFile "test/01.txt"
  pure $ input & toListOf (worded . _Show . takingWhile (>0) (iterated calculateRequiredFuel) )

test_solve2' :: TestTree
test_solve2'  = testCase "test_day01_solve2'" $ solve2' >>= \r -> take (length e) r @?= e
 -- 'e' is different since there is no longer a "20 limiter", instead stops at 0 or below
 where e = [50572.0,16855.0,5616.0,1870.0,621.0,205.0,66.0,20.0,4.0,126330.0,42108.0,14034.0]

-- recall : iterated passes through the original value, which is NOT wanted in this case.
-- remove it via 'dropping'
solve2'' :: IO Double
solve2'' =  do
  input <- Prelude.readFile "test/01.txt"
  pure $ input &
    sumOf (worded . _Show . takingWhile (>0) (dropping 1 (iterated calculateRequiredFuel)) )

test_solve2'' :: TestTree
test_solve2''  = testCase "test_day01_solve2''" $ solve2'' >>= \r -> r @?= 5115927.0

-- alternative : fewer brackets
solve2FB :: IO Double
solve2FB =  do
  input <- Prelude.readFile "test/01.txt"
  pure $ input &
    sumOf (worded . _Show . (takingWhile (> 0) . dropping 1 . iterated) calculateRequiredFuel)

test_solve2FB :: TestTree
test_solve2FB  = testCase "test_day01_solve2FB" $ solve2FB >>= \r -> r @?= 5115927.0
