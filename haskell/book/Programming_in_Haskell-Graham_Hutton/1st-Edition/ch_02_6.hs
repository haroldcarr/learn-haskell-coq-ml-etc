{-
Created       : 2015 Apr 19 (Sun) 05:04:20 by Harold Carr.
Last Modified : 2015 Apr 19 (Sun) 05:48:42 by Harold Carr.
-}

import           Test.HUnit      as T
import           Test.HUnit.Util as U

-- 2.6 Exercises

------------------------------------------------------------------------------
-- 1

{-
:i (^)
(^) :: (Num a, Integral b) => a -> b -> a 	-- Defined in `GHC.Real'
infixr 8 ^

:i (*)
class Num a where
  ...
  (*) :: a -> a -> a
  ...
  	-- Defined in `GHC.Num'
infixl 7 *

:i (+)
class Num a where
  (+) :: a -> a -> a
  ...
  	-- Defined in `GHC.Num'
infixl 6 +
-}

e1_1 :: [Test]
e1_1 = U.tt "e1_1"
     [ 2^3*4
     , (2^3)*4
     ]
     32

e1_2 :: [Test]
e1_2 = U.tt "e1_2"
     [ 2*3+4*5
     , (2*3)+(4*5)
     ]
     26

e1_3 :: [Test]
e1_3 = U.tt "e1_3"
     [ 2+3*4^5
     , 2+(3*(4^5))
     ]
     3074

------------------------------------------------------------------------------
-- 2 : no

------------------------------------------------------------------------------
-- 3

n :: Int
n = a `div` length xs
  where
    a  = 10
    xs = [1,2,3,4,5]

e3 :: [Test]
e3 = U.t "e3"
     n
     2

------------------------------------------------------------------------------
-- 4

last1 :: [a] -> a
last1 [] = error "empty list"
last1 xs = head $ drop n0 xs
  where n0 = length xs - 1

last2 :: [a] -> a
last2 [] = error "empty list"
last2 xs = xs !! (length xs - 1)

e4 :: [Test]
e4 = U.tt "e4"
     [ last1 [1,2,3,4]
     , last2 [1,2,3,4]
     ]
     4

------------------------------------------------------------------------------
-- 5

init1 :: [a] -> [a]
init1 [] = error "empty list"
init1 xs = take n0 xs
  where n0 = length xs - 1

init2 :: [a] -> [a]
init2 = reverse . tail . reverse

init3 :: [a] -> [a]
init3 [] = error "empty list"
init3 (x:_:[]) = [x]
init3 (x:xs) = x : init2 xs

e5 :: [Test]
e5 = U.tt "e5"
     [ init1 [1,2,3,4]
     , init2 [1,2,3,4]
     , init3 [1,2,3,4]
     ]
     [1,2,3]

------------------------------------------------------------------------------

main :: IO Counts
main =
    T.runTestTT $ T.TestList $ e1_1 ++ e1_2 ++ e1_3 ++ e3 ++ e4 ++ e5

-- End of file.


