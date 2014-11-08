{-
Created       : 2014 Nov 08 (Sat) 14:32:29 by Harold Carr.
Last Modified : 2014 Nov 08 (Sat) 15:25:13 by Harold Carr.
-}

module Lab3 where

import           Test.HUnit      as T
import           Test.HUnit.Util as U

-----------------------------------------------------------------------------------------------------------------------------
-- LIST COMPREHENSIONS
------------------------------------------------------------------------------------------------------------------------------

-- ===================================
-- Ex. 0 - 2
-- ===================================

evens :: [Integer] -> [Integer]
evens xs = [ x | x <- xs, even x]

e0_x,e0_0 :: [Test]
e0_x = U.t "e0_x" (evens [2, 5, 6, 13, 32])            [2, 6, 32]
e0_0 = U.t "e0_0" (sum . evens $ [827305 .. 927104])   43772529500

-- ===================================
-- Ex. 3 - 4
-- ===================================

-- complete the following line with the correct type signature for this function

-- squares :: (Enum t, Num t) => t -> [t]
-- squares :: Num -> Num
squares :: Integer -> [Integer]
-- squares :: a -> [a]
-- squares Integer a => a -> [a]
squares n = [ i*i | i <- [1 .. n]]

sumSquares :: Integer -> Integer
sumSquares n = sum (squares n)

e4 :: [Test]
e4 = U.t "e4" (sumSquares 50) 42925

-- ===================================
-- Ex. 5 - 7
-- ===================================

-- complete the following line with the correct type signature for this function

squares' :: (Enum t, Num t) => t -> t -> [t]
squares' m n = [ i*i | i <- [n+1 .. m+n]]

e5_x1,e5_x2,e5_x3,e5_x4 :: [Test]
e5_x1 = U.t "e5_x1" (squares' 4 2)   [9,16,25,36::Int]
e5_x2 = U.t "e5_x2" (squares' 2 0)   [1,4::Int]
e5_x3 = U.t "e5_x3" (squares' 0 2)   ([]::[Int])
e5_x4 = U.t "e5_x4" (squares' 0 0)   ([]::[Int])

sumSquares' :: Integer -> Integer
sumSquares' x = sum . uncurry squares' $ (x, x)

e5,e6,e7 :: [Test]
e5 = U.t "e5" (sumSquares' 50)             295425
e6 = U.t "e6" (sum $ squares' 10 0::Int)   385
e7 = U.t "e7" (sum $ squares' 0 10)        0

-- ===================================
-- Ex. 8
-- ===================================

coords :: Integer -> Integer -> [(Integer,Integer)]
coords x0 y0 = [ (x,y) | x <- [0 .. x0], y <- [0 .. y0]]

e8_x1,e8_x2,e8 :: [Test]
e8_x1 = U.t "e8_x1" (coords 1 1)   [(0,0), (0,1), (1,0), (1,1)]
e8_x2 = U.t "e8_x2" (coords 1 2)   [(0,0), (0,1), (0,2), (1,0), (1, 1), (1, 2)]
e8    = U.t "e8"    (foldr (-) 0 . map (uncurry (*)) $ coords 5 7) (-60)

-- ===================================
-- Ex. 9 : function application associates to the left
-- ===================================

-- ===================================
-- Ex. 10 : type signatures associate to the right
-- ===================================

-- ff :: a -> f -> b -> g -> Int
ff :: a -> (f -> b -> (g -> Int))
ff a f b g = 3

-- ===================================
-- Ex. 11
-- ===================================

-- tup :: (a, f, b, g, c) -> (a, f, b, g, c)
-- tup :: (a, (f, b), (g, c)) -> (a, (f, b), (g, c))
tup :: ((a, f, b, g, c)) -> ((a, f, b, g, c))
tup (a, f, b, g, c) = (a, f, b, g, c)

-- ===================================
-- Ex. 12
-- ===================================

t12 :: (a, f, b, g, c) -> (a, f, b, g, c)
t12 (a, f, b, g, c) = ((a, f, b, g, c))

------------------------------------------------------------------------------

main :: IO Counts
main =
    T.runTestTT $ T.TestList $ e0_x ++ e0_0 ++
                               e4 ++
                               e5_x1 ++ e5_x2 ++ e5_x3 ++ e5_x4 ++ e5 ++
                               e6 ++
                               e7 ++
                               e8_x1 ++ e8_x2 ++ e8

-- End of file.
