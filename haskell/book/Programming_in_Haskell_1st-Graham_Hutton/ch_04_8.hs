{-
Created       : 2015 Apr 19 (Sun) 09:49:24 by Harold Carr.
Last Modified : 2015 Apr 19 (Sun) 10:52:36 by Harold Carr.
-}

import           Test.HUnit      as T
import           Test.HUnit.Util as U

-- 4.8 Exercises

------------------------------------------------------------------------------
-- 1

halve :: [a] -> ([a],[a])
halve xs
    | n `mod` 2 /= 0 = error "uneven list"
    | otherwise      = (take n2 xs, drop n2 xs)
  where
    n  = length xs
    n2 = n `div` 2

e1 :: [Test]
e1 = U.t "e1"
     (halve [2,3,4,5,6,7])
     ([2,3,4],[5,6,7])

------------------------------------------------------------------------------
-- 2

safetailA, safetailB, safetailC :: Show a => [a] -> [a]

safetailA xs = if null xs then [] else tail xs
safetailB xs | null xs   = []
             | otherwise = tail xs
safetailC xs = case xs of
                   [] -> []
                   _  -> tail xs

e2a1 :: [Test]
e2a1 = U.t "e2a1"
     (safetailA []::[Int])
     []
e2a2 :: [Test]
e2a2 = U.t "e2a2"
     (safetailA [1,2])
     [2]

e2b1 :: [Test]
e2b1 = U.t "e2b1"
     (safetailB []::[Int])
     []
e2b2 :: [Test]
e2b2 = U.t "e2b2"
     (safetailB [1,2])
     [2]

e2c1 :: [Test]
e2c1 = U.t "e2c1"
     (safetailC []::[Int])
     []
e2c2 :: [Test]
e2c2 = U.t "e2c2"
     (safetailC [1,2])
     [2]

------------------------------------------------------------------------------
-- 3

or1,or2,or3,or4 :: Bool -> Bool -> Bool

or1 False False = False
or1 False True  = True
or1 True  False = True
or1 True  True  = True

or2 True  _     = True
or2 _     True  = True
or2 _     _     = False

or3 False b     = b
or3 a     False = a
or3 _     _     = True

or4 a     b | a == b = b
            | a == False = b
            | b == False = a

e3False :: [Test]
e3False = U.tt "e3False"
     [ or1 False False
     , or2 False False
     , or3 False False
     , or4 False False
     ]
     False

e3True :: [Test]
e3True = U.tt "e3True"
     [ or1 False True
     , or1 True  False
     , or1 True  True
     , or2 False True
     , or2 True  False
     , or2 True  True
     , or3 False True
     , or3 True  False
     , or3 True  True
     , or4 False True
     , or4 True  False
     , or4 True  True
     ]
     True

------------------------------------------------------------------------------
-- 4

andCond :: Bool -> Bool -> Bool
andCond a b = if a == True then b else False

e4False :: [Test]
e4False = U.tt "e4False"
     [ andCond False False
     , andCond False True
     , andCond True  False
     ]
     False

e4True :: [Test]
e4True = U.t "e4True"
     (andCond True True)
     True

------------------------------------------------------------------------------
-- 5

andCond5 :: Bool -> Bool -> Bool
andCond5 a b = if a == True then
                   if b == True then True
                   else False
               else False

e5False :: [Test]
e5False = U.tt "e5False"
     [ andCond5 False False
     , andCond5 False True
     , andCond5 True  False
     ]
     False

e5True :: [Test]
e5True = U.t "e5True"
     (andCond5 True True)
     True

------------------------------------------------------------------------------
-- 6

mult :: Num a => a -> a -> a -> a
mult = \x -> \y -> \z -> x * y * z

e6 :: [Test]
e6 = U.t "e6"
     (mult 2 3 4::Int)
     24

------------------------------------------------------------------------------

main :: IO Counts
main =
    T.runTestTT $ T.TestList $ e1 ++
                               e2a1 ++ e2a2 ++ e2b1 ++ e2b2 ++ e2c1 ++ e2c2 ++
                               e3False ++ e3True ++
                               e4False ++ e4True ++
                               e5False ++ e5True ++
                               e6

-- End of file.
