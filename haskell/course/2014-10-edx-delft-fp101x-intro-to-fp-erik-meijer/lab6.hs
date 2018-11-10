{-
Created       : 2014 Dec 27 (Sat) 14:38:17 by Harold Carr.
Last Modified : 2014 Dec 29 (Mon) 00:11:55 by Harold Carr.
-}

module Lab6 where

import           Prelude         as P
import           Test.HUnit      as T
import           Test.HUnit.Util as U

------------------------------------------------------------------------------------------------------------------------------
-- ROSE TREES, FUNCTORS, MONOIDS, FOLDABLES
------------------------------------------------------------------------------------------------------------------------------

data Rose a = a :> [Rose a] deriving (Eq, Show)

-- ===================================
-- Ex. 0-2
-- ===================================

root :: Rose a -> a
root (a :> _) = a

children :: Rose a -> [Rose a]
children (_ :> as) = as

e0a :: [Test]
e0a = U.t "e0a"
     (root (1 :> [2 :> [], 3 :> []]))
     1

e0b:: [Test]
e0b = U.t "e0b"
     (root ('a' :> []))
     'a'

e0c:: [Test]
e0c = U.t "e0c"
      (children (1 :> [2 :> [], 3 :> []]))
      [2 :> [], 3 :> []]

e0d:: [Test]
e0d = U.t "e0d"
      (children ('a' :> []))
      []

e03tree = 'x' :> map (flip (:>) []) ['a'..'x']

e0e:: [Test]
e0e = U.t "e0e"
      (length $ children e03tree)
      24

e1tree = 'x' :> map (\c -> c :> []) ['a'..'A']

e1 :: [Test]
e1 = U.t "e1"
     (length (children e1tree))
     0

xs = 0 :> [1 :> [2 :> [3 :> [4 :> [], 5 :> []]]], 6 :> [], 7 :> [8 :> [9 :> [10 :> []], 11 :> []], 12 :> [13 :> []]]]

ex2 = root . head . children . head . children . head . drop 2 $ children xs

e2 :: [Test]
e2 = U.t "e2"
     ex2
     9

-- ===================================
-- Ex. 3-7
-- ===================================

size :: Rose a -> Int
size (_ :> []) = 1
size (_ :> (x:xs)) = 1 + size x + (sum (map size xs))

leaves :: Rose a -> Int
leaves (_ :> []) = 1
leaves (_ :> (x:[])) = leaves x
leaves (a :> (x:xs)) = leaves x + (leaves (a :> xs))

e3tree = 1 :> map (\c -> c :> []) [1..5]
e3 :: [Test]
e3 = U.t "e3"
     (size e3tree)
     6

e4tree = 1 :> map (\c -> c :> []) [1..5]
e4 :: [Test]
e4 = U.t "e4"
     (size . head . children $ e4tree)
     1

e5tree = 1 :> map (\c -> c :> []) [1..5]
e5 :: [Test]
e5 = U.t "e5"
     (leaves e5tree)
     5

e6tree = 1 :> map (\c -> c :> []) [1..5]
e6 :: [Test]
e6 = U.t "e6"
     (product (map leaves (children e6tree)))
     1

ex7  = (*) (leaves . head . children . head . children $ xs) (product . map size . children . head . drop 2 . children $ xs)
e7 :: [Test]
e7 = U.t "e7"
     ex7
     16

-- ===================================
-- Ex. 8-10
-- ===================================

instance Functor Rose where
    fmap f (a :> []) = (f a) :> []
    fmap f (a :> xs) = (f a) :> (map (\x -> fmap f x) xs)

e8a :: [Test]
e8a  = U.t "e8a"
       (fmap (*2) (1 :> [2 :> [], 3 :> []]))
       (2 :> [4 :> [], 6 :> []])

e8b :: [Test]
e8b  = U.t "e8b"
       (fmap (+1) (1 :> []))
       (2 :> [])

e8tree = 1 :> map (\c -> c :> []) [1..5]
e8 :: [Test]
e8  = U.t "e8"
      (size (fmap leaves (fmap (:> []) e8tree)))
      6

e9f :: Rose a -> Rose a
e9f r = fmap head $ fmap (\x -> [x]) r

ex10 = round . root . head . children . fmap (\x -> if x > 0.5 then x else 0) $ fmap (\x -> sin(fromIntegral x)) xs
e10 :: [Test]
e10  = U.t "e10"
       ex10
       1

-- ===================================
-- Ex. 11-13
-- ===================================

class Monoid m where
    mempty :: m
    mappend :: m -> m -> m

newtype Sum     a = Sum     a deriving (Eq, Show)
newtype Product a = Product a deriving (Eq, Show)

instance Num a => Monoid (Sum a) where
  mempty                  = (Sum 0)
  mappend (Sum x) (Sum y) = (Sum $ x + y)

instance Num a => Monoid (Product a) where
  mempty                          = (Product 1)
  mappend (Product x) (Product y) = (Product $ x * y)

unSum :: Sum a -> a
unSum (Sum a) = a
unProduct :: Product a -> a
unProduct (Product a) = a

e11 :: [Test]
e11  = U.t "e11"
    (unProduct (Product 6 `mappend` (Product . unSum $ Sum 3 `mappend` Sum 4)))
    42

-- e12 :: Num num => Sum (unSum num)
-- e12 :: Int int => Sum int
e12 :: Num string => Sum string
e12 = Sum 3 `mappend` Sum 4

num1 = mappend (mappend (Sum 2) (mappend (mappend mempty (Sum 1)) mempty)) (mappend (Sum 2) (Sum 1))

num2 = mappend (Sum 3) (mappend mempty (mappend (mappend (mappend (Sum 2) mempty) (Sum (-1))) (Sum 3)))

ex13 = unSum (mappend (Sum 5) (Sum (unProduct (mappend (Product (unSum num2)) (mappend (Product (unSum num1)) (mappend mempty (mappend (Product 2) (Product 3))))))))

e13 :: [Test]
e13  = U.t "e13"
       ex13
       257

-- ===================================
-- Ex. 14-15
-- ===================================

newtype Endo a = Endo { appEndo :: a -> a }
instance Monoid (Endo a) where
    mempty = Endo id
    Endo f `mappend` Endo g = Endo (f . g)

class Functor f => Foldable f where
    fold         :: Monoid m => f m -> m
    fold          = foldMap id
    foldMap      :: Monoid m => (a -> m) -> f a -> m
    foldMap  f0   = Lab6.foldr (mappend . f0) mempty
    foldr        :: (a -> b -> b) -> b -> f a -> b
    foldr f1 z f  = appEndo (foldMap (Endo . f1) f) z

instance Foldable [] where
    foldr = P.foldr

instance Foldable Rose where
    foldMap f (x :> xs0) = f x `mappend` foldMap (foldMap f) xs0

e14tree = 1 :> [2 :> [], 3 :> [4 :> []]]
e14tree' = fmap Product e14tree

e14 :: [Test]
e14  = U.t "e14"
       (unProduct $ fold e14tree')
       24

sumxs = Sum 0 :> [Sum 13 :> [Sum 26 :> [Sum (-31) :> [Sum (-45) :> [], Sum 23 :> []]]], Sum 27 :> [], Sum 9 :> [Sum 15 :> [Sum 3 :> [Sum (-113) :> []], Sum 1 :> []], Sum 71 :> [Sum 55 :> []]]]


ex15 = unSum (mappend (mappend (fold sumxs) (mappend (fold . head . drop 2 . children $ sumxs) (Sum 30))) (fold . head . children $ sumxs))
e15 :: [Test]
e15  = U.t "e15"
       ex15
       111

-- ===================================
-- Ex. 16-18
-- ===================================

e16tree = 42 :> [3 :> [2:> [], 1 :> [0 :> []]]]
e16 :: [Test]
e16  = U.t "e16"
       (unSum $ foldMap Sum e16tree)
       48

ex17 = unSum (mappend (mappend (foldMap (\x -> Sum x) xs) (mappend (foldMap (\x -> Sum x) . head . drop 2 . children $ xs) (Sum 30))) (foldMap (\x -> Sum x) . head . children $ xs))
e17 :: [Test]
e17  = U.t "e17"
       ex17
       206

ex18 = unSum (mappend (mappend (foldMap (\x -> Sum x) xs) (Sum (unProduct (mappend (foldMap (\x -> Product x) . head . drop 2 . children $ xs) (Product 3))))) (foldMap (\x -> Sum x) . head . children $ xs))
e18 :: [Test]
e18  = U.t "e18"
       ex18
       25946026

-- ===================================
-- Ex. 19-21
-- ===================================

fproduct, fsum :: (Foldable f, Num a) => f a -> a
fsum     fa = unSum     $ fold (fmap Sum     fa)
fproduct fa = unProduct $ fold (fmap Product fa)

e19 :: [Test]
e19  = U.t "e19"
       (fsum xs)
       91

e20 :: [Test]
e20  = U.t "e20"
       (fproduct xs)
       0

ex21 = ((fsum . head . drop 1 . children $ xs) + (fproduct . head . children . head . children . head . drop 2 . children $ xs)) - (fsum . head . children . head . children $ xs)
e21 :: [Test]
e21  = U.t "e21"
       ex21
       82

------------------------------------------------------------------------------

main :: IO Counts
main =
    T.runTestTT $ T.TestList $ e0a ++ e0b ++ e0c ++ e0d ++ e0e ++ e1 ++ e2 ++ e3 ++
                               e4 ++ e5 ++ e6 ++ e7 ++
                               e8a ++ e8b ++ e8 ++ e10 ++
                               e11 ++ e13 ++ e14 ++ e15 ++ e16 ++
                               e17 ++ e18 ++ e19 ++ e20 ++ e21

-- End of file.

