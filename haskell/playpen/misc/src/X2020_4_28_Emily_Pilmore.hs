{-# OPTIONS_GHC -fno-warn-type-defaults #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module X2020_4_28_Emily_Pilmore where

newtype UXO a = UXO [a] deriving (Functor, Applicative, Show)

(+++) :: UXO a -> UXO a -> UXO a
UXO a +++ UXO b = UXO (a ++ b)

instance Monad UXO where
  return           = UXO . flip fmap [(1::Int) ..] . const
  UXO    []  >>= _ = UXO []
  UXO (a:as) >>= k = k a +++ (UXO as >>= k)


x12 :: UXO Int
x12  = do
  x <- UXO [1,2]
  UXO [x] +++ UXO [3,4]

l12 :: [Int]
l12  = do
  x <- [1,2]
  x : [3,4]

epl :: [Int]
epl  = do
  x <- [3,4,5]
  return (x + 1)


epu :: UXO Int
epu  = do
  x <- UXO [3,4,5]
  return (x + 1)

xxxl :: [Int]
xxxl = (*) <$>     [1,2,3] <*>     [10,100,1000]

xxxu :: UXO Int
xxxu = (*) <$> UXO [1,2,3] <*> UXO [10,100,1000]
