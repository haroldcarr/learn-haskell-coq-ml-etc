{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DefaultSignatures     #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE StandaloneDeriving    #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE UndecidableInstances  #-}

module XGenericsUse where

import           Generics.SOP    -- SOP = Sum of Products
import           Generics.SOP.TH
import           Generics.SOP.NP

class Eq' a where
    eq' :: a -> a -> Bool
    default eq' :: Generic a => a -> a -> Bool
    eq' = geq

geq :: (Generic a) => a -> a -> Bool
geq x y = geq' (from x) (from y)

geq' :: SOP I xs -> SOP I xs -> Bool
geq' = undefined

eqNP :: (All Eq xs, SingI xs) => NP I xs -> NP I xs -> Bool
eqNP xs ys =
      and
    $ collapse_NP
    $ hcliftA2 (Proxy::Proxy Eq) (\(I x) (I y) -> K (x == y)) xs ys

eqNS :: (All2 Eq xss, All SingI xss) => NS (NP I) xss -> NS (NP I) xss -> Bool
eqNS (Z x) (Z y) = eqNP x y
eqNS (S i) (S j) = eqNS i j
eqNS    _     _  = False

-- Example Use

data BinTree a = Leaf a | Node (BinTree a) (BinTree a) deriving Show

deriveGeneric ''BinTree

instance Eq' Int where
    eq' = (==)
instance Eq' a => Eq' (BinTree a)

