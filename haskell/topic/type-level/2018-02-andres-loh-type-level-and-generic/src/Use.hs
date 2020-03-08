{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DefaultSignatures     #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
-- {-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds             #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE StandaloneDeriving    #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE UndecidableInstances  #-}

module Use where

import           Generics.SOP
import           Generics.SOP.TH

{-# ANN module ("HLint: ignore Use camelCase" :: String) #-}

type family Sing :: k -> *
class SingI a where
  sing :: Sing a

class Eq' a where
  eq' :: a -> a -> Bool
  default eq' :: (Generic a, All2 Eq' (Code a), All SingI (Code a)) => a -> a -> Bool
  eq' = geq

geq :: (Generic a, All2 Eq' (Code a), All SingI (Code a)) => a -> a -> Bool
geq x y = geq' (from x) (from y)

geq' :: (All2 Eq' xss, All SingI xss) => SOP I xss -> SOP I xss -> Bool
geq' (SOP x) (SOP y) = eq_NS x y

eq_NS :: (All2 Eq' xss, All SingI xss) => NS (NP I) xss -> NS (NP I) xss -> Bool
eq_NS (Z x) (Z y) = eq_NP x y
eq_NS (S i) (S j) = eq_NS i j
eq_NS _     _     = False

eq_NP :: All Eq' xs => NP I xs -> NP I xs -> Bool
eq_NP xs ys =
    and
  $ hcollapse
  $ hcliftA2 (Proxy :: Proxy Eq') (\ (I x) (I y) -> K (eq' x y)) xs ys

-- Example use

data BinTree a = Node (BinTree a) (BinTree a) | Leaf a
  deriving Show

deriveGeneric ''BinTree

instance Eq' Int where
  eq' = (==)

instance (Eq' a, SingI '[a], SingI '[BinTree a, BinTree a]) => Eq' (BinTree a)



class Default a where
  def :: a
  default def :: (Generic a, Code a ~ (xs ': xss), All Default xs) => a
  def = gdef

gdef :: (Generic a, Code a ~ (xs ': xss), All Default xs) => a
gdef = to (SOP (Z (hcpure (Proxy :: Proxy Default) (I def))))

instance Default Int where
  def = 42

instance Default a => Default (BinTree a)
