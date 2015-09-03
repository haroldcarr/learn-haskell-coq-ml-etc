{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds             #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE StandaloneDeriving    #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE UndecidableInstances  #-}
module XGenericsAndTypeLevel where
import GHC.Exts (Constraint)
import Data.Proxy
{-
material on github

extremes
- parametric poly : same code on all types
- middle: generics: derive from structure of data types
- ad hoc : same name - dispatched to different code
-}
data List a where
    NNil :: List a
    CCons :: a -> List a -> List a
{-
:set -XDataKinds
:k '[]
:k '[Int, Bool, Char]
:k '[IO, Maybe]
-}
-- hetereogeneic lists
data HList :: [*] -> * where
    HNil :: HList '[]
    HCons :: x -> HList xs -> HList (x ': xs)
infixr 5 `HCons`
{-
:t 'x' `HCons` True `HCons` HNil
-}
-- NP : Nary Products
data NP :: (k -> *) -> [k] -> * where
    Nil :: NP f '[]
    (:*) :: f x -> NP f xs -> NP f (x ': xs)
infixr 5 :*
-- get hlist back
newtype I a   = I a deriving Show -- same as 'Identity', but shorter
newtype K a b = K a deriving Show
{-
:t I 'x' :* I True :* Nil
type families are functions on the type level
-}
-- what this real line says
-- All :: (* -> Contraint) -> [*] -> Constraint
type family All (c :: k -> Constraint) (xs :: [k]) :: Constraint where
    All c      '[]  = ()
    All c (x ': xs) = (c x, All c xs)
{-
:k All Show '[Int, Bool]
:kind! All Show '[Int, Bool]
-}
-- need type-level composition of f and g x
-- can't do as type family because must be fully applied
-- uses UndecidableInstances
class (f (g x)) => Compose f g x
instance (f (g x)) => Compose f g x
deriving instance All (Compose Show f) xs => Show (NP f xs)
{-
K 1 :* K 2 :* Nil
-}
-- rank2 type
mapNP :: (forall x . f x-> g x) -> NP f xs -> NP g xs
mapNP f       Nil = Nil
mapNP f (x :* xs) = f x :* mapNP f xs
{-
:t I (2::Int) :* I True :* I 'c' :* Nil
mapNP (\(I x) -> Just x) (I (2::Int) :* I True :* I 'c' :* Nil)
-}
{-
exercise : write body for:
eqNP :: (All Eq xs) => NP I xs -> NP I xs -> Bool
-}

eqNP :: (All Eq xs) => NP I xs -> NP I xs -> Bool
eqNP         Nil         Nil = True
eqNP (I x :* xs) (I y :* ys) = x == y && eqNP xs ys
eqNP          _           _  = error "impossible"  -- workaround GHC weakness
{-
eqNP (I (2::Int) :* I True :* I 'c' :* Nil) (I (2::Int) :* I True :* I 'c' :* Nil)
eqNP (I (2::Int) :* I True :* I 'c' :* Nil) (I (2::Int) :* I True :* I 'd' :* Nil)
-}
-- generic sums/choice of constructors; products/args to individual constructorsx
-- above : products (the args to individual constructors)
-- below : sums (the different constructors)

-- constrained map
-- have to tell GHC which constrain we mean : via proxy
-- data Proxy a = Proxy

cmapNP :: (All c xs) => Proxy c -> (forall x . c x => f x -> g x) -> NP f xs -> NP g xs
cmapNP _ _       Nil = Nil
cmapNP p f (x :* xs) = f x :* cmapNP p f xs
{-
cmapNP (Proxy::Proxy Show) (\(I x) -> K (show x)) (I (2::Int) :* I True :* I 'c' :* Nil)
-}
-- Applicative style interface:
-- pure :: a -> f a
-- (<*>) :: f (a -> b) -> f a -> f b
-- one way:
{- doesn't compile
class PureNP (xs :: [k]) where
    pureNP' :: (forall x . f x) -> NP f xs
instance (PureNP '[]) where
    pureNP' x = Nil
instance (PureNP xs) where
    pureNP' x = x :* pureNP' x
-}
-- better way : singleton types
{-
class PureNP (xs :: [k]) where
    pureNP' :: (forall x . f x) -> NP f xs
instance (PureNP '[]) where
    pureNP' x = Nil
instance (PureNP xs) where
    pureNP' x = x :* pureNP' x
-}
