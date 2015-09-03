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
data HList :: [*] -> * where
    HNil :: HList '[]
    HCons :: x -> HList xs -> HList (x ': xs)
infixr 5 `HCons`
{-
:t 'x' `HCons` True `HCons` HNil
-}
data NP :: (k -> *) -> [k] -> * where
    Nil :: NP f '[]
    (:*) :: f x -> NP f xs -> NP f (x ': xs)
-- get hlist back
newtype I a   = I a deriving Show -- same as 'Identity', but shorter
newtype K a b = K a deriving Show
{-
:t I 'x' :* I True :* Nil :: NP (K Int) '[Char, Bool]
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
:t I (2::Int) :* I True :* 'c' :* Nil :: NP I '[Int, Bool, Char]
mapNP (\(I x) -> Just x) (I (2::Int) :* I True :* 'c' :* Nil :: NP I '[Int, Bool, Char])
-}


