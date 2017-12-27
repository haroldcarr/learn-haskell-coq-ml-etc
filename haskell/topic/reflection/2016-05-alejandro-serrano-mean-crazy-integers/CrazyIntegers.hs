{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE KindSignatures    #-}
{-# LANGUAGE PolyKinds         #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE TypeFamilies      #-}

module CrazyIntegers where

data Nat = Z | S Nat

data SNat (n :: Nat) where
  SZ :: SNat 'Z
  SS :: forall n. SNat n -> SNat ('S n)

instance Show (SNat 'Z) where
  show SZ = "Z"
instance Show (SNat n) => Show (SNat ('S n)) where
  show (SS n) = 'S' : show n

data SomeNat where
  SomeNat :: forall n. Show (SNat n) => SNat n -> SomeNat

data Some f where
  Some :: f n -> Some f


instance Show (Some SNat) where
  show (Some SZ)     = "!Z"
  show (Some (SS n)) = '!' : 'S' : show (Some n)


instance Show SomeNat where
  show (SomeNat sn) = show sn

-- intToSomeNat :: Integer -> SNat n
intToSomeNat :: Integer -> Some SNat
intToSomeNat 0 = Some SZ
intToSomeNat n = case intToSomeNat (n-1) of
                   Some n' -> Some (SS n')


type family Add (n :: Nat) (m :: Nat) where
  Add 'Z     m = m
  Add ('S n) m = 'S (Add n m)

addS :: SNat n -> SNat m -> SNat (Add n m)
addS SZ      sm = sm
addS (SS sn) sm = SS (addS sn sm)

addSome :: Some SNat -> Some SNat -> Some SNat
addSome (Some n) (Some m) = Some (addS n m)


type family Unapply (x :: k) :: (t -> k, k) where
  Unapply (f x) = '(f, x)

data Equ (a :: k) (b :: k) where
  Refl :: Equ a a

typesAreEqual :: Equ (Unapply [Int]) '([], Int)
typesAreEqual = Refl
