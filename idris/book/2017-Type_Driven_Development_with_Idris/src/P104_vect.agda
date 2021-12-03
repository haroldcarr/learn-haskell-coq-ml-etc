module P104_vect where

open import Data.Fin
open import Data.Maybe   hiding (zip)
open import Data.Nat
open import Data.Product hiding (zip)
open import Function.Base

Nat = ℕ

infixr 7 _:>_

data Vec (A : Set) : Nat -> Set where
  V0   :                                        Vec A  0
  _:>_ : ∀ {n} (x : A) -> (xs : Vec A n) -> Vec A (suc n)

append : {elem : Set} {n m : Nat} -> Vec elem n -> Vec elem m -> Vec elem (n Data.Nat.+ m)
append       V0  ys = ys
append (x :> xs) ys = x :> append xs ys

zip : {a b : Set} {n : Nat} -> Vec a n -> Vec b n -> Vec (a × b) n
zip       V0        V0  = V0
zip (x :> xs) (y :> ys) = (x , y) :> zip xs ys

index : {a : Set} {n : Nat} -> Fin n -> Vec a n -> a
index  zero   (x :>  _) = x
index (suc i) (_ :> xs) = index i xs

v7 : Vec Nat 7
v7 = 1 :> 2 :> 3 :> 4 :> 5 :> 6 :> 7 :> V0

ti : ℕ
ti = index (suc (suc zero)) v7

tryIndex' : {a : Set} {n : Nat} -> Nat -> Vec a n -> Maybe a
tryIndex' _             V0  = nothing
tryIndex'  zero   (x :>  _) = just x
tryIndex' (suc i) (_ :> xs) = tryIndex' i xs

ti' : Maybe Nat
ti' = tryIndex' 2 v7

vectTake : {a : Set} {n : Nat} -> (m : Nat) -> Vec a (m Data.Nat.+ n) -> Vec a m
vectTake  zero          _  = V0
vectTake (suc k) (x :> xs) = x :> vectTake k xs

tvt : Vec Nat  3
tvt = vectTake 3 (1 :> 2 :> 3 :> 4 :> 5 :> 6 :> 7 :> V0)

