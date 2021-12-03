module P070_vect_insert_sort where

open import Data.Bool
open import Data.Nat
open import Data.Vec hiding (insert)
open import Eq-Ord
open import Function.Base

Nat = ℕ
S   = suc

insert : {elem : Set} {len : Nat} {{eqT : Eq elem}} {{ordT : Ord eqT}}
      -> (x : elem) -> (xsSorted : Vec elem len)
      -> Vec elem (S len)
insert x       [] = x ∷ []
insert x (y ∷ xs) =
  case Eq-Ord.compare x y of λ where
    LT -> x ∷ y ∷ xs
    _  -> y ∷ insert x xs

insSort : {elem : Set} {len : Nat} {{eqT : Eq elem}} {{ordT : Ord eqT}}
       -> Vec elem len
       -> Vec elem len
insSort      []  = []
insSort (x ∷ xs) = insert x (insSort xs)

{-
insert 2 (1 ∷ 3 ∷ 5 ∷ [])
insSort (1 ∷ 3 ∷ 2 ∷ 9 ∷ 7 ∷ 6 ∷ 4 ∷ 5 ∷ 8 ∷ [])
-}

