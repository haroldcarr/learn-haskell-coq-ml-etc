module Eq-Ord where

-- file:///Users/hcarr/Downloads/On_the_Bright_Side_of_Type_Classes_Instance_Argume.pdf

open import Data.Bool
import      Data.Nat
open import Relation.Nullary

private
  Nat = Data.Nat.ℕ

record Eq (t : Set) : Set where
  field eq : t -> t -> Bool

open Eq {{ ... }} public

private
  eqBoolPrim : (x y : Bool) -> Bool
  eqBoolPrim false false = true
  eqBoolPrim true  true  = true
  eqBoolPrim false true  = false
  eqBoolPrim true  false = false

instance
  eqBool : Eq Bool
  eqBool = record { eq = eqBoolPrim }

instance
  eqNat  : Eq Nat
  eqNat  = record { eq = Data.Nat._≡ᵇ_ }

equal : {t : Set} -> {{eqT : Eq t}} -> t -> t -> Bool
equal {{eqT}} = Eq.eq eqT

data Ordering : Set where
  LT EQ GT : Ordering

record Ord {A : Set} (eqA : Eq A) : Set where
  field _<=_ : A -> A -> Bool

open Ord {{ ... }} public

private
  ≤Bool : (x y : Bool) -> Bool
  ≤Bool false b     = true
  ≤Bool true  false = false
  ≤Bool true  true  = true

instance
 <=Bool : Ord eqBool
 <=Bool = record { _<=_ = ≤Bool }

private
  <=NatP : (x y : Nat) -> Bool
  <=NatP x y
    with x Data.Nat.≤? y
  ... | does Relation.Nullary.because _ = does

instance
 <=Nat  : Ord eqNat
 <=Nat  = record { _<=_ = <=NatP }

compare : {t : Set} -> {{eqT : Eq t}} -> {{ordT : Ord eqT}} -> t -> t -> Ordering
compare x y =
  if      equal x y then EQ
  else if x <= y    then LT
  else                   GT
