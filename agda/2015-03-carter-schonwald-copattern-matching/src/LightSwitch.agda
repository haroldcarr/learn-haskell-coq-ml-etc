module LightSwitch where

open import Data.Nat
open import Data.Bool

record Light : Set where
  coinductive
  field
    isLit : Bool -- head
    switchFlip : Light -- tail
open Light

firstOffAlts : Light
firstOnAlts  : Light

isLit      firstOffAlts = false
switchFlip firstOffAlts = firstOnAlts

isLit      firstOnAlts = true
switchFlip firstOnAlts = firstOffAlts

alternates : forall (b : Bool ) -> Light
isLit      (alternates x) = x
switchFlip (alternates x) = alternates (not x)
