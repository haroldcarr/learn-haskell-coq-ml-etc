module p022_string_or_int where

open import Data.Bool
open import Data.Nat
import      Data.Nat.Show as DNS
open import Data.String
open import Function.Base

Nat = ℕ

StringOrInt : Bool -> Set
StringOrInt x = case x of λ where
    true  -> Nat
    false -> String

getStringOrInt : (x : Bool) → StringOrInt x
getStringOrInt true  = 94
getStringOrInt false = "Ninety Four"

-- https://lists.chalmers.se/pipermail/agda/2020/011788.html
data Is {ℓ} {A : Set ℓ} : A → Set ℓ where
  ⌊_⌋ : (x : A) → Is x

getStringOrInt' : (x : Bool) → StringOrInt x
getStringOrInt' x = case ⌊ x ⌋ of λ where
  ⌊ true  ⌋ → 94
  ⌊ false ⌋ → "Ninety Four"

valToString : (x : Bool) -> StringOrInt x -> String
valToString true  val = DNS.show val
valToString false val = val

{-
valToString true   99
valToString false "dd"
-}
