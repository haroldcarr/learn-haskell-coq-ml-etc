open import Base
open import Nat
open import PropositionalEquality using (_≡_; refl; cong; equivalence)
open import Sort _≤?_ totalOrder

module Test where

empty : OList ⊥ ⊤
empty = nil ⊥≤̂

l-9 : OList ⊥ ⊤
l-9 = insert 9 empty ⊥≤̂ ≤̂⊤

_ : l-9 ≡ cons 9 (nil ≤̂⊤) ⊥≤̂
_ = refl

l-5-9 : OList ⊥ ⊤
l-5-9 = insert 5 l-9 ⊥≤̂ ≤̂⊤

_ : l-5-9 ≡ cons 5
                 (cons 9
                       (nil ≤̂⊤)
                       (≤-lift (s≤s (s≤s (s≤s (s≤s (s≤s z≤n)))))))
                 ⊥≤̂
_ = refl

l-1-5-9 : OList ⊥ ⊤
l-1-5-9 = insert 1 l-5-9 ⊥≤̂ ≤̂⊤

_ : l-1-5-9 ≡ cons 1
                   (cons 5
                         (cons 9
                               (nil ≤̂⊤)
                               (≤-lift (s≤s (s≤s (s≤s (s≤s (s≤s z≤n)))))))
                         (≤-lift (s≤s z≤n))) -- NOTE CHANGE
                   ⊥≤̂
_ = refl

sl : List ℕ
sl = t-sort (12 ∷ 3 ∷ 7 ∷ 4 ∷ 40 ∷ 5 ∷ 0 ∷ [])

_ : sl ≡ (0 ∷ 3 ∷ 4 ∷ 5 ∷ 7 ∷ 12 ∷ 40 ∷ [])
_ = refl
