open import eq
open import bool
open import bool-relations using (transitive; total)
open import maybe
open import nat
open import nat-thms       using (≤-trans; ≤-total)
open import product

module z05-01-hc-sorted-list-test where

open import bool-relations _≤_ hiding (transitive; total)

import z05-01-hc-sorted-list as SL
open SL nat _≤_ (λ {x} {y} {z} → ≤-trans {x} {y} {z})
                (λ {x} {y}     → ≤-total {x} {y})

empty : slist 10 10
empty = snil refl

_ : slist 9 10
_ = slist-insert 9 empty refl

--_ : slist-insert 9 empty refl ≡ scons 9  (snil {!!}) {!!}
--_ = refl

