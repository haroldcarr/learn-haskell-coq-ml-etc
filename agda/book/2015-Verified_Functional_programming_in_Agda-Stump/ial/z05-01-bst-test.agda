open import eq
open import bool
open import bool-relations using (transitive; total)
open import maybe
open import nat
open import nat-thms       using (≤-trans; ≤-total)
open import product

module z05-01-bst-test where

open import bool-relations _≤_ hiding (transitive; total)

import z05-01-bst
open   z05-01-bst nat _≤_ (λ {x} {y} {z} → ≤-trans {x} {y} {z})
                          (λ {x} {y}     → ≤-total {x} {y})

empty : bst 0 0
empty = bst-leaf refl

_ : bst-search 0 empty ≡ nothing
_ = refl

_ : bst-insert 5 empty ≡ bst-node 5 (bst-leaf refl) (bst-leaf refl) refl refl
_ = refl

bst-5 : z05-01-bst.bst nat _≤_
                           (λ {x} {y} {z} → ≤-trans {x} {y} {z})
                           (λ {x} {y}     → ≤-total {x} {y})
                           0 5
bst-5 = bst-insert 5 empty

_ : maybe (Σ ℕ (λ z → (5 < z || 5 =ℕ z) && (z < 5 || z =ℕ 5) ≡ tt))
_ = bst-search 5 bst-5

_ : bst-insert 9 bst-5 ≡ bst-node 5 (bst-leaf refl)
                           (bst-node 9 (bst-leaf refl) (bst-leaf refl) refl refl) refl refl
_ = refl

_ : z05-01-bst.bst nat _≤_
                       (λ {x} {y} {z} → ≤-trans {x} {y} {z})
                       (λ {x} {y}     → ≤-total {x} {y})
                       0 9
_ = bst-insert 9 bst-5

_ : z05-01-bst.bst nat _≤_
                       (λ {x} {y} {z} → ≤-trans {x} {y} {z})
                       (λ {x} {y}     → ≤-total {x} {y})
                       0 5
_ = bst-dec-lb {0} {0} {5} (bst-leaf refl) refl
