{-# OPTIONS --allow-unsolved-metas #-}

open import bool
open import bool-thms2
open import eq
open import maybe
open import product
open import product-thms
open import bool-relations using (transitive ; total)

module z05-01-hc-sorted-list
  (A : Set)                    -- type of elements
  (_≤A_ : A → A → 𝔹)           -- ordering function
  (≤A-trans : transitive _≤A_) -- proof of transitivity of given ordering
  (≤A-total : total      _≤A_) -- proof of totality     of given ordering
  where

open import bool-relations _≤A_ hiding (transitive ; total)
open import minmax _≤A_ ≤A-trans ≤A-total

data slist : A → A → Set where
  snil  : ∀ {l u : A}
        → l ≤A u ≡ tt
        → slist l u
  scons : ∀ {l u : A}
        → (d : A)      -- value stored at head
        → slist l u    -- elements to the right ≥ stored value
        → d ≤A l ≡ tt
        → slist (min d l) (max d u)

slist-insert : ∀ {l u : A}
             → (d : A)                   -- insert 'd'
             → slist l u                 -- into this 'bst'
             → d ≤A l ≡ tt
             → slist (min d l) (max d u) -- type might change
slist-insert d (snil l≤Au) d≤Al≡tt = scons d (snil l≤Au) d≤Al≡tt

slist-insert d   (scons d' xs d'≤Al≡tt) d≤Al≡tt  with keep (d ≤A d')

slist-insert d L@(scons d' xs d'≤Al≡tt) d≤Al≡tt | tt , p = scons d L d≤Al≡tt

slist-insert d L@(scons d' xs d'≤Al≡tt) d≤Al≡tt | ff , p = {!!}
{-
slist-insert d d≤Al≡tt L@(scons d' xs d'≤Al≡tt) | ff , p with slist-insert d {!!} {!!}

slist-insert d d≤Al≡tt L@(scons d' xs d'≤Al≡tt) | ff , p | R' = {!!}
-}

{-
bst-insert : ∀{l u : A}
           → (d : A)                 -- insert 'd'
           → bst l u                 -- into this 'bst'
           → bst (min d l) (max d u) -- type might change
bst-insert d (bst-leaf p) =
  bst-node d (bst-leaf ≤A-refl) (bst-leaf ≤A-refl) min-≤1 max-≤1

bst-insert d (bst-node d' L R p1 p2) with keep (d ≤A d')

bst-insert d (bst-node d' L R p1 p2) | tt , p with bst-insert d L

bst-insert d (bst-node d' L R p1 p2) | tt , p | L' rewrite p =
  bst-node d' L'                (bst-inc-ub R (≤A-trans p2 max-≤2))
           (min2-mono p1) ≤A-refl

bst-insert d (bst-node d' L R p1 p2) | ff , p with bst-insert d R

bst-insert d (bst-node d' L R p1 p2) | ff , p | R' rewrite p =
  bst-node d' (bst-dec-lb L p1) R'
           min-≤2         (max2-mono p2)


-}
