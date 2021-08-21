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
