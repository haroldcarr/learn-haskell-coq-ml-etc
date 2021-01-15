module v01-02-induction where

import Relation.Binary.PropositionalEquality as Eq
open Eq             using (_≡_; refl; sym)
open Eq.≡-Reasoning using (begin_; _≡⟨⟩_; _∎)
open import v01-01-basics

plus-n-O : ∀ (n : nat) → n ≡ n + 0
plus-n-O    O  = refl
plus-n-O (S n) rewrite sym (plus-n-O n) = refl

minus-n-n : ∀ (n : nat) → minus n n ≡ 0
minus-n-n    O  = refl
minus-n-n (S n) rewrite sym (minus-n-n n) = refl

mult-0-r : ∀ (n : nat) → n * 0 ≡ 0
mult-0-r    O  = refl
mult-0-r (S n) rewrite mult-0-r n = refl

plus-n-Sm : ∀ (n m : nat) → S (n + m) ≡ n + (S m)
plus-n-Sm    O  m = refl
plus-n-Sm (S n) m rewrite sym (plus-n-Sm n m) = refl

plus-comm : ∀ (n m : nat) → n + m ≡ m + n
plus-comm    n     O  rewrite sym (plus-n-O n) = refl
plus-comm    O  (S m) rewrite sym (plus-n-O m) = refl
plus-comm (S n) (S m)
  rewrite
    sym (plus-n-Sm n m)
  | sym (plus-n-Sm m n)
  | plus-comm n m
  = refl

plus-assoc : ∀ (n m p : nat) → n + (m + p) ≡ (n + m) + p
plus-assoc    O  m p = refl
plus-assoc (S n) m p        --         S n + (m + p) ≡ S n + m + p
  rewrite
    plus-comm (S n + m) p        -- S (plus n (plus m p)) ≡ plus p (S (plus n m))
  | sym (plus-n-Sm p (plus n m)) -- S (plus n (plus m p)) ≡ S (plus p (plus n m))
  | plus-comm p (plus n m)       -- S (plus n (plus m p)) ≡ S (plus (plus n m) p)
  | plus-assoc n m p             -- S (plus (plus n m) p) ≡ S (plus (plus n m) p)
  = refl

double : (n : nat) → nat
double    O  = O
double (S n) = S (S (double n))

double-plus : ∀ (n : nat) → double n ≡ n + n
double-plus    O  = refl
double-plus (S n)          --     double (S n) ≡ S n + S n
                           -- S (S (double n)) ≡ S n + S n
  rewrite
    double-plus n          -- S (S (plus n n)) ≡ S (plus n (S n))
  | plus-n-Sm n n          -- S (plus n (S n)) ≡ S (plus n (S n))
  = refl

{-
Inconvenient in evenb def: recursive call on n - 2.
Makes inductibe proofs about evenb harder since may need IH about n - 2.
Following lemma gives alternative characterization of evenb (S n) that works better with induction.
-}

evenb-S : ∀ (n : nat) → evenb (S n) ≡ negb (evenb n)
evenb-S    O  = refl
evenb-S (S n)                 -- evenb (S (S n)) ≡ negb (evenb (S n))
                              --         evenb n ≡ negb (evenb (S n))
  rewrite
    evenb-S n                 --         evenb n ≡ negb (negb (evenb n))
  | negb-involutive (evenb n) --         evenb n ≡ evenb n
  = refl

-- Proofs Within Proofs TODO
