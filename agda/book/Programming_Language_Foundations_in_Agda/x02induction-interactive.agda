module x02induction-interactive where

import      Relation.Binary.PropositionalEquality as Eq
open        Eq             using (_≡_; refl; cong; sym)
open        Eq.≡-Reasoning using (begin_; _≡⟨⟩_; step-≡; _∎)
open import Data.Nat       using (ℕ; zero; suc; _+_; _*_; _∸_)


{-
------------------------------------------------------------------------------
## Building proofs interactively

    +-assoc′ : ∀ (m n p : ℕ) → (m + n) + p ≡ m + (n + p)
    +-assoc′ m n p = ?

C-c C-l

    +-assoc′ : ∀ (m n p : ℕ) → (m + n) + p ≡ m + (n + p)
    +-assoc′ m n p = { }0

new window at the bottom:

    ?0 : ((m + n) + p) ≡ (m + (n + p))

indicates hole 0 needs to be filled with a proof of the stated judgment

to prove the proposition by induction on `m`
move cursor into hole
C-c C-c
prompt: pattern variables to case (empty for split on result):

type `m` to case split on that variable

    +-assoc′ : ∀ (m n p : ℕ) → (m + n) + p ≡ m + (n + p)
    +-assoc′ zero n p = { }0
    +-assoc′ (suc m) n p = { }1

There are now two holes, and the window at the bottom tells you what
each is required to prove:

    ?0 : ((zero + n) + p) ≡ (zero + (n + p))
    ?1 : ((suc m + n) + p) ≡ (suc m + (n + p))

goto hole 0
C-c C-,

    Goal: (n + p) ≡ (n + p)
    ————————————————————————————————————————————————————————————
    p : ℕ
    n : ℕ

indicates that after simplification the goal for hole 0 is as stated,
 and that variables `p` and `n` of the stated types are available to use in the proof.

the proof of the given goal is simple
goto goal
C-c C-r
fills in with refl
C-c C-l renumbers remaining hole to 0:

    +-assoc′ : ∀ (m n p : ℕ) → (m + n) + p ≡ m + (n + p)
    +-assoc′ zero n p = refl
    +-assoc′ (suc m) n p = { }0

goto hole 0
C-c C-,

    Goal: suc ((m + n) + p) ≡ suc (m + (n + p))
    ————————————————————————————————————————————————————————————
    p : ℕ
    n : ℕ
    m : ℕ

gives simplified goal and available variables

need to rewrite by the induction hypothesis

    +-assoc′ : ∀ (m n p : ℕ) → (m + n) + p ≡ m + (n + p)
    +-assoc′ zero n p = refl
    +-assoc′ (suc m) n p rewrite +-assoc′ m n p = { }0

goto hole
C-c C-,

    Goal: suc (m + (n + p)) ≡ suc (m + (n + p))
    ————————————————————————————————————————————————————————————
    p : ℕ
    n : ℕ
    m : ℕ

goto goal
C-c C-r
fills in, completing proof

    +-assoc′ : ∀ (m n p : ℕ) → (m + n) + p ≡ m + (n + p)
    +-assoc′ zero n p = refl
    +-assoc′ (suc m) n p rewrite +-assoc′ m n p = refl
-}
