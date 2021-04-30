module 747Negation where

-- Library

open import Relation.Binary.PropositionalEquality using (_≡_; refl) -- added last
open import Data.Nat using (ℕ; zero; suc) 
open import Data.Empty using (⊥; ⊥-elim)
open import Data.Sum using (_⊎_; inj₁; inj₂)
open import Data.Product using (_×_; proj₁; proj₂)

-- Negation is defined as implying false.

¬_ : Set → Set
¬ A = A → ⊥

-- if both ¬ A and A hold, then ⊥ holds (not surprisingly).

¬-elim : ∀ {A : Set}
  → ¬ A
  → A
    ---
  → ⊥

¬-elim = {!!}

infix 3 ¬_

-- Double negation introduction.

¬¬-intro : ∀ {A : Set}
  → A
    -----
  → ¬ ¬ A

¬¬-intro = {!!}

-- Double negation cannot be eliminated in intuitionistic logic.

-- Triple negation elimination.

¬¬¬-elim : ∀ {A : Set}
  → ¬ ¬ ¬ A
    -------
  → ¬ A

¬¬¬-elim = {!!}

-- One direction of the contrapositive.

contraposition : ∀ {A B : Set}
  → (A → B)
    -----------
  → (¬ B → ¬ A)

contraposition = {!!}

-- The other direction cannot be proved in intuitionistic logic.

-- not-equal-to.

_≢_ : ∀ {A : Set} → A → A → Set
x ≢ y  =  ¬ (x ≡ y)

_ : 1 ≢ 2
_ = {!!}

-- One of the first-order Peano axioms.

peano : ∀ {m : ℕ} → zero ≢ suc m
peano = {!!}

-- Copied from 747Isomorphism.

postulate
  extensionality : ∀ {A B : Set} {f g : A → B}
    → (∀ (x : A) → f x ≡ g x)
      -----------------------
    → f ≡ g

-- Two proofs of ⊥ → ⊥ which look different but are the same
-- (assuming extensionality).
   
id : ⊥ → ⊥
id x = x

id′ : ⊥ → ⊥
id′ ()

id≡id′ : id ≡ id′
id≡id′ = extensionality (λ())

-- Assuming extensionality, any two proofs of a negation are the same

assimilation : ∀ {A : Set} (¬x ¬x′ : ¬ A) → ¬x ≡ ¬x′
assimilation ¬x ¬x′ = extensionality λ x → ⊥-elim (¬x x)

-- Strict inequality (copied from 747Relations).

infix 4 _<_

data _<_ : ℕ → ℕ → Set where

  z<s : ∀ {n : ℕ}
      ------------
    → zero < suc n

  s<s : ∀ {m n : ℕ}
    → m < n
      -------------
    → suc m < suc n

-- 747/PLFA exercise: NotFourLTThree (1 point)
-- Show ¬ (4 < 3).

¬4<3 : ¬ (4 < 3)
¬4<3 = {!!}

-- 747/PLFA exercise: LTIrrefl (1 point)
-- < is irreflexive (never reflexive).

¬n<n : ∀ (n : ℕ) → ¬ (n < n)
¬n<n n = {!!}

-- 747/PLFA exercise: LTTrich (3 points)
-- Show that strict inequality satisfies trichotomy,
-- in the sense that exactly one of the three possibilities holds.
-- Here is the expanded definition of trichotomy.

data Trichotomy (m n : ℕ) : Set where
  is-< : m < n → ¬ m ≡ n → ¬ n < m → Trichotomy m n
  is-≡ : m ≡ n → ¬ m < n → ¬ n < m → Trichotomy m n
  is-> : n < m → ¬ m ≡ n → ¬ m < n → Trichotomy m n

<-trichotomy : ∀ (m n : ℕ) → Trichotomy m n
<-trichotomy m n = {!!}

-- PLFA exercise: one of DeMorgan's Laws as isomorphism
-- ⊎-dual-× : ∀ {A B : Set} → ¬ (A ⊎ B) ≃ (¬ A) × (¬ B)
-- Expand negation as implies-false, then look in 747Relations
-- for a law of which this is a special case.

-- What about ¬ (A × B) ≃ (¬ A) ⊎ (¬ B)?
-- Answer: RHS implies LHS but converse cannot be proved in intuitionistic logic.

-- Intuitionistic vs classical logic.

-- The law of the excluded middle (LEM, or just em) cannot be
-- proved in intuitionistic logic.
-- But we can add it, and get classical logic.

-- postulate
--  em : ∀ {A : Set} → A ⊎ ¬ A

-- How do we know this does not give a contradiction?
-- The following theorem of intuitionistic logic demonstrates this.
-- (The proof is compact, but takes some thought.)

em-irrefutable : ∀ {A : Set} → ¬ ¬ (A ⊎ ¬ A)
em-irrefutable = {!!}

-- PLFA exercise: classical equivalences
-- Excluded middle cannot be proved in intuitionistic logic,
-- but adding it is consistent and gives classical logic.
-- Here are four other classical theorems with the same property.
-- You can show that each of them is logically equivalent to all the others.
-- You do not need to prove twenty implications, since implication is transitive.
-- But there is a lot of choice as to how to proceed!

-- Excluded Middle
em = ∀ {A : Set} → A ⊎ ¬ A

-- Double Negation Elimination
dne = ∀ {A : Set} → ¬ ¬ A → A

-- Peirce’s Law
peirce =  ∀ {A B : Set} → ((A → B) → A) → A

-- Implication as disjunction
iad =  ∀ {A B : Set} → (A → B) → ¬ A ⊎ B

-- De Morgan:
dem = ∀ {A B : Set} → ¬ (¬ A × ¬ B) → A ⊎ B

-- End of classical five exercise.

-- Definition: a formula is stable if double negation holds for it.

Stable : Set → Set
Stable A = ¬ ¬ A → A

-- PLFA exercise: every negated formula is stable.
-- This is triple negation elimination.

-- PLFA exercise: the conjunction of two stable formulas is stable.
-- This is the version of DeMorgan's Law that is a special case, above.

-- Where negation sits in the standard library.

import Relation.Nullary using (¬_)
import Relation.Nullary.Negation using (contraposition)

-- Unicode used in this chapter:

{-

  ¬  U+00AC  NOT SIGN (\neg)
  ≢  U+2262  NOT IDENTICAL TO (\==n)

-}
