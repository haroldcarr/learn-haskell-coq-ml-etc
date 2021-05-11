module x07-747Negation-hc where

open import Relation.Binary.PropositionalEquality using (_≡_; cong; refl; subst; sym) -- added last
open import Data.Nat using (ℕ; zero; suc)
open import Data.Empty using (⊥; ⊥-elim)
open import Data.Sum using (_⊎_; inj₁; inj₂)
open import Data.Product using (_×_; proj₁; proj₂)

------------------------------------------------------------------------------
-- NEGATION : defined as implying false
-- import Relation.Nullary using (¬_)
¬_ : Set → Set
¬ A = A → ⊥
infix 3 ¬_

-- if both ¬ A and A hold, then ⊥ holds
¬-elim : ∀ {A : Set}
  → ¬ A
  → A
    ---
  → ⊥
¬-elim ¬a a = ¬a a

-- double negation introduction (cannot be eliminated in intuitionistic logic)
¬¬-intro : ∀ {A : Set}
  → A
    -----
  → ¬ ¬ A
-- ¬¬-intro a     ¬a = ¬a a -- agda with auto split/fill to this
¬¬-intro    a = λ ¬a → ¬a a -- easier to understand with ¬ def using anon λ

-- triple negation elimination
¬¬¬-elim : ∀ {A : Set}
  → ¬ ¬ ¬ A
    -------
  → ¬ A
-- ¬¬¬-elim ¬¬¬a     a = ¬-elim ¬¬¬a (¬¬-intro a)
¬¬¬-elim    ¬¬¬a = λ a → ¬-elim ¬¬¬a (¬¬-intro a)

-- import Relation.Nullary.Negation using (contraposition)
-- one direction of the contrapositive
-- the other direction cannot be proved in intuitionistic logic
contraposition : ∀ {A B : Set}
  → (A → B)
    -----------
  → (¬ B → ¬ A)
-- contraposition a→b     ¬b     a = ¬b (a→b a)
contraposition    a→b = λ ¬b → λ a → ¬b (a→b a)

-- not-equal-to
_≢_ : ∀ {A : Set} → A → A → Set
x ≢ y  =  ¬ (x ≡ y)

_ : 1 ≢ 2
_ = λ ()

-- a first-order Peano axioms
peano : ∀ {m : ℕ} → zero ≢ suc m
peano ()

open import x05-842Isomorphism-hc using (extensionality)

-- two proofs of ⊥ → ⊥
-- they look different but are the same (assuming extensionality)
id : ⊥ → ⊥
id x = x

id′ : ⊥ → ⊥
id′ ()

id≡id′ : id ≡ id′
id≡id′ = extensionality (λ())

-- assuming extensionality, any two proofs of a negation are the same
assimilation : ∀ {A : Set} (¬x ¬x′ : ¬ A) → ¬x ≡ ¬x′
assimilation ¬x ¬x′ = extensionality λ x → ⊥-elim (¬x x)

-- BEGIN: Strict inequality (copied from 747Relations).
infix 4 _<_

data _<_ : ℕ → ℕ → Set where

  z<s : ∀ {n : ℕ}
      ------------
    → zero < suc n

  s<s : ∀ {m n : ℕ}
    → m < n
      -------------
    → suc m < suc n
-- END: Strict inequality (copied from 747Relations).

-- NotFourLTThree
¬4<3 : ¬ (4 < 3)
¬4<3 (s<s (s<s (s<s ())))

-- < is irreflexive (never reflexive)
¬n<n : ∀ (n : ℕ) → ¬ (n < n)
-- ¬n<n (suc n)       (s<s n<n) = ¬n<n n n<n
¬n<n    (suc n) = λ { (s<s n<n) → ¬n<n n n<n }

-- 747/PLFA exercise: LTTrich (3 points)
-- Prove that strict inequality satisfies trichotomy : exactly one of the three possibilities holds.
data Trichotomy (m n : ℕ) : Set where
  is-< : m < n → ¬ m ≡ n → ¬ n < m → Trichotomy m n
  is-≡ : m ≡ n → ¬ m < n → ¬ n < m → Trichotomy m n
  is-> : n < m → ¬ m ≡ n → ¬ m < n → Trichotomy m n

open import x03-842Relations-hc-2 using (suc-injective; m≡n→sucm≡sucn)

inv-s<s : ∀ {m n : ℕ}
  → suc m < suc n
    -------------
  →     m <     n
inv-s<s (s<s m<n) = m<n

<-trichotomy : ∀ (m n : ℕ) → Trichotomy m n
<-trichotomy zero     zero   = is-≡ refl (λ ()) (λ ())
<-trichotomy zero    (suc n) = is-< z<s  (λ ()) (λ ())
<-trichotomy (suc m)  zero   = is-> z<s  (λ ()) (λ ())
<-trichotomy (suc m) (suc n) with <-trichotomy m n
... | is-< m<n ¬m≡n ¬n<m = is-< (s<s m<n)               (λ sucm≡sucn → ¬m≡n (suc-injective sucm≡sucn))
                                                        (λ sucn<sucm → ¬n<m (inv-s<s       sucn<sucm))
... | is-≡ m≡n ¬m<n ¬n<m = is-≡ (m≡n→sucm≡sucn m n m≡n) (λ sucm<sucn → ¬m<n (inv-s<s       sucm<sucn))
                                                        (λ sucn<sucm → ¬n<m (inv-s<s       sucn<sucm))
... | is-> n<m ¬m≡n ¬m<n = is-> (s<s n<m)               (λ sucm≡sucn → ¬m≡n (suc-injective sucm≡sucn))
                                                        (λ sucm<sucn → ¬m<n (inv-s<s       sucm<sucn))

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
em-irrefutable = λ z → z (inj₂ (λ x → z (inj₁ x)))

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

