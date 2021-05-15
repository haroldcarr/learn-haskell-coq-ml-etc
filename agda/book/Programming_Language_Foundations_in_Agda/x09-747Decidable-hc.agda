module x09-747Decidable-hc where

-- Library

import Relation.Binary.PropositionalEquality as Eq
open Eq using (_≡_; cong; refl; sym) -- added sym
open Eq.≡-Reasoning
open import Data.Nat using (ℕ; zero; suc; _≤_; z≤n; s≤s)
open import Data.Product using (_×_) renaming (_,_ to ⟨_,_⟩)
open import Data.Sum using (_⊎_; inj₁; inj₂)
open import Relation.Nullary using (¬_)
open import Relation.Nullary.Negation using ()
  renaming (contradiction to ¬¬-intro)
open import Data.Unit using (⊤; tt)
open import Data.Empty using (⊥; ⊥-elim)


-- Copied from 747Isomorphism.

record _⇔_ (A B : Set) : Set where
  field
    to   : A → B
    from : B → A
open _⇔_

-- Copied from 747Relations.

infix 4 _<_

data _<_ : ℕ → ℕ → Set where

  z<s : ∀ {n : ℕ}
      ------------
    → zero < suc n

  s<s : ∀ {m n : ℕ}
    → m < n
      -------------
    → suc m < suc n

-- Recall that the constructors for _≤_ are z≤n and s≤s.

-- Here are a couple of examples to show how to prove inequalities
-- (or their negations).

2≤4 : 2 ≤ 4
2≤4 = s≤s (s≤s z≤n)

¬4≤2 : ¬ (4 ≤ 2)
¬4≤2 = λ { (s≤s (s≤s ())) }

-- Bool values (computation)
data Bool : Set where
  true  : Bool
  false : Bool

-- Boolean comparison function
infix 4 _≤ᵇ_

_≤ᵇ_ : ℕ → ℕ → Bool
zero  ≤ᵇ zero  = true
zero  ≤ᵇ suc n = true
suc m ≤ᵇ zero  = false
suc m ≤ᵇ suc n = m ≤ᵇ n

-- PLFA steps through these computations using equational reasoning.

_ : (2 ≤ᵇ 4) ≡ true
_ =  refl

_ : (4 ≤ᵇ 2) ≡ false
_ = refl

-- Relating evidence and computation.

-- Boolean types (evidence)
T : Bool → Set
T true  = ⊤
T false = ⊥

T→≡ : ∀ (b : Bool) → T b → b ≡ true
T→≡ true tt = refl

≡→T : ∀ {b : Bool} → b ≡ true → T b
≡→T refl = tt

≤ᵇ→≤ : ∀ (m n : ℕ) → T (m ≤ᵇ n) → m ≤ n
≤ᵇ→≤  zero    zero   t = z≤n
≤ᵇ→≤  zero   (suc n) t = z≤n
≤ᵇ→≤ (suc m) (suc n) t = s≤s (≤ᵇ→≤ m n t)

≤→≤ᵇ : ∀ {m n : ℕ} → m ≤ n → T (m ≤ᵇ n)
≤→≤ᵇ {zero} {zero}      z≤n  = tt
≤→≤ᵇ {zero} {suc n}     z≤n  = tt
≤→≤ᵇ               (s≤s m≤n) = ≤→≤ᵇ m≤n

-- computation and evidence

data Dec (A : Set) : Set where
  yes :   A → Dec A
  no  : ¬ A → Dec A

inv-s≤s : ∀ {m n : ℕ}
  → suc m ≤ suc n
    -------------
  →     m ≤     n
inv-s≤s (s≤s m≤n) = m≤n

-- helpers for defining _≤?_ (use these so examples will normalize)

¬s≤z : ∀ {m : ℕ} → ¬ (suc m ≤ zero)
¬s≤z = λ ()

¬s≤s : ∀ {m n : ℕ} → ¬ (m ≤ n) → ¬ (suc m ≤ suc n)
¬s≤s  {zero}  {zero} ¬m≤n = λ        sucm≤sucn  → ¬m≤n z≤n
¬s≤s  {zero} {suc n} ¬m≤n = λ        sucm≤sucn  → ¬m≤n z≤n
¬s≤s {suc m}  {zero} ¬m≤n = λ { (s≤s sucm≤sucn) → ¬m≤n sucm≤sucn }
¬s≤s {suc m} {suc n} ¬m≤n = λ { (s≤s sucm≤sucn) → ¬m≤n (s≤s (inv-s≤s sucm≤sucn)) }

-- Decidable ≤.

_≤?_ : ∀ (m n : ℕ) → Dec (m ≤ n)
zero  ≤? zero  = yes z≤n
zero  ≤? suc n = yes z≤n
suc m ≤? zero  = no (λ ())
suc m ≤? suc n
  with m ≤? n
... | yes  m≤n = yes (s≤s  m≤n)
... | no  ¬m≤n = no (¬s≤s ¬m≤n)

-- can also evaluate the LHS of these tests via C-c C-n
_ : 2 ≤? 4 ≡ yes (s≤s  (s≤s  z≤n))
_ = refl
_ : 4 ≤? 2 ≡ no (¬s≤s (¬s≤s ¬s≤z))
_ = refl

-- 747/PLFA exercise: DecLT (3 point)
-- Decidable strict equality.
-- You will need these helper functions as we did above.

¬z<z : ¬ (zero < zero)
¬z<z = λ ()

¬s<s : ∀ {m n : ℕ} → ¬ (m < n) → ¬ (suc m < suc n)
¬s<s ¬m<n = λ { (s<s sucm<sucn) → ¬m<n sucm<sucn }

¬s<z : ∀ {n : ℕ} → ¬ (suc n < zero)
¬s<z = λ ()

_<?_ : ∀ (m n : ℕ) → Dec (m < n)
zero  <? zero  = no (λ ())
zero  <? suc n = yes z<s
suc m <? zero  = no (λ ())
suc m <? suc n
  with m <? n
... | yes m<n = yes (s<s  m<n)
... | no ¬m<n = no (¬s<s ¬m<n)

_ : 2 <? 4 ≡ yes (s<s (s<s (z<s)))
_ = refl
_ : 4 <? 2 ≡ no (¬s<s (¬s<s ¬s<z))
_ = refl
_ : 3 <? 3 ≡ no (¬s<s (¬s<s (¬s<s ¬z<z)))
_ = refl

-- 747/PLFA exercise: DecNatEq (3 points)
-- Decidable equality for natural numbers.

_≡ℕ?_ : ∀ (m n : ℕ) → Dec (m ≡ n)
zero  ≡ℕ? zero  = yes refl
zero  ≡ℕ? suc n = no (λ ())
suc m ≡ℕ? zero  = no (λ ())
suc m ≡ℕ? suc n
  with m ≡ℕ? n
... | yes m≡n = yes (cong suc m≡n)
... | no ¬m≡n = no  λ { refl → ¬m≡n refl }

-- Reusing ≤ᵇ and proofs of equivalence with ≤ to decide ≤.

_≤?′_ : ∀ (m n : ℕ) → Dec (m ≤ n)
m ≤?′ n = {!!}

-- Erasing Dec down to Bool (or "isYes").

⌊_⌋ : ∀ {A : Set} → Dec A → Bool
⌊ yes x ⌋ = true
⌊ no x ⌋ = false

_≤ᵇ′_ : ℕ → ℕ → Bool
m ≤ᵇ′ n  =  ⌊ m ≤? n ⌋

-- If D is Dec A, then T ⌊ D ⌋ is inhabited exactly when A is inhabited.

toWitness : ∀ {A : Set} {D : Dec A} → T ⌊ D ⌋ → A
toWitness t = {!!}

fromWitness : ∀ {A : Set} {D : Dec A} → A → T ⌊ D ⌋
fromWitness a = {!!}

-- Similar ideas when it is the "no" witnesses we want to handle.

isNo : ∀ {A : Set} → Dec A → Bool
isNo (yes _) = false
isNo (no _)  = true

toWitnessFalse : ∀ {A : Set} {D : Dec A} → T (isNo D) → ¬ A
toWitnessFalse = {!!}

fromWitnessFalse : ∀ {A : Set} {D : Dec A} → ¬ A → T (isNo D)
fromWitnessFalse = {!!}

-- Agda standard library definitions for use of these.

True : ∀ {A : Set} → (D : Dec A) → Set
True Q = T ⌊ Q ⌋ 

False : ∀ {A : Set} → (D : Dec A) → Set
False Q = T (isNo Q)

-- A concrete example.

≤ᵇ′→≤ : ∀ {m n : ℕ} → T (m ≤ᵇ′ n) → m ≤ n
≤ᵇ′→≤  =  toWitness

≤→≤ᵇ′ : ∀ {m n : ℕ} → m ≤ n → T (m ≤ᵇ′ n)
≤→≤ᵇ′  =  fromWitness

-- Conclusion: use Decidables instead of Booleans!

-- Logical connectives for Decidables.

infixr 6 _∧_

_∧_ : Bool → Bool → Bool
true ∧ true = true
true ∧ false = false
false ∧ y = false

infixr 6 _×-dec_

_×-dec_ : ∀ {A B : Set} → Dec A → Dec B → Dec (A × B)
da ×-dec db = {!!}

infixr 5 _∨_

_∨_ : Bool → Bool → Bool
true ∨ y = true
false ∨ y = y

infixr 5 _⊎-dec_

_⊎-dec_ : ∀ {A B : Set} → Dec A → Dec B → Dec (A ⊎ B)
da ⊎-dec db = {!!}

not : Bool → Bool
not true  = false
not false = true

¬? : ∀ {A : Set} → Dec A → Dec (¬ A)
¬? da = {!!}

-- A Boolean version of implication.

_⊃_ : Bool → Bool → Bool
true ⊃ true = true
true ⊃ false = false
false ⊃ y = true

_→-dec_ : ∀ {A B : Set} → Dec A → Dec B → Dec (A → B)
da →-dec db = {!!}

-- 747/PLFA exercise: ErasBoolDec (3 points)
-- Erasure relates boolean and decidable operations.

∧-× : ∀ {A B : Set} (x : Dec A) (y : Dec B) → ⌊ x ⌋ ∧ ⌊ y ⌋ ≡ ⌊ x ×-dec y ⌋
∧-× da db = {!!}

∨-× : ∀ {A B : Set} (x : Dec A) (y : Dec B) → ⌊ x ⌋ ∨ ⌊ y ⌋ ≡ ⌊ x ⊎-dec y ⌋
∨-× da db = {!!}

not-¬ : ∀ {A : Set} (x : Dec A) → not ⌊ x ⌋ ≡ ⌊ ¬? x ⌋
not-¬ da = {!!}

-- 747/PLFA exercise: iff-erasure.

_iff_ : Bool → Bool → Bool
true iff true = true
true iff false = false
false iff true = false
false iff false = true

_⇔-dec_ : ∀ {A B : Set} → Dec A → Dec B → Dec (A ⇔ B)
da ⇔-dec db = {!!}

iff-⇔ : ∀ {A B : Set} (x : Dec A) (y : Dec B) → ⌊ x ⌋ iff ⌊ y ⌋ ≡ ⌊ x ⇔-dec y ⌋ 
iff-⇔ da db = {!!}

-- Proof by reflection.
-- Or, getting Agda to construct proofs at compile time.

-- A guarded version of monus.

minus : (m n : ℕ) (n≤m : n ≤ m) → ℕ
minus m zero _ = m
minus (suc m) (suc n) (s≤s m≤n) = minus m n m≤n

-- But we have to provide proofs.

_ : minus 5 3 (s≤s (s≤s (s≤s z≤n))) ≡ 2
_ = refl

-- Agda will fill in an implicit record type if it can fill in all fields.
-- Since ⊤ is defined as a record type with no fields...
-- We can get Agda to compute a value of type True (n ≤? m).

_-_ : (m n : ℕ) {n≤m : True (n ≤? m)} → ℕ
_-_ m n {n≤m} = minus m n (toWitness n≤m)

_ : 5 - 3 ≡ 2
_ = refl

-- We will later use this to get Agda to compute parts of proofs
-- that would be annoying for us to provide.


