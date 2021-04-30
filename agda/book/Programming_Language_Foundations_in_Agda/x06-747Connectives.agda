module 747Connectives where

-- Library

import Relation.Binary.PropositionalEquality as Eq
open Eq using (_≡_; refl)
open Eq.≡-Reasoning
open import Data.Nat using (ℕ)
open import Function using (_∘_)

-- Copied from 747Isomorphism.

postulate
  extensionality : ∀ {A B : Set} {f g : A → B}
    → (∀ (x : A) → f x ≡ g x)
      -----------------------
    → f ≡ g

infix 0 _≃_
record _≃_ (A B : Set) : Set where
  constructor mk-≃  -- This has been added, not in PLFA
  field
    to   : A → B
    from : B → A
    from∘to : ∀ (x : A) → from (to x) ≡ x
    to∘from : ∀ (y : B) → to (from y) ≡ y
open _≃_

infix 0 _≲_
record _≲_ (A B : Set) : Set where
  field
    to      : A → B
    from    : B → A
    from∘to : ∀ (x : A) → from (to x) ≡ x
open _≲_

record _⇔_ (A B : Set) : Set where
  field
    to   : A → B
    from : B → A
open _⇔_ 

-- You may copy over the various reasoning modules if you wish.

-- End of code from 747Isomorphism.

-- Logical AND is Cartesian product.

data _×_ (A : Set) (B : Set) : Set where

  ⟨_,_⟩ : 
      A
    → B
      -----
    → A × B

-- Destructors (eliminators) for ×.

proj₁ : ∀ {A B : Set}
  → A × B
    -----
  → A

proj₁ A×B = {!!}

proj₂ : ∀ {A B : Set}
  → A × B
    -----
  → B

proj₂ A×B = {!!}

-- An easier (equivalent) construction using records.

record _×′_ (A B : Set) : Set where
  field
    proj₁′ : A
    proj₂′ : B
open _×′_

-- Eta-equivalence relates constructors and destructors.

η-× : ∀ {A B : Set} (w : A × B) → ⟨ proj₁ w , proj₂ w ⟩ ≡ w
η-× A×B = {!!}

-- Bool (Booleans), a type with exactly two members.

infixr 2 _×_
data Bool : Set where
  true  : Bool
  false : Bool

-- A type with three members (useful for examples).

data Tri : Set where
  aa : Tri
  bb : Tri
  cc : Tri

-- Bool × Tri has six members.
-- Here is a function counting them.

×-count : Bool × Tri → ℕ
×-count ⟨ true  , aa ⟩  =  1
×-count ⟨ true  , bb ⟩  =  2
×-count ⟨ true  , cc ⟩  =  3
×-count ⟨ false , aa ⟩  =  4
×-count ⟨ false , bb ⟩  =  5
×-count ⟨ false , cc ⟩  =  6

-- Cartesian product is commutative and associative up to isomorphism.

×-comm : ∀ {A B : Set} → A × B ≃ B × A
×-comm = {!!}

×-assoc : ∀ {A B C : Set} → (A × B) × C ≃ A × (B × C)
×-assoc = {!!}

-- 747/PLFA exercise: IffIsoIfOnlyIf (1 point)
-- Show A ⇔ B is isomorphic to (A → B) × (B → A).

iff-iso-if-onlyif : ∀ {A B : Set} → A ⇔ B ≃ (A → B) × (B → A)
iff-iso-if-onlyif = {!!}

-- Logical True is a type with one member (unit)

data ⊤ : Set where

  tt :
    --
    ⊤

η-⊤ : ∀ (w : ⊤) → tt ≡ w
η-⊤ w = {!!}

⊤-count : ⊤ → ℕ
⊤-count tt = 1

-- Unit is the left and right identity of product.

⊤-identityˡ : ∀ {A : Set} → ⊤ × A ≃ A
⊤-identityˡ = {!!}

⊤-identityʳ : ∀ {A : Set} → (A × ⊤) ≃ A
⊤-identityʳ = {!!}

-- Logical OR (disjunction) is sum (disjoint union).

data _⊎_ : Set → Set → Set where

  inj₁ : ∀ {A B : Set}
    → A
      -----
    → A ⊎ B

  inj₂ : ∀ {A B : Set}
    → B
      -----
    → A ⊎ B

-- One way to eliminate a sum.

case-⊎ : ∀ {A B C : Set}
  → (A → C)
  → (B → C)
  → A ⊎ B
    -----------
  → C
case-⊎ f g s = {!!}

-- We typically use pattern-matching to eliminate sums.

-- Eta equivalence for sums.

η-⊎ : ∀ {A B : Set} (w : A ⊎ B) → case-⊎ inj₁ inj₂ w ≡ w
η-⊎ w = {!!}

-- A generalization.

uniq-⊎ : ∀ {A B C : Set} (h : A ⊎ B → C) (w : A ⊎ B) →
  case-⊎ (h ∘ inj₁) (h ∘ inj₂) w ≡ h w
uniq-⊎ h w = {!!}

infix 1 _⊎_

-- Bool ⊎ Tri has five members

⊎-count : Bool ⊎ Tri → ℕ
⊎-count (inj₁ true)   =  1
⊎-count (inj₁ false)  =  2
⊎-count (inj₂ aa)     =  3
⊎-count (inj₂ bb)     =  4
⊎-count (inj₂ cc)     =  5

-- 747/PLFA exercise: SumCommIso (1 point)
-- Sum is commutative up to isomorphism.

⊎-comm : ∀ {A B : Set} → A ⊎ B ≃ B ⊎ A
⊎-comm = {!!}

-- 747/PLFA exercise: SumAssocIso (1 point)
-- Sum is associative up to isomorphism.

⊎-assoc : ∀ {A B C : Set} → (A ⊎ B) ⊎ C ≃ A ⊎ (B ⊎ C)
⊎-assoc = {!!}

-- Logical False is the empty type ("bottom", "empty").

data ⊥ : Set where
  -- no clauses!

-- Ex falso quodlibet "from falsehood, anything follows".

⊥-elim : ∀ {A : Set}
  → ⊥
    --
  → A

⊥-elim w = {!!}

uniq-⊥ : ∀ {C : Set} (h : ⊥ → C) (w : ⊥) → ⊥-elim w ≡ h w
uniq-⊥ h w = {!!}

⊥-count : ⊥ → ℕ
⊥-count w = {!!}

-- 747/PLFA exercise: EmptyLeftIdSumIso (1 point)
-- Empty is the left unit of sum up to isomorphism.

⊎-identityˡ : ∀ {A : Set} → ⊥ ⊎ A ≃ A
⊎-identityˡ = {!!}

-- 747/PLFA exercise: EmptyRightIdSumIso (1 point)
-- Empty is the right unit of sum up to isomorphism.

⊎-identityʳ : ∀ {A : Set} → A ⊎ ⊥ ≃ A
⊎-identityʳ = {!!}

-- Logical implication (if-then) is... the function type constructor!
-- Eliminating an if-then (modus ponens) is function application.

→-elim : ∀ {A B : Set}
  → (A → B)
  → A
    -------
  → B

→-elim L M = L M

-- This works because eta-reduction for → is built in.

η-→ : ∀ {A B : Set} (f : A → B) → (λ (x : A) → f x) ≡ f
η-→ f = refl

-- The function space A → B is sometimes called the exponential Bᴬ.
-- Bool → Tri has 3² or 9 members.

→-count : (Bool → Tri) → ℕ
→-count f with f true | f false
...          | aa     | aa      =   1
...          | aa     | bb      =   2
...          | aa     | cc      =   3
...          | bb     | aa      =   4
...          | bb     | bb      =   5
...          | bb     | cc      =   6
...          | cc     | aa      =   7
...          | cc     | bb      =   8
...          | cc     | cc      =   9

-- In math,   (p ^ n) ^ m = p ^ (n * m).
-- For types, (A ^ B) ^ C ≃ A ^ (B × C).

-- In a language where functions take multiple parameters,
-- this is called "currying".

currying : ∀ {A B C : Set} → (A → B → C) ≃ (A × B → C)
currying = {!!}

-- In math,   p ^ (n + m) = (p ^ n) * (p ^ m).
-- For types, (A ⊎ B → C) ≃ ((A → C) × (B → C)).

→-distrib-⊎ : ∀ {A B C : Set} → (A ⊎ B → C) ≃ ((A → C) × (B → C))
→-distrib-⊎ = {!!}

-- In math,   (p * n) ^ m = (p ^ m) * (n ^ m).
-- For types, (A → B × C) ≃ (A → B) × (A → C).

→-distrib-× : ∀ {A B C : Set} → (A → B × C) ≃ (A → B) × (A → C)
→-distrib-× = {!!}

-- More distributive laws.

×-distrib-⊎ : ∀ {A B C : Set} → (A ⊎ B) × C ≃ (A × C) ⊎ (B × C)
×-distrib-⊎ = {!!}

⊎-distrib-× : ∀ {A B C : Set} → (A × B) ⊎ C ≲ (A ⊎ C) × (B ⊎ C)
⊎-distrib-× = {!!}

-- Think of a counterexample to show the above isn't an isomorphism.

-- 747/PLFA exercise: ImpProdRightDist (1 point)

×-distrib-→ : ∀ {A B C : Set} → (C → (A × B)) ≃ (C → A) × (C → B)
×-distrib-→ = {!!}

-- 747/PLFA exercise: ImpSumLeftDist (1 point)

⊎-distrib-→ : ∀ {A B C : Set} → ((A ⊎ B) → C) ≃ (A → C) × (B → C)
⊎-distrib-→ = {!!}

-- PLFA exercise: a weak distributive law.
-- ⊎-weak-× : ∀ {A B C : Set} → (A ⊎ B) × C → A ⊎ (B × C)
-- ⊎-weak-× A⊎B×C = {!!}
-- State and prove the strong law, and explain the relationship.

-- 747/PLFA exercise: SumOfProdImpProdOfSum (1 point)
-- A disjunct of conjuncts implies a conjunct of disjuncts.

⊎×-implies-×⊎ : ∀ {A B C D : Set} → (A × B) ⊎ (C × D) → (A ⊎ C) × (B ⊎ D)
⊎×-implies-×⊎ A×B⊎C×D = {!!}

-- PLFA exercise: Is the converse true? If so, prove it; if not, give a counterexample.

-- See PLFA for a number of slight differences with the standard library.

-- Unicode introduced in this chapter:

{-

  ×  U+00D7  MULTIPLICATION SIGN (\x)
  ⊎  U+228E  MULTISET UNION (\u+)
  ⊤  U+22A4  DOWN TACK (\top)
  ⊥  U+22A5  UP TACK (\bot)
  η  U+03B7  GREEK SMALL LETTER ETA (\eta)
  ₁  U+2081  SUBSCRIPT ONE (\_1)
  ₂  U+2082  SUBSCRIPT TWO (\_2)
  ⇔  U+21D4  LEFT RIGHT DOUBLE ARROW (\<=>, \iff, \lr=)

-}
