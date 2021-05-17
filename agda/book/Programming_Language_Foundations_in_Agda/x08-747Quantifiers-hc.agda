module x08-747Quantifiers-hc where

-- Library

import Relation.Binary.PropositionalEquality as Eq
open Eq using (_≡_; refl; cong; sym)
open import Data.Nat using (ℕ; zero; suc; _+_; _*_; _≤_; z≤n; s≤s) -- added ≤
-- open import Data.Nat.Properties using (≤-refl)
open import Relation.Nullary using (¬_)
open import Data.Product using (_×_; proj₁; proj₂) renaming (_,_ to ⟨_,_⟩) -- added proj₂
open import Data.Sum using (_⊎_; inj₁; inj₂ ) -- added inj₁, inj₂
open import Function using (_∘_) -- added

-- BEGIN: Copied from 747Isomorphism.
postulate
  extensionality : ∀ {A B : Set} {f g : A → B}
    → (∀ (x : A) → f x ≡ g x)
      -----------------------
    → f ≡ g

infix 0 _≃_
record _≃_ (A B : Set) : Set where
  constructor mk-≃
  field
    to   : A → B
    from : B → A
    from∘to : ∀ (x : A) → from (to x) ≡ x
    to∘from : ∀ (y : B) → to (from y) ≡ y
open _≃_

record _⇔_ (A B : Set) : Set where
  field
    to   : A → B
    from : B → A
open _⇔_
-- END: Copied from 747Isomorphism.

-- Logical forall is ∀.
-- Forall elimination is function application.
∀-elim : ∀ {A : Set} {B : A → Set}
  → (L : ∀ (x : A) → B x)
  → (M : A)
    -----------------
  → B M
∀-elim L M = L M

-- A → B is nicer syntax for ∀ (_ : A) → B.

-- 747/PLFA exercise: ForAllDistProd (1 point)
-- ∀ distributes over ×
-- (note: → distributes over × was shown in Connectives)

∀-distrib-× : ∀ {A : Set} {B C : A → Set} →
  (∀ (a : A) → B a × C a) ≃ (∀ (a : A) → B a) × (∀ (a : A) → C a)
to      ∀-distrib-× a→Ba×Ca         = ⟨ proj₁ ∘ a→Ba×Ca , proj₂ ∘ a→Ba×Ca ⟩
from    ∀-distrib-× ⟨ a→ba , a→ca ⟩ = λ a → ⟨ a→ba a , a→ca a ⟩
from∘to ∀-distrib-× a→Ba×Ca         = refl
to∘from ∀-distrib-× ⟨ a→ba , a→ca ⟩ = refl

-- 747/PLFA exercise: SumForAllImpForAllSum (1 point)
-- disjunction of foralls implies a forall of disjunctions
⊎∀-implies-∀⊎ : ∀ {A : Set} {B C : A → Set} →
  (∀ (a : A) → B a) ⊎ (∀ (a : A) → C a)  →  ∀ (a : A) → B a ⊎ C a
⊎∀-implies-∀⊎ (inj₁ a→ba) = inj₁ ∘ a→ba
⊎∀-implies-∀⊎ (inj₂ a→ca) = inj₂ ∘ a→ca

-- Existential quantification
-- a pair:
-- - a witness and
-- - a proof that the witness satisfies the property
data Σ (A : Set) (B : A → Set) : Set where
  ⟨_,_⟩ : (a : A) → B a → Σ A B

-- convenient syntax

Σ-syntax = Σ
infix 2 Σ-syntax
syntax Σ-syntax A (λ x → B) = Σ[ x ∈ A ] B

-- can use the RHS syntax in code,
-- but LHS will show up in displays of goal and context.
-- This syntax is equivalent to defining a dependent record type.

record Σ′ (A : Set) (B : A → Set) : Set where
  field
    proj₁′ : A
    proj₂′ : B proj₁′

-- convention : library uses ∃ when domain of bound variable is implicit

∃ : ∀ {A : Set} (B : A → Set) → Set
∃ {A} B = Σ A B

-- syntax

∃-syntax = ∃
syntax ∃-syntax (λ x → B) = ∃[ x ] B

-- eliminate existential with a function
-- that consumes the witness and proof
-- and reaches a conclusion C
∃-elim : ∀ {A : Set} {B : A → Set} {C : Set}
  → (∀ a → B a → C)
  → ∃[ a ] B a
    ---------------
  → C
∃-elim a→Ba→C ⟨ a , Ba ⟩ = a→Ba→C a Ba

-- generalization of currying (from Connectives)
-- currying : ∀ {A B C : Set} → (A → B → C) ≃ (A × B → C)
∀∃-currying : ∀ {A : Set} {B : A → Set} {C : Set}
  → (∀ a → B a → C) ≃ (∃[ a ] B a → C)
_≃_.to      ∀∃-currying a→Ba→C ⟨ a , Ba ⟩ = a→Ba→C a Ba
_≃_.from    ∀∃-currying ∃xB a Ba          = ∃xB ⟨ a , Ba ⟩
_≃_.from∘to ∀∃-currying a→Ba→C            = refl
_≃_.to∘from ∀∃-currying ∃xB               = extensionality λ { ⟨ a , Ba ⟩ → refl}

-- 747/PLFA exercise: ExistsDistSum (2 points)
-- existentials distribute over disjunction
∃-distrib-⊎ : ∀ {A : Set} {B C : A → Set} →
  ∃[ a ] (B a ⊎ C a) ≃ (∃[ a ] B a) ⊎ (∃[ a ] C a)
to      ∃-distrib-⊎       ⟨ a , inj₁ Ba ⟩  = inj₁ ⟨ a ,      Ba ⟩
to      ∃-distrib-⊎       ⟨ a , inj₂ Ca ⟩  = inj₂ ⟨ a ,      Ca ⟩
from    ∃-distrib-⊎ (inj₁ ⟨ a ,      Ba ⟩) =      ⟨ a , inj₁ Ba ⟩
from    ∃-distrib-⊎ (inj₂ ⟨ a ,      Ca ⟩) =      ⟨ a , inj₂ Ca ⟩
from∘to ∃-distrib-⊎       ⟨ a , inj₁ Ba ⟩  = refl
from∘to ∃-distrib-⊎       ⟨ a , inj₂ Ca ⟩  = refl
to∘from ∃-distrib-⊎ (inj₁ ⟨ a ,      Ba ⟩) = refl
to∘from ∃-distrib-⊎ (inj₂ ⟨ a ,      Ca ⟩) = refl

-- 747/PLFA exercise: ExistsProdImpProdExists (1 point)
-- existentials distribute over ×
∃×-implies-×∃ : ∀ {A : Set} {B C : A → Set} →
  ∃[ a ] (B a × C a) → (∃[ a ] B a) × (∃[ a ] C a)
∃×-implies-×∃ ⟨ a , ⟨ Ba , Ca ⟩ ⟩ = ⟨ ⟨ a , Ba ⟩ , ⟨ a , Ca ⟩ ⟩

-- existential example: revisiting even/odd.
-- mutually-recursive definitions of even and odd.
data even : ℕ → Set
data odd  : ℕ → Set

data even where

  even-zero : even zero

  even-suc : ∀ {n : ℕ}
    → odd n
      ------------
    → even (suc n)

data odd where
  odd-suc : ∀ {n : ℕ}
    → even n
      -----------
    → odd (suc n)

-- number is even iff it is          double some other number
even-∃ : ∀ {n : ℕ} → even n → ∃[ m ] (    m * 2 ≡ n)
-- number is odd  iff it is one plus double some other number
odd-∃  : ∀ {n : ℕ} →  odd n → ∃[ m ] (1 + m * 2 ≡ n)

even-∃  even-zero = ⟨ zero , refl ⟩
even-∃ (even-suc odd-suc-x*2)
    with odd-∃ odd-suc-x*2
... | ⟨ x , refl ⟩ = ⟨ suc x , refl ⟩
odd-∃  (odd-suc  even-x*2)
    with even-∃ even-x*2
... | ⟨ x , refl ⟩ = ⟨     x , refl ⟩

∃-even : ∀ {n : ℕ} → ∃[ m ] (    m * 2 ≡ n) → even n
∃-odd  : ∀ {n : ℕ} → ∃[ m ] (1 + m * 2 ≡ n) →  odd n

∃-even ⟨ zero  , refl ⟩ = even-zero
∃-even ⟨ suc x , refl ⟩ = even-suc (∃-odd  ⟨ x , refl ⟩)
∃-odd  ⟨     x , refl ⟩ = odd-suc  (∃-even ⟨ x , refl ⟩)

-- PLFA exercise: what if we write the arithmetic more "naturally"?
-- (Proof gets harder but is still doable).

-- 747/PLFA exercise: AltLE (3 points)
-- alternate definition of y ≤ z
-- (Optional exercise: Is this an isomorphism?)
+0 : ∀ (m : ℕ) → m + zero ≡ m
+0  zero   = refl
+0 (suc m) = cong suc (+0 m)

open import x03-842Relations-hc-2 using (s≤s)

xxx : ∀ {m n : ℕ} → m ≤ n → suc (m + zero) ≤ suc n
xxx {m} {n} p rewrite +0 m = s≤s p

≤-refl : ∀ {n : ℕ}
    -----
  → n ≤ n
≤-refl {zero}  = z≤n
≤-refl {suc n} = s≤s (≤-refl {n})

+-suc : ∀ (m n : ℕ) → m + suc n ≡ suc (m + n)
+-suc  zero   n = refl
+-suc (suc m) n = cong suc (+-suc m n)

aaa : ∀ {x y z : ℕ} → y + suc x ≡ z → y ≤ y + suc x
--aaa {x} {y} y+sucx≡z rewrite sym y+sucx≡z {- | +-suc y x -} = {!!}
aaa {zero}  {zero}  refl = z≤n
aaa {suc x} {zero}  refl = z≤n
aaa {zero}  {suc y} refl = s≤s {!!}
aaa {suc x} {suc y} refl = s≤s {!!}

zzz : ∀ {y z : ℕ} → y + zero ≡ z → y ≤ z
zzz {y} y+x≡z rewrite +0 y | y+x≡z | sym y+x≡z = ≤-refl {y}

∃-≤ : ∀ {y z : ℕ} → ( (y ≤ z) ⇔ ( ∃[ x ] (y + x ≡ z) ) )
{-
∃-≤ {y} {z} = record { to = λ { z≤n → ⟨ zero , {!!} ⟩ ; (s≤s y≤z) → {!!} }; from = {!!} }
-}
to   ∃-≤           z≤n        = ⟨          zero  , {!!} ⟩
to   ∃-≤      (s≤s z≤n)       = ⟨      suc zero  , {!!} ⟩
to   ∃-≤ (s≤s (s≤s m≤n))      = ⟨ suc (suc zero) , {!!} ⟩
from ∃-≤ ⟨ zero  , y+zero≡z ⟩ = zzz y+zero≡z
from ∃-≤ ⟨ suc x , y+sucx≡z ⟩ rewrite sym y+sucx≡z = {!!}

-- The negation of an existential is isomorphic to a universal of a negation.
¬∃≃∀¬ : ∀ {A : Set} {B : A → Set}
  → (¬ ∃[ a ] B a) ≃ ∀ a → ¬ B a
to      ¬∃≃∀¬ = λ { ¬∃B     a → λ Ba   → ¬∃B ⟨ a , Ba ⟩ }
from    ¬∃≃∀¬ = λ { a→¬Ba ⟨ a ,   Ba ⟩ → a→¬Ba a   Ba }
from∘to ¬∃≃∀¬ = λ   ¬∃B                → {!!} -- TODO
to∘from ¬∃≃∀¬ = λ   a→¬Ba              → refl

-- 747/PLFA exercise: ExistsNegImpNegForAll (1 point)
-- Existence of negation implies negation of universal.

∃¬-implies-¬∀ : ∀ {A : Set} {B : A → Set}
  → ∃[ a ] (¬ B a)
    --------------
  → ¬ (∀ a → B a)
∃¬-implies-¬∀ ⟨ a , ¬Ba ⟩ = λ a→Ba → ¬Ba (a→Ba a)

-- The converse cannot be proved in intuitionistic logic.

-- PLFA exercise: isomorphism between naturals and existence of canonical binary.
-- This is essentially what we did at the end of 747Isomorphism.

