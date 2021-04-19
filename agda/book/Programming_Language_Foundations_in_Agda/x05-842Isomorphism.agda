module 842Isomorphism where

-- Library

import Relation.Binary.PropositionalEquality as Eq
open Eq using (_≡_; refl; cong; cong-app; sym) -- added last
open Eq.≡-Reasoning
open import Data.Nat using (ℕ; zero; suc; _+_)
open import Data.Nat.Properties using (+-comm; +-suc; +-identityʳ) -- added last

-- Function composition.

_∘_  : ∀ {A B C : Set} → (B → C) → (A → B) → (A → C)
(g ∘ f)    x = g (f x)

_∘′_ : ∀ {A B C : Set} → (B → C) → (A → B) → (A → C)
g ∘′ f = λ x → g (f x)

postulate
  extensionality : ∀ {A B : Set} {f g : A → B}
    → (∀ (x : A) → f x ≡ g x)
      -----------------------
    → f ≡ g

-- Another definition of addition.

_+′_ : ℕ → ℕ → ℕ -- split on n instead, get different code
m +′ zero = m
m +′ suc n = suc (m +′ n)

same-app : ∀ (m n : ℕ) → m +′ n ≡ m + n
same-app m zero = sym (+-identityʳ m)
same-app m (suc n) rewrite +-suc m n | same-app m n = refl

same : _+′_ ≡ _+_  -- this requires extensionality
same = extensionality λ x → extensionality λ x₁ → same-app x x₁

-- Isomorphism.

infix 0 _≃_
record _≃_ (A B : Set) : Set where
  constructor mk-≃  -- This has been added, not in PLFA
  field
    to   : A → B
    from : B → A
    from∘to : ∀ (x : A) → from (to x) ≡ x
    to∘from : ∀ (y : B) → to (from y) ≡ y
open _≃_

-- Equivalent to the following:

data _≃′_ (A B : Set): Set where
  mk-≃′ : ∀ (to : A → B) →
          ∀ (from : B → A) →
          ∀ (from∘to : (∀ (x : A) → from (to x) ≡ x)) →
          ∀ (to∘from : (∀ (y : B) → to (from y) ≡ y)) →
          A ≃′ B

to′ : ∀ {A B : Set} → (A ≃′ B) → (A → B)
to′ (mk-≃′ f g g∘f f∘g) = f

from′ : ∀ {A B : Set} → (A ≃′ B) → (B → A)
from′ (mk-≃′ f g g∘f f∘g) = g

from∘to′ : ∀ {A B : Set} → (A≃B : A ≃′ B)
                         → (∀ (x : A)
                         → from′ A≃B (to′ A≃B x) ≡ x)
from∘to′ (mk-≃′ f g g∘f f∘g) = g∘f

to∘from′ : ∀ {A B : Set} → (A≃B : A ≃′ B)
                         → (∀ (y : B)
                         → to′ A≃B (from′ A≃B y) ≡ y)
to∘from′ (mk-≃′ f g g∘f f∘g) = f∘g

-- End of equivalent formulation (records are faster!)

-- Properties of isomorphism.

-- Reflexivity.

≃-refl : ∀ {A : Set}
    -----
  → A ≃ A

-- in empty hole, split on result, get copatterns (not in PLFA)

to ≃-refl x = x
from ≃-refl x = x
from∘to ≃-refl x = refl
to∘from ≃-refl x = refl

-- Symmetry.

≃-sym : ∀ {A B : Set}
  → A ≃ B
    -----
  → B ≃ A

to (≃-sym A≃B) = from A≃B
from (≃-sym A≃B) = to A≃B
from∘to (≃-sym A≃B) = to∘from A≃B
to∘from (≃-sym A≃B) = from∘to A≃B

-- Transitivity.

≃-trans : ∀ {A B C : Set}
  → A ≃ B
  → B ≃ C
    -----
  → A ≃ C

to (≃-trans A≃B B≃C) = to B≃C ∘ to A≃B
from (≃-trans A≃B B≃C) = from A≃B ∘ from B≃C
from∘to (≃-trans A≃B B≃C) x rewrite from∘to B≃C (to A≃B x) = from∘to A≃B x
to∘from (≃-trans A≃B B≃C) x rewrite to∘from A≃B (from B≃C x) = to∘from B≃C x

-- Isomorphism is an equivalence relation.
-- We can create syntax for equational reasoning.

module ≃-Reasoning where

  infix  1 ≃-begin_
  infixr 2 _≃⟨_⟩_
  infix  3 _≃-∎

  ≃-begin_ : ∀ {A B : Set}
    → A ≃ B
      -----
    → A ≃ B
  ≃-begin A≃B = A≃B

  _≃⟨_⟩_ : ∀ (A : Set) {B C : Set}
    → A ≃ B
    → B ≃ C
      -----
    → A ≃ C
  A ≃⟨ A≃B ⟩ B≃C = ≃-trans A≃B B≃C

  _≃-∎ : ∀ (A : Set)
      -----
    → A ≃ A
  A ≃-∎ = ≃-refl

open ≃-Reasoning

-- Embedding (weaker than isomorphism)

infix 0 _≲_
record _≲_ (A B : Set) : Set where
  field
    to      : A → B
    from    : B → A
    from∘to : ∀ (x : A) → from (to x) ≡ x
open _≲_

≲-refl : ∀ {A : Set} → A ≲ A
to ≲-refl x = x
from ≲-refl x = x
from∘to ≲-refl x = refl

≲-trans : ∀ {A B C : Set} → A ≲ B → B ≲ C → A ≲ C
to (≲-trans A≲B B≲C) = to B≲C ∘ to A≲B
from (≲-trans A≲B B≲C) = from A≲B ∘ from B≲C
from∘to (≲-trans A≲B B≲C) x rewrite from∘to B≲C (to A≲B x) = from∘to A≲B x

≲-antisym : ∀ {A B : Set}
  → (A≲B : A ≲ B)
  → (B≲A : B ≲ A)
  → (to A≲B ≡ from B≲A)
  → (from A≲B ≡ to B≲A)
    -------------------
  → A ≃ B

to (≲-antisym A≲B B≲A to≡from from≡to) = to A≲B
from (≲-antisym A≲B B≲A to≡from from≡to) = from A≲B
from∘to (≲-antisym A≲B B≲A to≡from from≡to) x = from∘to A≲B x
to∘from (≲-antisym A≲B B≲A to≡from from≡to) y
  rewrite from≡to | to≡from = from∘to B≲A y

-- Tabular reasoning for embedding.

module ≲-Reasoning where

  infix  1 ≲-begin_
  infixr 2 _≲⟨_⟩_
  infix  3 _≲-∎

  ≲-begin_ : ∀ {A B : Set}
    → A ≲ B
      -----
    → A ≲ B
  ≲-begin A≲B = A≲B

  _≲⟨_⟩_ : ∀ (A : Set) {B C : Set}
    → A ≲ B
    → B ≲ C
      -----
    → A ≲ C
  A ≲⟨ A≲B ⟩ B≲C = ≲-trans A≲B B≲C

  _≲-∎ : ∀ (A : Set)
      -----
    → A ≲ A
  A ≲-∎ = ≲-refl

open ≲-Reasoning

-- PLFA exercise: Isomorphism implies embedding.

≃-implies-≲ : ∀ {A B : Set}
  → A ≃ B
    -----
  → A ≲ B  

to (≃-implies-≲ a≃b) = to a≃b
from (≃-implies-≲ a≃b) = from a≃b
from∘to (≃-implies-≲ a≃b) = from∘to a≃b

-- PLFA exercise: propositional equivalence (weaker than embedding).

record _⇔_ (A B : Set) : Set where
  field
    to   : A → B
    from : B → A
open _⇔_ -- added

-- This is also an equivalence relation.

⇔-refl : ∀ {A : Set}
    -----
  → A ⇔ A

_⇔_.to ⇔-refl x = x
_⇔_.from ⇔-refl x = x

⇔-sym : ∀ {A B : Set}
  → A ⇔ B
    -----
  → B ⇔ A

_⇔_.to (⇔-sym A⇔B) = from A⇔B
_⇔_.from (⇔-sym A⇔B) = to A⇔B

⇔-trans : ∀ {A B C : Set}
  → A ⇔ B
  → B ⇔ C
    -----
  → A ⇔ C

to (⇔-trans A⇔B B⇔C) = to B⇔C ∘ to A⇔B
from (⇔-trans A⇔B B⇔C) = from A⇔B ∘ from B⇔C

-- 842 extended exercise: Canonical bitstrings.
-- Modified and extended from Bin-predicates exercise in PLFA Relations.

-- Copied from 842Naturals.

data Bin-ℕ : Set where
  bits : Bin-ℕ
  _x0 : Bin-ℕ → Bin-ℕ
  _x1 : Bin-ℕ → Bin-ℕ

dbl : ℕ → ℕ
dbl zero = zero
dbl (suc n) = suc (suc (dbl n))

-- Copy your versions of 'inc', 'tob', and 'fromb' over from earlier files.
-- You may choose to change the definitions here to make proofs easier.
-- But make sure to test them if you do!
-- You may also copy over any theorems that prove useful.

inc : Bin-ℕ → Bin-ℕ
inc n = {!!}

tob : ℕ → Bin-ℕ
tob n = {!!}

dblb : Bin-ℕ → Bin-ℕ
dblb n = {!!}

fromb : Bin-ℕ → ℕ
fromb n = {!!}


-- The reason that we couldn't prove ∀ {n : Bin-ℕ} → tob (fromb n) ≡ n
-- is because of the possibility of leading zeroes in a Bin-ℕ value.
-- 'bits x0 x0 x1' is such a value that gives a counterexample.
-- However, the theorem is true is true for n without leading zeroes.
-- We define a predicate to be able to state this in a theorem.
-- A value of type One n is evidence that n has a leading one.

data One : Bin-ℕ → Set where
  [bitsx1] : One (bits x1)
  _[x0] : ∀ {n : Bin-ℕ} → One n → One (n x0)
  _[x1] : ∀ {n : Bin-ℕ} → One n → One (n x1)

-- Here's a proof that 'bits x1 x0 x0' has a leading one.

_ : One (bits x1 x0 x0)
_ = [bitsx1] [x0] [x0]

-- There is no value of type One (bits x0 x0 x1).
-- But we can't state and prove this yet, because we don't know
-- how to express negation. That comes in the Connectives chapter.

-- A canonical binary representation is either zero or has a leading one.

data Can : Bin-ℕ → Set where
  [zero] : Can bits
  [pos]  : ∀ {n : Bin-ℕ} → One n → Can n

-- Some obvious examples:

_ : Can bits
_ = [zero]

_ : Can (bits x1 x0)
_ = [pos] ([bitsx1] [x0])

-- The Bin-predicates exercise in PLFA Relations gives three properties of canonicity. 
-- The first is that the increment of a canonical number is canonical.

-- Most of the work is done in the following lemma.

-- 842 exercise: IncCanOne (2 points)
-- The increment of a canonical number has a leading one.

one-inc : ∀ {n : Bin-ℕ} → Can n → One (inc n)
one-inc cn = {!!}

-- The first canonicity property is now an easy corollary.

-- 842 exercise: OneInc (1 point)

can-inc : ∀ {n : Bin-ℕ} → Can n → Can (inc n)
can-inc cn = {!!}

-- The second canonicity property is that converting a unary number
-- to binary produces a canonical number.

-- 842 exercise: CanToB (1 point)

to-can : ∀ (n : ℕ) → Can (tob n)
to-can n = {!!}

-- The third canonicity property is that converting a canonical number
-- from binary and back to unary produces the same number.

-- This takes more work, and some helper lemmas from 842Induction.
-- You will need to discover which ones.

-- 842 exercise: OneDblbX0 (1 point)

-- This helper function relates binary double to the x0 constructor,
-- for numbers with a leading one.

dblb-x0 : ∀ {n : Bin-ℕ} → One n → dblb n ≡ n x0
dblb-x0 on = {!!}

-- We can now prove the third property for numbers with a leading one.

-- 842 exercise: OneToFrom (3 points)

one-to∘from : ∀ {n : Bin-ℕ} → One n → tob (fromb n) ≡ n
one-to∘from on = {!!}

-- The third property is now an easy corollary.

-- 842 exercise: CanToFrom (1 point)

can-to∘from : ∀ {n : Bin-ℕ} → Can n → tob (fromb n) ≡ n
can-to∘from cn = {!!}

-- 842 exercise: OneUnique (2 points)
-- Proofs of positivity are unique.

one-unique : ∀ {n : Bin-ℕ} → (x y : One n) → x ≡ y
one-unique x y = {!!}

-- 842 exercise: CanUnique (1 point)
-- Proofs of canonicity are unique.

can-unique : ∀ {n : Bin-ℕ} → (x y : Can n) → x ≡ y
can-unique x y = {!!}

-- Do we have an isomorphism between ℕ (unary) and canonical binary representations?
-- Can is not a set, but a family of sets, so it doesn't quite fit
-- into our framework for isomorphism.
-- But we can roll all the values into one set which is isomorphic to ℕ.

-- A CanR value wraps up a Bin-ℕ and proof it has a canonical representation.

data CanR : Set where
  wrap : ∀ (n : Bin-ℕ) → Can n → CanR

-- We can show that there is an isomorphism between ℕ and CanR.

-- 842 exercise: IsoNCanR (3 points)

iso-ℕ-CanR : ℕ ≃ CanR
iso-ℕ-CanR = {!!}

-- Can we get an isomorphism between ℕ and some binary encoding,
-- without the awkwardness of non-canonical values?
-- Yes: we use digits 1 and 2, instead of 0 and 1 (multiplier/base is still 2).
-- This is known as bijective binary numbering.
-- The counting sequence goes <empty>, 1, 2, 11, 12, 21, 22, 111...

data Bij-ℕ : Set where
  bits : Bij-ℕ
  _x1 : Bij-ℕ → Bij-ℕ
  _x2 : Bij-ℕ → Bij-ℕ

-- There is an isomorphism between ℕ and Bij-ℕ.
-- The proof largely follows the outline of what we did above,
-- and is left as an optional exercise.

-- See PLFA for remarks on standard library definitions similar to those here.

-- Unicode introduced in this chapter:

{-

  ∘  U+2218  RING OPERATOR (\o, \circ, \comp)
  λ  U+03BB  GREEK SMALL LETTER LAMBDA (\lambda, \Gl)
  ≃  U+2243  ASYMPTOTICALLY EQUAL TO (\~-)
  ≲  U+2272  LESS-THAN OR EQUIVALENT TO (\<~)
  ⇔  U+21D4  LEFT RIGHT DOUBLE ARROW (\<=>)

-}
