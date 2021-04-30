{-# OPTIONS --allow-unsolved-metas #-}

module x05-842Isomorphism-hc where

import Relation.Binary.PropositionalEquality as Eq
open Eq using (_≡_; refl; cong; cong-app; sym; subst) -- added last
open Eq.≡-Reasoning
open import Data.Nat using (ℕ; zero; suc; _+_; _*_; _^_)
open import Data.Nat.Properties using (+-comm; +-suc; +-identityʳ) -- added last

open import x02-842Induction-hc-2 hiding (+-suc; +-identityʳ)

------------------------------------------------------------------------------
-- Function composition.

_∘_  : ∀ {A B C : Set} → (B → C) → (A → B) → (A → C)
(g ∘ f)    x = g (f x)

_∘′_ : ∀ {A B C : Set} → (B → C) → (A → B) → (A → C)
g ∘′ f = λ x → g (f x)

------------------------------------------------------------------------------
-- EXTENSIONALITY

postulate
  extensionality : ∀ {A B : Set} {f g : A → B}
    → (∀ (x : A) → f x ≡ g x)
      -----------------------
    →              f   ≡ g

-- def of + that matches on right
_+′_ : ℕ → ℕ → ℕ
m +′ zero  = m
m +′ suc n = suc (m +′ n)

same-app : ∀ (m n : ℕ) → m +′ n ≡ m + n
same-app m  zero         -- (m +′ zero) ≡ m + zero
                         --  m          ≡ m + zero
  rewrite +-identityʳ m  --  m          ≡ m
  = refl
same-app m (suc n) -- (m +′ suc n) ≡ m + suc n
                   -- suc (m +′ n) ≡ m + suc n
  rewrite
    +-suc m n      -- suc (m +′ n) ≡ suc (m + n)
  | same-app m n   -- suc (m +  n) ≡ suc (m + n)
  = refl

-- requires extensionality
+′-same-as-+ : _+′_ ≡ _+_
+′-same-as-+ = extensionality λ m → extensionality λ n → same-app m n

------------------------------------------------------------------------------
-- ISOMORPHISM

infix 0 _≃_
record _≃_ (A B : Set) : Set where
  constructor mk-≃  -- added, not in PLFA
  field
    to      : A → B
    from    : B → A
    from∘to : ∀ (x : A) → from (to x) ≡ x
    to∘from : ∀ (y : B) → to (from y) ≡ y
open _≃_

-- RECORD def equivalent to DATA def and functions:

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

------------------------------------------------------------------------------
-- PROPERTIES OF ISOMORPHISM : AN EQUIVALENCE RELATION (i.e., reflexive, symmetric and transitive)

-- reflexive
≃-refl : ∀ {A : Set}
    -----
  → A ≃ A

-- in empty hole, split on result, get COPATTERNS (not in PLFA)

to      ≃-refl x = x
from    ≃-refl x = x
from∘to ≃-refl x = refl
to∘from ≃-refl x = refl

-- symmetric
≃-sym : ∀ {A B : Set}
  → A ≃ B
    -----
  → B ≃ A

to      (≃-sym A≃B) = from    A≃B
from    (≃-sym A≃B) = to      A≃B
from∘to (≃-sym A≃B) = to∘from A≃B
to∘from (≃-sym A≃B) = from∘to A≃B

-- transitive
≃-trans : ∀ {A B C : Set}
  → A ≃ B
  →     B ≃ C
    -----
  → A ≃     C

to      (≃-trans A≃B B≃C) = to   B≃C ∘ to   A≃B
from    (≃-trans A≃B B≃C) = from A≃B ∘ from B≃C
from∘to (≃-trans A≃B B≃C) x rewrite from∘to B≃C (to   A≃B x) = from∘to A≃B x
to∘from (≃-trans A≃B B≃C) x rewrite to∘from A≃B (from B≃C x) = to∘from B≃C x


-- syntax for isomorphic equational reasoning
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
    →     B ≃ C
      -----
    → A ≃     C
  A ≃⟨ A≃B ⟩ B≃C = ≃-trans A≃B B≃C

  _≃-∎ : ∀ (A : Set)
      -----
    → A ≃ A
  A ≃-∎ = ≃-refl

open ≃-Reasoning

------------------------------------------------------------------------------
-- EMBEDDING (weaker than isomorphism)

infix 0 _≲_
record _≲_ (A B : Set) : Set where
  field
    to      : A → B
    from    : B → A
    from∘to : ∀ (x : A) → from (to x) ≡ x
open _≲_

≲-refl : ∀ {A : Set} → A ≲ A
to      ≲-refl x = x
from    ≲-refl x = x
from∘to ≲-refl x = refl

≲-trans : ∀ {A B C : Set} → A ≲ B → B ≲ C → A ≲ C
to      (≲-trans A≲B B≲C) = to   B≲C ∘ to   A≲B
from    (≲-trans A≲B B≲C) = from A≲B ∘ from B≲C
from∘to (≲-trans A≲B B≲C) x rewrite from∘to B≲C (to A≲B x) = from∘to A≲B x

≲-antisym : ∀ {A B : Set}
  → (A≲B : A ≲ B)
  → (B≲A : B ≲ A)
  → (to   A≲B ≡ from B≲A)
  → (from A≲B ≡ to   B≲A)
    -------------------
  → A ≃ B

to      (≲-antisym A≲B B≲A to≡from from≡to)   = to      A≲B
from    (≲-antisym A≲B B≲A to≡from from≡to)   = from    A≲B
from∘to (≲-antisym A≲B B≲A to≡from from≡to) x = from∘to A≲B x
to∘from (≲-antisym A≲B B≲A to≡from from≡to) y
  rewrite from≡to | to≡from                   = from∘to B≲A y

-- equational reasoning for embedding
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

------------------------------------------------------------------------------
-- isomorphism implies embedding

≃-implies-≲ : ∀ {A B : Set}
  → A ≃ B
    -----
  → A ≲ B

to      (≃-implies-≲ a≃b) = to      a≃b
from    (≃-implies-≲ a≃b) = from    a≃b
from∘to (≃-implies-≲ a≃b) = from∘to a≃b

------------------------------------------------------------------------------
-- PROPOSITIONAL EQUIVALENCE (weaker than embedding) - aka IFF
-- an equivalence relation

record _⇔_ (A B : Set) : Set where
  field
    to   : A → B
    from : B → A
open _⇔_ -- added

⇔-refl : ∀ {A : Set}
    -----
  → A ⇔ A

_⇔_.to   ⇔-refl x = x
_⇔_.from ⇔-refl x = x

⇔-sym : ∀ {A B : Set}
  → A ⇔ B
    -----
  → B ⇔ A

_⇔_.to   (⇔-sym A⇔B) = from A⇔B
_⇔_.from (⇔-sym A⇔B) = to   A⇔B

⇔-trans : ∀ {A B C : Set}
  → A ⇔ B
  →     B ⇔ C
    -----
  → A ⇔     C

to   (⇔-trans A⇔B B⇔C) = to   B⇔C ∘ to   A⇔B
from (⇔-trans A⇔B B⇔C) = from A⇔B ∘ from B⇔C

{-
------------------------------------------------------------------------------
-- 842 extended exercise: Canonical bitstrings.
-- Modified and extended from Bin-predicates exercise in PLFA Relations.

cannot prove
              ∀ {n : Bin-ℕ} → tob (fromb n) ≡ n
because of possibility of leading zeroes in Bin-ℕ value (e.g., 'bits x0 x0 x1')

It s true for Bin-ℕ without leading zeroes
-}
-- Predicate that states:
-- value of type One n is evidence that n has a leading one.
data One : Bin-ℕ → Set where
  [bitsx1] :                         One (bits x1)
  _[x0]    : ∀ {b : Bin-ℕ} → One b → One (b x0)
  _[x1]    : ∀ {b : Bin-ℕ} → One b → One (b x1)

-- proof that 'bits x1 x0 x0' has leading one
_ : One (bits x1 x0 x0)
_ = [bitsx1] [x0] [x0]

-- There is no value of type One (bits x0 x0 x1).
-- Can't state and prove yet, because negation not covered until Connectives chapter.

-- Canonical binary representation is either zero or has a leading one.
data Can : Bin-ℕ → Set where
  [zero] :                         Can bits
  [pos]  : ∀ {b : Bin-ℕ} → One b → Can b

_ : Can bits
_ = [zero]

_ : Can (bits x1 x0)
_ = [pos] ([bitsx1] [x0])

-- PLFA Relations Bin-predicates exercise gives three properties of canonicity.
-- First : increment of a canonical number is canonical.
-- Most of the work is done in the following lemma.

-- IncCanOne : increment of a canonical number has a leading one.
one-inc : ∀ {b : Bin-ℕ} → Can b → One (inc b)
one-inc [zero]           =          [bitsx1] -- note: 'n' is bound to 'bits'
one-inc ([pos] [bitsx1]) =          [bitsx1] [x0]
one-inc ([pos] (b [x0])) =                b  [x1]
one-inc ([pos] (b [x1])) = one-inc ([pos] b) [x0]

-- OneInc : 1st canonicity property is now a corollary.
can-inc : ∀ {b : Bin-ℕ} → Can b → Can (inc b)
can-inc cb = [pos] (one-inc cb)

-- CanToB : 2nd canonicity property : converting unary to binary produces a canonical number.
to-can : ∀ (n : ℕ) → Can (tob n)
to-can   zero  = [zero]
to-can (suc n) = can-inc (to-can n)

-- OneDblbX0 : relates binary double to the x0 constructor, for numbers with a leading one.
dblb-x0 : ∀ {b : Bin-ℕ} → One b → dblb b ≡ b x0
dblb-x0 [bitsx1]                     = refl
dblb-x0 (on [x0]) rewrite dblb-x0 on = refl
dblb-x0 (on [x1]) rewrite dblb-x0 on = refl

dblb-x1 : ∀ {b : Bin-ℕ} → One b → inc (dblb b) ≡ b x1
dblb-x1 [bitsx1]  = refl
dblb-x1 (on [x0])           --  inc (dblb (b  x0))    ≡ ((b x0) x1)
                            --      (dblb  b  x1)     ≡ ((b x0) x1)
  rewrite sym (dblb-x0 on)  --      (dblb  b  x1)     ≡ (dblb b x1)
  = refl
dblb-x1 (on [x1])           --  inc (dblb (b  x1))    ≡ ((b x1) x1)
                            -- (inc (dblb  b) x1)     ≡ ((b x1) x1)
  rewrite dblb-x1 on        --           ((b  x1) x1) ≡ ((b x1) x1)
  = refl

-- To prove 3rd property, use lemmas from 842Induction
-- 'one-to∘from' below and some new ones you define.

one-bits-to-nat : ∀ {b : Bin-ℕ} → One b → ℕ
one-bits-to-nat {b} _ = fromb b

one-to∘from : ∀ {b : Bin-ℕ} → One b → tob (fromb b) ≡ b
one-to∘from [bitsx1]  = refl
one-to∘from {b} (on [x0])       --      tob      (fromb (b  x0))  ≡ (b  x0)
                                --      tob  (dbl (fromb b ))     ≡ (b  x0)
  rewrite
    sym (dblb-x0 on)            --      tob  (dbl (fromb b ))     ≡ dblb b
  | to∘dbl (one-bits-to-nat on) --      dblb (tob (fromb b₁))     ≡ dblb b₁
  | one-to∘from on              --      dblb              lhs     ≡ dblb lhs
  = refl
one-to∘from {b} (on [x1])       --      tob      (fromb (b₁ x1))  ≡ (b₁ x1)
                                -- inc (tob (dbl (fromb  b₁)))    ≡ (b₁ x1)
  rewrite
    to∘dbl (one-bits-to-nat on) -- inc (dblb (tob (fromb b₁)))    ≡ (b₁ x1)
  | one-to∘from on              -- inc (dblb lhs)                 ≡ (lhs x1)
  | dblb-x1 on                  --          (lhs x1)              ≡ (lhs x1)
  = refl

-- 3rd canonicity property :
-- converting a canonical number from binary and back to unary produces the same number.
-- CanToFrom
can-to∘from : ∀ {b : Bin-ℕ} → Can b → tob (fromb b) ≡ b
can-to∘from [zero]    = refl
can-to∘from ([pos] b) = one-to∘from b

-- Proofs of positivity are unique.
one-unique : ∀ {b : Bin-ℕ} → (ox oy : One b) → ox ≡ oy
one-unique [bitsx1]  [bitsx1]  = refl
one-unique (ox [x0]) (oy [x0])
  with one-unique ox oy
... | refl = refl
one-unique (ox [x1]) (oy [x1])
  with one-unique ox oy
... | refl = refl

-- Proofs of canonicity are unique.
can-unique : ∀ {b : Bin-ℕ} → (cx cy : Can b) → cx ≡ cy
can-unique [zero]     [zero]     = refl
can-unique ([pos] ox) ([pos] oy)
  with one-unique ox oy
... | refl = refl

------------------------------------------------------------------------------
-- There is NOT an isomorphism between ℕ (unary) and canonical binary representations.
-- Because Can is not a set, but a family of sets, it does not fit into framework for isomorphism.

-- It is possible to roll all the values into one set which is isomorphic to ℕ.
-- A CanR VALUE wraps a Bin-ℕ and proof it has a canonical representation.
data CanR : Set where
  wrap : ∀ (b : Bin-ℕ) → Can b → CanR

-- IsoNCanR : Prove an isomorphism between ℕ and CanR.
iso-ℕ-CanR : ℕ ≃ CanR
to      iso-ℕ-CanR          n  = wrap (tob n) (to-can n) -- -- Goal: CanR; n : ℕ
from    iso-ℕ-CanR (wrap b cb) = fromb b -- Goal: ℕ; cb : Can b, b  : Bin-ℕ
                                         -- Constraints
                                         -- fromb (tob n) = ?0 (b = (tob n)) (cb = (to-can n)) : ℕ
from∘to iso-ℕ-CanR          n  = from∘tob n -- Goal: fromb (tob n) ≡ n; n : ℕ
to∘from iso-ℕ-CanR (wrap b cb)  -- Goal: to iso-ℕ-CanR (from iso-ℕ-CanR (wrap b cb)) ≡ wrap b cb
                                -- Goal: wrap (tob (fromb b)) (to-can (fromb b))     ≡ wrap b cb
                                -- cb : Can b, b  : Bin-ℕ
  with to-can (fromb b) | can-to∘from cb
... |  tcfb             | ctfcb -- Goal: wrap (tob (fromb b)) tcfb ≡ wrap b cb
                                -- ctfcb : tob (fromb b) ≡ b, tcfb  : Can (tob (fromb b))
  rewrite
    ctfcb                       -- Goal: wrap b tcfb ≡ wrap b cb
  | can-unique cb tcfb          -- Goal: wrap b tcfb ≡ wrap b tcfb
  = refl

------------------------------------------------------------------------------
-- BIJECTIVE BINARY NUMBERING -- TODO
-- Isomorphism between ℕ and some binary encoding (without awkward non-canonical values).
-- Via using digits 1 and 2 (instead of 0 and 1) where the multiplier/base is still 2.
-- e.g., <empty>, 1, 2, 11, 12, 21, 22, 111, ...

data Bij-ℕ : Set where
  bits :         Bij-ℕ
  _x1  : Bij-ℕ → Bij-ℕ
  _x2  : Bij-ℕ → Bij-ℕ

--------------------------------------------------
incBij-ℕ : Bij-ℕ → Bij-ℕ
incBij-ℕ   bits  = bits x1
incBij-ℕ (bj x1) = bj x2
incBij-ℕ (bj x2) = (incBij-ℕ bj) x1

_ : incBij-ℕ  bits        ≡ bits       x1
_ = refl
_ : incBij-ℕ (bits    x1) ≡ bits       x2
_ = refl
_ : incBij-ℕ (bits    x2) ≡ bits    x1 x1
_ = refl
_ : incBij-ℕ (bits x1 x1) ≡ bits    x1 x2
_ = refl
_ : incBij-ℕ (bits x1 x2) ≡ bits    x2 x1
_ = refl
_ : incBij-ℕ (bits x2 x1) ≡ bits    x2 x2
_ = refl
_ : incBij-ℕ (bits x2 x2) ≡ bits x1 x1 x1
_ = refl

--------------------------------------------------
toBij-ℕ : ℕ → Bij-ℕ
toBij-ℕ  zero   = bits
toBij-ℕ (suc n) = incBij-ℕ (toBij-ℕ n)

_ : toBij-ℕ  0 ≡ bits
_ = refl
_ : toBij-ℕ  1 ≡ bits          x1
_ = refl
_ : toBij-ℕ  2 ≡ bits          x2
_ = refl
_ : toBij-ℕ  3 ≡ bits       x1 x1
_ = refl
_ : toBij-ℕ  4 ≡ bits       x1 x2
_ = refl
_ : toBij-ℕ  5 ≡ bits       x2 x1
_ = refl
_ : toBij-ℕ  6 ≡ bits       x2 x2
_ = refl
_ : toBij-ℕ  7 ≡ bits    x1 x1 x1
_ = refl
_ : toBij-ℕ  8 ≡ bits    x1 x1 x2
_ = refl
_ : toBij-ℕ  9 ≡ bits    x1 x2 x1
_ = refl
_ : toBij-ℕ 10 ≡ bits    x1 x2 x2
_ = refl
_ : toBij-ℕ 11 ≡ bits    x2 x1 x1
_ = refl
_ : toBij-ℕ 12 ≡ bits    x2 x1 x2
_ = refl
_ : toBij-ℕ 14 ≡ bits    x2 x2 x2
_ = refl
_ : toBij-ℕ 16 ≡ bits x1 x1 x1 x2
_ = refl
_ : toBij-ℕ 18 ≡ bits x1 x1 x2 x2
_ = refl
_ : toBij-ℕ 20 ≡ bits x1 x2 x1 x2
_ = refl
-------------------------
_ : toBij-ℕ  0 ≡ bits
_ = refl
_ : toBij-ℕ  0 ≡ bits
_ = refl
-------------------------
_ : toBij-ℕ  1 ≡ bits          x1
_ = refl
_ : toBij-ℕ  2 ≡ bits          x2
_ = refl
-------------------------
_ : toBij-ℕ  2 ≡ bits          x2
_ = refl
_ : toBij-ℕ  4 ≡ bits       x1 x2
_ = refl
-------------------------
_ : toBij-ℕ  3 ≡ bits       x1 x1
_ = refl
_ : toBij-ℕ  6 ≡ bits       x2 x2
_ = refl
-------------------------
_ : toBij-ℕ  4 ≡ bits       x1 x2
_ = refl
_ : toBij-ℕ  8 ≡ bits    x1 x1 x2
_ = refl
-------------------------
_ : toBij-ℕ  5 ≡ bits       x2 x1
_ = refl
_ : toBij-ℕ 10 ≡ bits    x1 x2 x2
_ = refl
-------------------------
_ : toBij-ℕ  6 ≡ bits       x2 x2
_ = refl
_ : toBij-ℕ 12 ≡ bits    x2 x1 x2
_ = refl
-------------------------
_ : toBij-ℕ  7 ≡ bits    x1 x1 x1
_ = refl
_ : toBij-ℕ 14 ≡ bits    x2 x2 x2
_ = refl
-------------------------
_ : toBij-ℕ  8 ≡ bits    x1 x1 x2
_ = refl
_ : toBij-ℕ 16 ≡ bits x1 x1 x1 x2
_ = refl
-------------------------
_ : toBij-ℕ  9 ≡ bits    x1 x2 x1
_ = refl
_ : toBij-ℕ 18 ≡ bits x1 x1 x2 x2
_ = refl
-------------------------
_ : toBij-ℕ 10 ≡ bits    x1 x2 x2
_ = refl
_ : toBij-ℕ 20 ≡ bits x1 x2 x1 x2
_ = refl
-------------------------

--------------------------------------------------
fromBij-ℕ : Bij-ℕ → ℕ
fromBij-ℕ  bj = fbj bj 0
 where
  fbj : Bij-ℕ → ℕ → ℕ
  fbj bits    _ = zero
  fbj (bj x1) n = (1 * (2 ^ n)) + fbj bj (n + 1)
  fbj (bj x2) n = (2 * (2 ^ n)) + fbj bj (n + 1)

_ : fromBij-ℕ (bits) ≡ 0
_ = refl
_ : fromBij-ℕ (bits          x1)  ≡  1
_ = refl
_ : fromBij-ℕ (bits          x2)  ≡  2
_ = refl
_ : fromBij-ℕ (bits       x1 x1)  ≡  3
_ = refl
_ : fromBij-ℕ (bits       x1 x2)  ≡  4
_ = refl
_ : fromBij-ℕ (bits       x2 x1)  ≡  5
_ = refl
_ : fromBij-ℕ (bits       x2 x2)  ≡  6
_ = refl
_ : fromBij-ℕ (bits    x1 x1 x1)  ≡  7
_ = refl
_ : fromBij-ℕ (bits    x1 x1 x2)  ≡  8
_ = refl
_ : fromBij-ℕ (bits    x1 x2 x1)  ≡  9
_ = refl
_ : fromBij-ℕ (bits    x1 x2 x2)  ≡ 10
_ = refl
_ : fromBij-ℕ (bits x1 x2 x1 x2)  ≡ 20
_ = refl

--------------------------------------------------
doubleBij-ℕ : Bij-ℕ → Bij-ℕ
doubleBij-ℕ  bits   = bits
doubleBij-ℕ (bj x1) = {!!}
doubleBij-ℕ (bj x2) = {!!}

--------------------------------------------------
xxx : ∀ {n : ℕ} → fromBij-ℕ (toBij-ℕ n) ≡ n
xxx {zero}  = refl
xxx {suc n} = {! !}

--------------------------------------------------
-- Prove the isomorphism between ℕ and Bij-ℕ.
-- Largely follows outline of above.

iso-ℕ-Bij-ℕ : ℕ ≃ Bij-ℕ
to      iso-ℕ-Bij-ℕ n   = toBij-ℕ n
from    iso-ℕ-Bij-ℕ bjn = fromBij-ℕ bjn
from∘to iso-ℕ-Bij-ℕ n   = {!!}
to∘from iso-ℕ-Bij-ℕ bjn = {!!}
