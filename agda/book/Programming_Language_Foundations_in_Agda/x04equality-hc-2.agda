
---------------------------------------------------------------------------------
-- Equality

data _≡_ {A : Set} (x : A) : A → Set where
  refl : x ≡ x
{-# BUILTIN EQUALITY _≡_ #-}
infix 4 _≡_

------------------------------------------------------------------------------
-- Equality is an equivalence relation : reflexive, symmetric, and transitive.
-- Reflexivity is built-in to the definition of equality.

sym : ∀ {A : Set} {x y : A}
  → x ≡ y
    -----
  → y ≡ x
sym refl = refl

trans : ∀ {A : Set} {x y z : A}
  → x ≡ y
  →     y ≡ z
    -----
  → x ≡     z
trans refl refl  =  refl

-- Equality satisfies congruence.
-- If two terms are equal, they are eual after the same function is applied to both:
cong : ∀ {A B : Set} (f : A → B) {x y : A}
  → x ≡ y
    ---------
  → f x ≡ f y
cong f refl  =  refl

-- Congruence of 2 arg functions
cong₂ : ∀ {A B C : Set} (f : A → B → C) {u x : A} {v y : B}
  → u ≡ x
  → v ≡ y
    -------------
  → f u v ≡ f x y
cong₂ f refl refl  =  refl

-- Equality is a congruence in the function position of an application.
-- If two functions are equal, then applying them to the same term yields equal terms:
cong-app : ∀ {A B : Set} {f g : A → B}
  → f ≡ g
    ---------------------
  → ∀ (x : A) → f x ≡ g x
cong-app refl x = refl


-- Equality satisfies substitution.
-- If two values are equal and a predicate holds of the first then it also holds of the second:
subst : ∀ {A : Set} {x y : A} (P : A → Set)
  → x ≡ y
    ---------
  → P x → P y
subst P refl px = px

-- Chains of equations : support equational reasoning
module ≡-Reasoning {A : Set} where

  infix  1 begin_
  infixr 2 _≡⟨⟩_ _≡⟨_⟩_
  infix  3 _∎

  begin_ : ∀ {x y : A}
    → x ≡ y
      -----
    → x ≡ y
  begin x≡y  =  x≡y

  _≡⟨⟩_ : ∀ (x : A) {y : A}
    → x ≡ y
      -----
    → x ≡ y
  x ≡⟨⟩ x≡y  =  x≡y

  _≡⟨_⟩_ : ∀ (x : A) {y z : A}
    → x ≡ y
    → y ≡ z
      -----
    → x ≡ z
  x ≡⟨ x≡y ⟩ y≡z  =  trans x≡y y≡z

  _∎ : ∀ (x : A)
      -----
    → x ≡ x
  x ∎  =  refl

open ≡-Reasoning

-- equational proof of transitivity
trans′ : ∀ {A : Set} {x y z : A}
  → x ≡ y
  → y ≡ z
    -----
  → x ≡ z
trans′ {A} {x} {y} {z} x≡y y≡z =
  begin
    x
  ≡⟨ x≡y ⟩
    y
  ≡⟨ y≡z ⟩
    z
  ∎

{-
Exercise trans and ≡-Reasoning (practice): TODO
Cannot use the definition of trans’ using ≡-Reasoning as the definition for trans.
WHY?
Hint: look at the definition of _≡⟨_⟩_
-}

data ℕ : Set where
  zero : ℕ
  suc  : ℕ → ℕ

_+_ : ℕ → ℕ → ℕ
zero    + n  =  n
(suc m) + n  =  suc (m + n)

postulate
  +-identity : ∀ (m : ℕ) → m + zero ≡ m
  +-suc : ∀ (m n : ℕ) → m + suc n ≡ suc (m + n)

+-comm : ∀ (m n : ℕ) → m + n ≡ n + m
+-comm m zero =
  begin
    m + zero
  ≡⟨ +-identity m ⟩
    m
  ≡⟨⟩
    zero + m
  ∎
+-comm m (suc n) =
  begin
    m + suc n
  ≡⟨ +-suc m n ⟩
    suc (m + n)
  ≡⟨ cong suc (+-comm m n) ⟩
    suc (n + m)
  ≡⟨⟩
    suc n + m
  ∎

{-
Exercise ≤-Reasoning (stretch) TODO
The proof of monotonicity from Chapter Relations can be written in a more readable form
by using an analogue of our notation for ≡-Reasoning.
Define ≤-Reasoning analogously, and use it to write out an alternative proof
that addition is monotonic with regard to inequality.
Rewrite all of +-monoˡ-≤, +-monoʳ-≤, and +-mono-≤.
-}

------------------------------------------------------------------------------
-- Rewriting
-- keyword rewrite : followed by evidence of an EQUALITY
-- that equality is used to rewrite
-- - the type of the goal
-- - and of any variable in scope

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

even-comm : ∀ (m n : ℕ)
  → even (m + n)
    ------------
  → even (n + m)
even-comm m n ev         -- even (n + m)
  rewrite +-comm n m     -- even (m + n) ; ev : even (m + n)
  = ev


--------------------------------------------------
-- Multiple rewrites each separated by a vertical bar.
+-comm′ : ∀ (m n : ℕ) → m + n ≡ n + m
+-comm′ zero    n  rewrite +-identity n             =  refl
+-comm′ (suc m) n  rewrite +-suc n m | +-comm′ m n  =  refl


--------------------------------------------------
-- rewrite is shorthand for an use of WITH

even-comm′ : ∀ (m n : ℕ)
  → even (m + n)
    ------------
  → even (n + m)
even-comm′ m n ev with   m + n  | +-comm m n
...                  | .(n + m) | refl       = ev

{-
--------------------------------------------------
DOT pattern, .(n + m).
dot followed by an expression.
Used when other info forces value matched to be equal to value of expr in dot pattern.
-}

------------------------------------------------------------------------------
-- subst (instead of rewrite)

even-comm″ : ∀ (m n : ℕ)
  → even (m + n)
    ------------
  → even (n + m)
even-comm″ m n  =  subst even (+-comm m n)

{-
------------------------------------------------------------------------------
Leibniz equality

≡ def is due to Martin Löf (1975).

Older form is due to Leibniz (1686).

IDENTITY OF INDISCERNIBLES:
-  two objects are equal IFF they satisfy the same properties

Below: define Leibniz equality, and show that two terms satisfy Leibniz equality IFF
they satisfy Martin Löf equality.

x ≐ y holds if every property P that holds of x also holds of y

this def is sufficient to ensure the converse
- that every property P that holds of y also holds of x

Let x and y be objects of type A.
x ≐ y holds if for every predicate P over type A we have that P x implies P y:
_]
-}

_≐_ : ∀ {A : Set} (x y : A) → Set₁
_≐_ {A} x y = ∀ (P : A → Set) → P x → P y

{-
NOTE: write _≐_ {A} x y (instead of x ≐ y) to provide access to implicit parameter.

--------------------------------------------------
FIRST USE OF LEVELS.

Cannot assign Set the type Set (Russell’s Paradox and Girard’s Paradox).

Set is an abbreviation for Set₀

Since _≐_ def  mentions Set on right-hand side, the corresponding signature must use Set₁.

Leibniz equality is reflexive (follows by a variant of the identity function)
and transitive (by a variant of function composition)
-}
refl-≐ : ∀ {A : Set} {x : A}
  → x ≐ x
refl-≐ P Px  =  Px

trans-≐ : ∀ {A : Set} {x y z : A}
  → x ≐ y
  → y ≐ z
    -----
  → x ≐ z
trans-≐ x≐y y≐z P Px  =  y≐z P (x≐y P Px)

-- Symmetry : show that if P x implies P y for all predicates P,
-- then the implication holds the other way round as well:
sym-≐ : ∀ {A : Set} {x y : A}
  → x ≐ y
    -----
  → y ≐ x
sym-≐ {A} {x} {y} x≐y P  =  Qy
  where
    Q : A → Set
    Q z = P z → P x
    Qx : Q x
    Qx = refl-≐ P
    Qy : Q y
    Qy = x≐y Q Qx
{-
Given x ≐ y, a specific P, construct a proof that P y implies P x.
Instantiate the equality with a predicate Q such that Q z holds if P z implies P x.
The property Q x is trivial by reflexivity, and hence Q y follows from x ≐ y.
But Q y is exactly a proof of what we require, that P y implies P x.

show Martin Löf equality implies Leibniz equality, and vice versa.

if x ≡ y then need for any P to take evidence of P x to evidence of P y
which can be done since equality of x and y implies that any proof of P x is also a proof of P y:
-}

≡-implies-≐ : ∀ {A : Set} {x y : A}
  → x ≡ y
    -----
  → x ≐ y
≡-implies-≐ x≡y P  =  subst P x≡y

-- given that for any P we can take a proof of P x to a proof of P y then show x ≡ y:

≐-implies-≡ : ∀ {A : Set} {x y : A}
  → x ≐ y
    -----
  → x ≡ y
≐-implies-≡ {A} {x} {y} x≐y  =  Qy
  where
    Q : A → Set
    Q z = x ≡ z
    Qx : Q x
    Qx = refl
    Qy : Q y
    Qy = x≐y Q Qx
{-
proof similar to that for symmetry of Leibniz equality.
We take Q to be the predicate that holds of z if x ≡ z.
Then Q x is trivial by reflexivity of Martin Löf equality, and hence Q y follows from x ≐ y.
But Q y is exactly a proof of what we require, that x ≡ y.

(Parts adapted from
≐≃≡: Leibniz Equality is Isomorphic to Martin-Löf Identity, Parametrically,
by Andreas Abel, Jesper Cockx, Dominique Devries, Andreas Nuyts, and Philip Wadler, draft, 2017.)

------------------------------------------------------------------------------
Universe polymorphism

Not every type belongs to Set.
Set₀ : Set₁, Set₁ : Set₂, ...

≡ def at beginning is ok for comparing values of a type that belongs to Set.

How to compare two values of a type that belongs to Set ℓ for some arbitrary level ℓ?

Via universe polymorphism : definition made with respect to arbitrary level ℓ.
To use levels:
-}
open import Level using (Level; _⊔_) renaming (zero to lzero; suc to lsuc)
{-

Levels : isomorphic to natural numbers

lzero : Level
lsuc  : Level → Level

Set₀, Set₁, Set₂, and so on, are abbreviations for

Set lzero
Set (lsuc lzero)
Set (lsuc (lsuc lzero))

_⊔_ : Level → Level → Level -- given two levels returns the largest

equality, symetry, generalised to an arbitrary level:
-}

data _≡′_ {ℓ : Level} {A : Set ℓ} (x : A) : A → Set ℓ where
  refl′ : x ≡′ x

sym′ : ∀ {ℓ : Level} {A : Set ℓ} {x y : A}
  → x ≡′ y
    ------
  → y ≡′ x
sym′ refl′ = refl′

{-
most defs in standard library are generalised to arbitrary levels

generalised Leibniz equality:
-}

_≐′_ : ∀ {ℓ : Level} {A : Set ℓ} (x y : A) → Set (lsuc ℓ)
_≐′_ {ℓ} {A} x y = ∀ (P : A → Set ℓ) → P x → P y

-- generalized composition
_∘_ : ∀ {ℓ₁ ℓ₂ ℓ₃ : Level} {A : Set ℓ₁} {B : Set ℓ₂} {C : Set ℓ₃}
  → (B → C) → (A → B) → A → C
(g ∘ f) x  =  g (f x)

{-
Standard library

defines _≡⟨_⟩_ as step-≡, which reverses the order of the arguments.

defines a syntax macro ((automatically imported with step-≡) i, which recovers original arg order:
-}
