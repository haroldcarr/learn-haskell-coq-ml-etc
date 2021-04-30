module x04equality where

------------------------------------------------------------------------------
-- EQUALITY

-- for any      type A
-- for any x of type A
-- refl constructor provides evidence that x ≡ x
-- i.e., every value is equal to itself
data _≡_ {A : Set} (x : A) : A → Set where
  refl : x ≡ x

infix 4 _≡_

-- tell Agda which type corresponds to equality
{-# BUILTIN EQUALITY _≡_ #-}

{-
Note:
- 1st arg to _≡_ is PARAMETER (x : A)
  - BEST PRACTICE: use parameters when possible
  - can be a parameter because it does not vary
- 2nd arg is an INDEX in A → Set
  - must be an index, so it can be required to be equal to the first

------------------------------------------------------------------------------
-- EQUALITY is an EQUIVALENCE RELATION (reflexive, symmetric, transitive)

reflexivity in the def of equality, via refl constructor
-}

-- symmetry
sym : ∀ {A : Set} {x y : A}
  → x ≡ y  -- arg has type x ≡ y
    -----
  → y ≡ x
sym refl -- LHS :instantiates arg to refl CONSTRUCTOR (only one possible)
         -- refl requires x y to be the same
  = refl -- RHS : need a term of type x ≡ x, so refl

-- transitive
trans : ∀ {A : Set} {x y z : A}
  → x ≡ y
  →     y ≡ z
    -----
  → x ≡     z
trans refl refl = refl

------------------------------------------------------------------------------
-- EQUALITY SATISFIES CONGRUENCE

-- if two terms are equal, they remain so after same function is applied to both
cong : ∀ {A B : Set} (f : A → B) {x y : A}
  →   x ≡   y
    ---------
  → f x ≡ f y
cong f refl = refl

-- where f takes two args
cong₂ : ∀ {A B C : Set} (f : A → B → C) {u x : A} {v y : B}
  →   u   ≡   x
  →     v ≡     y
    -------------
  → f u v ≡ f x y
cong₂ f refl refl = refl

-- if two functions are equal, then applying them to same term yields equal terms
cong-app : ∀ {A B : Set} {f g : A → B}
  →             f   ≡ g
    ---------------------
  → ∀ (x : A) → f x ≡ g x
cong-app refl x = refl

------------------------------------------------------------------------------
-- EQUALITY SATISFIES SUBSTITUTION

-- if two values are equal and a predicate holds of first then it also holds of second
subst : ∀ {A : Set} {x y : A} (P : A → Set)
  →   x ≡  y
    ---------
  → P x
  →     P y
subst P refl px = px

------------------------------------------------------------------------------
--  CHAINS OF EQUATIONS (aka EQUATIONAL REASONING)

module ≡-Reasoning {A : Set} where

  infix  1 begin_
  infixr 2 _≡⟨⟩_ _≡⟨_⟩_
  infix  3 _∎

  -- identity : this is just used to make proof look nice
  begin_ : ∀ {x y : A}
    → x ≡ y
      -----
    → x ≡ y
  begin x≡y = x≡y

  -- think of _≡⟨⟩_ as equivalent to _≡⟨ refl ⟩_
  _≡⟨⟩_ : ∀ (x : A) {y : A}
    → x ≡ y
      -----
    → x ≡ y
  x ≡⟨⟩ x≡y = x≡y

  _≡⟨_⟩_ : ∀ (x : A) {y z : A}
    → x ≡ y
    → y ≡ z
      -----
    → x ≡ z
  x ≡⟨ x≡y ⟩ y≡z = trans x≡y y≡z

  _∎ : ∀ (x : A)
      -----
    → x ≡ x
  x ∎ = refl

open ≡-Reasoning

trans′ : ∀ {A : Set} {x y z : A}
  → x ≡ y
  →     y ≡ z
    -----
  → x ≡     z
trans′ {A} {x} {y} {z} x≡y y≡z =
  begin          -- Goal: x ≡ z   ; y≡z : y ≡ z; x≡y : x ≡ y
    x ≡⟨ x≡y ⟩   -- Goal: y ≡ z
    y ≡⟨ y≡z ⟩   -- Goal: z ≡ z
    z
  ∎

{-
EXERCISE trans and ≡-Reasoning (practice) TODO
Cannot use definition of trans’ using ≡-Reasoning as the definition for trans.
Why?
Hint: look at the definition of _≡⟨_⟩_

-- Your code goes here

------------------------------------------------------------------------------
Chains of equations, another example : addition is commutative
-}

data ℕ : Set where
  zero : ℕ
  suc  : ℕ → ℕ

{-# BUILTIN NATURAL ℕ #-}

_+_ : ℕ → ℕ → ℕ
zero    + n = n
(suc m) + n = suc (m + n)

infixl 6  _+_

-- to save space, postulate (rather than prove) two lemmas:
-- POSTULATE : specifies a signature but no def. Use with care. DON'T postulate something false!
postulate
  +-identity : ∀ (m   : ℕ) → m + zero  ≡ m
  +-suc      : ∀ (m n : ℕ) → m + suc n ≡ suc (m + n)

+-comm : ∀ (m n : ℕ) → m + n ≡ n + m
+-comm m zero =
  begin
    m + zero    ≡⟨ +-identity m ⟩
    m           ≡⟨⟩
    zero + m
  ∎
+-comm m (suc n) =
  begin
         m + suc n  ≡⟨ +-suc m n ⟩
    suc (m +     n) ≡⟨ cong suc (+-comm m n) ⟩
    suc (n +     m) ≡⟨⟩ -- terms are equivalent to simplified term
    suc  n +     m
  ∎

{-
------------------------------------------------------------------------------
Exercise ≤-Reasoning (stretch)

redo proof of +-monoʳ-≤ (from Chapter Relations) using an analogue of ≡-Reasoning

define ≤-Reasoning

use it to write out proof that addition is monotonic with regard to inequality
by redoing all of +-monoˡ-≤, +-monoʳ-≤, and +-mono-≤
-}

module ≤-Reasoning {A : Set} where

  data _≤_ : ℕ → ℕ → Set where
    z≤n : ∀ {n : ℕ}
        --------
      → zero ≤ n

    s≤s : ∀ {m n : ℕ}
      →     m ≤     n
        -------------
      → suc m ≤ suc n

  infix 4 _≤_

  ≤-trans : ∀ {m n p : ℕ} → m ≤ n → n ≤ p → m ≤ p
  ≤-trans      z≤n         _  = z≤n
  ≤-trans (s≤s m≤n) (s≤s n≤p) = s≤s (≤-trans m≤n n≤p)

  infix  1 begin≤_
  infixr 2 _≤⟨⟩_ _≤⟨_⟩_
  infix  3 _∎≤

  --------------------------------------------------

  begin≤_ : ∀ {x y : ℕ}
    → x ≤ y
      -----
    → x ≤ y
  begin≤ x≤y = x≤y

  -- can think of _≤⟨⟩_ as equivalent to _≤⟨ refl ⟩_
  _≤⟨⟩_ : ∀ (x : ℕ) {y : ℕ}
    → x ≤ y
      -----
    → x ≤ y
  x ≤⟨⟩ x≤y = x≤y

  _≤⟨_⟩_ : ∀ (x : ℕ) {y z : ℕ}
    → x ≤ y
    → y ≤ z
      -----
    → x ≤ z
  x ≤⟨ x≤y ⟩ y≤z = ≤-trans x≤y y≤z

  _∎≤ : ∀ (x : ℕ)
      -----
    → x ≤ x
  zero  ∎≤ = z≤n
  suc x ∎≤ = s≤s (x ∎≤)

  --------------------------------------------------

  +-monoʳ-≤ : ∀ (n p q : ℕ)
    →      p  ≤      q
      -------------
    → (n + p) ≤ (n + q)

  +-monoʳ-≤ zero    p q p≤q  =
    begin≤
      zero + p ≤⟨⟩
             p ≤⟨ p≤q ⟩
             q ≤⟨⟩
      zero + q
    ∎≤

  +-monoʳ-≤ (suc n) p q p≤q =
    begin≤
      suc n + p ≤⟨ s≤s (+-monoʳ-≤ n p q p≤q) ⟩
      suc n + q
    ∎≤

  -------------------------

  postulate
    +-comm-≤ : ∀ (m n : ℕ) → m + n ≤ n + m

  +-monoˡ-≤ : ∀ (m n p : ℕ)
    → m     ≤ n
      -------------
    → m + p ≤ n + p

  +-monoˡ-≤ m n p m≤n =
    begin≤
      m + p ≤⟨ +-comm-≤ m p ⟩
      p + m ≤⟨ +-monoʳ-≤ p m n m≤n ⟩
      p + n ≤⟨ +-comm-≤ p n ⟩
      n + p
    ∎≤

  -------------------------

  +-mono-≤ : ∀ (m n p q : ℕ)
    → m     ≤ n
    →     p ≤     q
      -------------
    → m + p ≤ n + q
  +-mono-≤ m n p q m≤n p≤q =
    begin≤
      m + p ≤⟨ +-monoˡ-≤ m n p m≤n ⟩
      n + p ≤⟨ +-monoʳ-≤ n p q p≤q ⟩
      n + q
    ∎≤

{-
------------------------------------------------------------------------------
-- Rewriting
-}

data even : ℕ → Set
data odd  : ℕ → Set

data even where

  even-zero : even zero

  even-suc : ∀ {n : ℕ}
    → odd       n
      ------------
    → even (suc n)

data odd where
  odd-suc : ∀ {n : ℕ}
    → even     n
      -----------
    → odd (suc n)

{-
given even (m + n) holds
prove even (n + m) holds

REWRITE: notation to support this kind of reasoning
-}

-- then use rewrite
even-comm : ∀ (m n : ℕ)
  → even (m + n)
    ------------
  → even (n + m)
even-comm m n ev      -- even (n + m)
  rewrite +-comm n m  -- Goal: even (m + n); ev : even (m + n)
  = ev

{-
keyword REWRITE : followed by evidence of an equality
That equality is used to rewrite the type of the goal and of any variable in scope.
-}

even-comm' : ∀ (m n : ℕ)
  → even (m + n)
    ------------
  → even (n + m)
even-comm' m n ev        -- Goal: even (n + m)  ; ev : even (m + n)
  rewrite
    +-comm m n           -- Goal: even (n + m)  ; ev : even (n + m) <-- rewrites evidence
  = ev --   ^
--          note: arg order diff than 'even-comm' above

------------------------------------------------------------------------------
-- Multiple rewrites : each separated by a vertical bar

+-comm′ : ∀ (m n : ℕ) → m + n ≡ n + m
+-comm′ zero    n        --    zero + n ≡ n + zero
                         --           n ≡ n + zero
  rewrite
    +-identity n         --           n ≡ n
  = refl
+-comm′ (suc m) n        -- suc  m + n  ≡ n + suc m
                         -- suc (m + n) ≡ n + suc m
  rewrite
    +-suc   n m          -- suc (m + n) ≡ suc (n + m) <-- rewrites LHS
  | +-comm′ m n          -- suc (n + m) ≡ suc (n + m) <-- rewrites RHS
  = refl

{-
Previous +-comm proof required cong suc (+-comm m n) (invoking inductive hypothesis).
Rewriting automatically takes congruence into account.
Proofs with rewriting are shorter.
Proofs as chains of equalities are easier to follow.
-}

+-comm′' : ∀ (m n : ℕ) → m + n ≡ n + m
+-comm′' zero    n =
  begin
    zero + n ≡⟨⟩
           n ≡⟨ +-comm′ zero n ⟩
    n + zero
  ∎
+-comm′' (suc m) n =
  begin
    suc m + n ≡⟨ +-comm′ (suc m)  n ⟩
    n + suc m
  ∎

{-
------------------------------------------------------------------------------
--rewrite is shorthand for 'WITH'
-}

even-comm′ : ∀ (m n : ℕ)
  → even (m + n)
    ------------
  → even (n + m)
even-comm′ m n ev with   m + n  | +-comm m n
...                  | .(n + m) | refl       = ev

{-
WITH : can be followed by one or more expressions, separated by bars
- where each equation has the same number of patterns

1st COLUMN asserts m + n and n + m are identical
2nd COLUMN justifies assertion with evidence of the appropriate equality

DOT PATTERN : .(n + m)
- dot followed by an expression
- used when other info forces value matched to be equal to value of expression in dot pattern

Here: m + n ≡ n + m justified by matching +-comm m n with refl

------------------------------------------------------------------------------
-- using SUBSTITUTION instead of REWRITE
-}

even-comm″ : ∀ (m n : ℕ)
  → even (m + n)
    ------------
  → even (n + m)
even-comm″ m n
  --      m + n ≡ n + m
  --            v
--= subst even {!!}
--= subst {!!}  (+-comm m n)
--         ^
-- Constraints
-- ?0 (n + m) =< even (n + m)
-- even (m + n) =< ?0 (m + n)
  = subst even (+-comm m n)

{-
------------------------------------------------------------------------------
-- Leibniz equality : TWO OBJECTS EQUAL IFF THEY SATISFY THE SAME PROPERTIES.

The form of asserting equality so far is due to Martin Löf (1975).

Another form is due to Leibniz (1686).


Define Leibniz equality and show that two terms satisfy Leibniz equality
IFF they satisfy Martin Löf equality.

x ≐ y
holds if
every property P that holds of x also holds of y
also ensures converse
every property P that holds of y also holds of x

Let x and y be objects of type A.
x ≐ y holds if for every predicate P over type A we have that P x implies P y:
-}

_≐_ : ∀ {A : Set} (x y : A) → Set₁
_≐_ {A} x y = ∀ (P : A → Set) → P x → P y

{-
write _≐_ {A} x y (instead of infix) to provide access to the implicit parameter A

FIRST USE OF LEVELS

Cannot assign Set the type Set
- would lead to contradictions such as Russell’s Paradox and Girard’s Paradox

hierarchy of types, where Set : Set₁, Set₁ : Set₂, ...

'Set' is an abbreviation for 'Set₀'

Since the equation defining _≐_ mentions Set on the right-hand side,
the corresponding signature must use Set₁.

------------------------------------------------------------------------------
Leibniz equality is reflexive, transitive, symmetric
-}

-- reflexiviity follows by a variant of the identity function
refl-≐ : ∀ {A : Set} {x : A}
  → x ≐ x
refl-≐ P Px = Px

-- transitivity follows by a variant of function composition
trans-≐ : ∀ {A : Set} {x y z : A}
  → x ≐ y
  → y ≐ z
    -----
  → x ≐ z
trans-≐ x≐y y≐z P Px = y≐z P (x≐y P Px)

-- show that if P x implies P y for all predicates P
-- then         P y implies P x
sym-≐ : ∀ {A : Set} {x y : A}
  → x ≐ y           -- given x ≐ y
    -----
  → y ≐ x
sym-≐ {A} {x} {y} x≐y P = Qy -- TODO : where is 'P' in the signature?
  where
    Q : A → Set     -- instantiate the equality with a predicate Q such that Q z holds
    Q z = P z → P x -- if P z implies P x

    Qx : Q x        -- The property Q x is by reflexivity, and hence Q y follows from x ≐ y.
    Qx = refl-≐ P

    Qy : Q y        --  Q y is the required proof : P y implies P x
    Qy = x≐y Q Qx

{-
------------------------------------------------------------------------------
Martin Löf equality implies Leibniz equality

given x ≡ y
need for any P to take evidence of P x to evidence of P y

the equality of x and y implies that any proof of P x is also a proof of P y
follows from substitution
-}

≡-implies-≐ : ∀ {A : Set} {x y : A}
  → x ≡ y
    -----
  → x ≐ y
≡-implies-≐ x≡y P = subst P x≡y

{-
------------------------------------------------------------------------------
Leibniz equality implies Martin Löf equality

given for any P we can take a proof of P x to a proof of P y
show x ≡ y

proof is similar to that for symmetry of Leibniz equality
-}

≐-implies-≡ : ∀ {A : Set} {x y : A}
  → x ≐ y
    -----
  → x ≡ y
≐-implies-≡ {A} {x} {y} x≐y = Qy
  where
    Q : A → Set   -- Q is predicate that holds of z if x ≡ z
    Q z = x ≡ z

    Qx : Q x      -- Q x by reflexivity of Martin Löf equality
    Qx = refl

    Qy : Q y      -- Q y follows from x ≐ y
    Qy = x≐y Q Qx -- Q y is required proof : x ≡ y

{-
This section adapted from
≐≃≡: Leibniz Equality is Isomorphic to Martin-Löf Identity, Parametrically
draft/2017
by Andreas Abel, Jesper Cockx, Dominique Devries, Andreas Nuyts, and Philip Wadler

------------------------------------------------------------------------------
UNIVERSE POLYMORPHISM (aka LEVELs)

every type belongs somewhere in the hierarchy Set₀, Set₁, Set₂, ...

Set abbreviates Set₀

Set₀ : Set₁
Set₁ : Set₂
...

to compare values of a type that belongs to Set ℓ for some arbitrary level ℓ?

via UNIVERSE POLYMORPHISM

definition is made with respect to an arbitrary level ℓ

to use levels:
-}

open import Level using (Level; _⊔_) renaming (zero to lzero; suc to lsuc)

{-
rename constructors zero and suc to avoid confusion between levels and naturals

Levels are isomorphic to natural numbers, and have similar constructors:

lzero : Level
lsuc  : Level → Level

Set₀, Set₁, Set₂, ... abbreviations for

Set lzero
Set (lsuc lzero)
Set (lsuc (lsuc lzero))

_⊔_ : Level → Level → Level

that given two levels returns the larger of the two.
-}

-- equality, generalised to an arbitrary level:
data _≡′_ {ℓ : Level} {A : Set ℓ} (x : A) : A → Set ℓ where
  refl′ : x ≡′ x

-- generalised definition of symmetry:
sym′ : ∀ {ℓ : Level} {A : Set ℓ} {x y : A}
  → x ≡′ y
    ------
  → y ≡′ x
sym′ refl′ = refl′

{-
For simplicity, this book avoids universe polymorphism.
Most definitions in the standard library are generalised to arbitrary levels as above.
-}

-- generalised definition of Leibniz equality:
_≐′_ : ∀ {ℓ : Level} {A : Set ℓ} (x y : A) → Set (lsuc ℓ)
_≐′_ {ℓ} {A} x y = ∀ (P : A → Set ℓ) → P x → P y

{-
Before the signature used Set₁ as the type of a term that includes Set,
whereas here the signature uses Set (lsuc ℓ) as the type of a term that includes Set ℓ.

Most other functions in the standard library are also generalised to arbitrary levels.
-}

-- definition of composition.
_∘_ : ∀ {ℓ₁ ℓ₂ ℓ₃ : Level} {A : Set ℓ₁} {B : Set ℓ₂} {C : Set ℓ₃}
  → (B → C) → (A → B) → A → C
(g ∘ f) x = g (f x)

{-
------------------------------------------------------------------------------
Standard library

standard library defines _≡⟨_⟩_ as step-≡,
which reverses the order of the arguments

standard library defines a macro,imported when import step-≡
which recovers the original argument order:

import Relation.Binary.PropositionalEquality as Eq
open Eq using (_≡_; refl; trans; sym; cong; cong-app; subst)
open Eq.≡-Reasoning using (begin_; _≡⟨⟩_; step-≡; _∎)

------------------------------------------------------------------------------
Unicode

≡  U+2261  IDENTICAL TO                     (\==, \equiv)
⟨  U+27E8  MATHEMATICAL LEFT ANGLE BRACKET  (\<)
⟩  U+27E9  MATHEMATICAL RIGHT ANGLE BRACKET (\>)
∎  U+220E  END OF PROOF                     (\qed)
≐  U+2250  APPROACHES THE LIMIT             (\.=)
ℓ  U+2113  SCRIPT SMALL L                   (\ell)
⊔  U+2294  SQUARE CUP                       (\lub)
₀  U+2080  SUBSCRIPT ZERO                   (\_0)
₁  U+2081  SUBSCRIPT ONE                    (\_1)
₂  U+2082  SUBSCRIPT TWO                    (\_2)
-}
