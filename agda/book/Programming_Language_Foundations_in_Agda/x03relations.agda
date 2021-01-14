module x03relations where

import      Relation.Binary.PropositionalEquality as Eq
open        Eq                  using (_≡_; refl; cong)
open import Data.Nat            using (ℕ; zero; suc; _+_; _*_)
open import Data.Nat.Properties using (+-comm; +-identityʳ; *-comm)

{-
------------------------------------------------------------------------------
-- DEFINING RELATIONS

z≤n --------
    zero ≤ n

        m ≤     n
s≤s -------------
    suc m ≤ suc n
-}

-- INDEXED datatype : the type m ≤ n is indexed by two naturals, m and n
data _≤_ : ℕ → ℕ → Set where

  -- base case holds for all naturals
  z≤n : ∀ {n : ℕ}    -- IMPLICIT args
      --------
    → zero ≤ n

  -- inductive case holds if m ≤ n
  s≤s : ∀ {m n : ℕ}  -- IMPLICIT args
    →     m ≤     n
      -------------
    → suc m ≤ suc n

{-
proof 2 ≤ 4

  z≤n -----
      0 ≤ 2
 s≤s -------
      1 ≤ 3
s≤s ---------
      2 ≤ 4
-}

_ : 2 ≤ 4
_ = s≤s (s≤s z≤n) -- no explicit 'n'     arg  to z≤n because implicit
                  -- no explicit 'm' 'n' args to s≤s because implicit

-- can be explicit

_ : 2 ≤ 4
_ = s≤s {1} {3} (s≤s {0} {2} (z≤n {2}))

_ : 2 ≤ 4
_ = s≤s {m = 1} {n = 3} (s≤s {m = 0} {n = 2} (z≤n {n = 2}))

_ : 2 ≤ 4
_ = s≤s {n = 3} (s≤s {n = 2} z≤n)

-- infer explicit term via _
-- e.g., +-identityʳ variant with implicit arguments
+-identityʳ′ : ∀ {m : ℕ} → m + zero ≡ m
+-identityʳ′ = +-identityʳ _ -- agda infers _ from context (if it can)

-- precedence

infix 4 _≤_

------------------------------------------------------------------------------
-- DECIDABILITY : can compute ≤ : see Chapter Decidable

{-
------------------------------------------------------------------------------
-- INVERSION

above def goes from smaller to larger things (e.g., m ≤ n to suc m ≤ suc n)

sometimes go from bigger to smaller things.

only one way to prove suc m ≤ suc n, for any m n

use it to invert rule
-}

inv-s≤s : ∀ {m n : ℕ}
  → suc m ≤ suc n
    -------------
  → m ≤ n
inv-s≤s (s≤s m≤n) = m≤n
{-           ^
          variable name  (m ≤ n (with spaces) is a type)

m≤n is of type m ≤ n

convention : var name from type

not every rule is invertible
e.g., rule for z≤n has no non-implicit hypotheses, so nothing to invert

another example inversion

-}
inv-z≤n : ∀ {m : ℕ}
  → m ≤ zero
    --------
  → m ≡ zero -- only one way a number can be ≤ zero
inv-z≤n z≤n = refl

{-
------------------------------------------------------------------------------
-- PROPERTIES OF ORDERING RELATIONS

- REFLEXIVE       : ∀ n    , n ≤ n
- TRANSITIVE      : ∀ m n p, if m ≤ n && n ≤ p then m ≤ p
- ANTI-SYMMETRIC  : ∀ m n  , if m ≤ n && n ≤ m then m ≡ n
- TOTAL           : ∀ m n  , either m ≤ n or n ≤ m

_≤_ satisfies all 4

names for some combinations:

PREORDER      : reflexive and transitive
PARTIAL ORDER : preorder      that is also anti-symmetric
TOTAL ORDER   : partial order that is also total

Exercise orderings (practice)

preorder that is not a partial order:

-- https://math.stackexchange.com/a/2217969

partial order that is not a total order:

-- https://math.stackexchange.com/a/367590

-- other properties

- SYMMETRIC : ∀ x y, if x R y then y R x

------------------------------------------------------------------------------
REFLEXIVITY
-}

-- proof is via induction on implicit arg n
≤-refl : ∀ {n : ℕ} -- implicit arg to make it easier to invoke
    -----
  → n ≤ n
  --
≤-refl {zero}  = z≤n        -- zero  ≤ zero
-- inductive case applies IH '≤-refl {n}' for a proof of n ≤ n to 's≤s' giving proof suc n ≤ suc n
≤-refl {suc n} = s≤s ≤-refl -- suc n ≤ suc n

{-
------------------------------------------------------------------------------
TRANSITIVITY
-}

-- inductive proof using evidence m ≤ n
≤-trans : ∀ {m n p : ℕ}
  → m ≤ n
  → n ≤ p
    -----
  → m ≤ p


-- base
-- 1st first inequality holds by z≤n
--            v
≤-trans      z≤n         _  = z≤n
{-                       ^     ^
                         |     now must show zero ≤ p; follows by z≤n
n ≤ p case is irrelevant (written _)

-}

{-
inductive
-- 1st inequality
--          |     2nd inequality
            v         v -}
≤-trans (s≤s m≤n) (s≤s n≤p) = s≤s (≤-trans m≤n n≤p)
{-
that provides
      suc m ≤ suc n
                suc n ≤ suc p
must now show suc m ≤ suc p

IH '≤-trans m≤n n≤p' establishes m ≤ p

goal follows by applying s≤s

≤-trans (s≤s m≤n) z≤n case cannot arise since
- 1st inequality implies middle value is suc n
- 2nd inequality implies that it is zero

ALTERNATIVE with explicit implicits
-}

≤-trans′ : ∀ (m n p : ℕ)
  → m ≤ n
  → n ≤ p
    -----
  → m ≤ p
≤-trans′   zero       _       _       z≤n         _  = z≤n
≤-trans′ (suc m) (suc n) (suc p) (s≤s m≤n) (s≤s n≤p) = s≤s (≤-trans′ m n p m≤n n≤p)

{-
technique of induction on evidence that a property holds
- e.g., inducting on evidence that m ≤ n
rather than induction on values of which the property holds
- e.g., inducting on m
is often used

------------------------------------------------------------------------------
ANTI-SYMMETRY
-}

-- proof via induction over the evidence that m ≤ n and n ≤ m hold
≤-antisym : ∀ {m n : ℕ}
  → m ≤ n
  → n ≤ m
    -----
  → m ≡ n
≤-antisym      z≤n       z≤n  =  refl
≤-antisym (s≤s m≤n) (s≤s n≤m) =  cong suc (≤-antisym m≤n n≤m)

{-
base : both inequalities hold by z≤n
- so given zero ≤ zero and zero ≤ zero
- shows zero ≡ zero (via Reflexivity of equality)

inductive
- after using evidence for 1st and 2nd inequalities
- have suc m ≤ suc n and suc n ≤ suc m
- must show suc m ≡ suc n
- IH '≤-antisym m≤n n≤m' establishes m ≡ n
- goal follows by congruence

Exercise ≤-antisym-cases (practice) TODO

The above proof omits cases where one argument is z≤n and one argument is s≤s.
Why is it ok to omit them?

-- Your code goes here

------------------------------------------------------------------------------
TOTAL
-}

-- datatype with PARAMETERS
data Total (m n : ℕ) : Set where

  forward :
      m ≤ n
      ---------
    → Total m n

  flipped :
      n ≤ m
      ---------
    → Total m n
{-

Evidence that Total m n holds is either of the form
- forward m≤n or
- flipped n≤m
where m≤n and n≤m are evidence of m ≤ n and n ≤ m respectively

(above definition could also be written as a disjunction; see Chapter Connectives.)

above data type uses PARAMETERS m n --- equivalent to following INDEXED datatype:
-}

data Total′ : ℕ → ℕ → Set where

  forward′ : ∀ {m n : ℕ}
    → m ≤ n
      ----------
    → Total′ m n

  flipped′ : ∀ {m n : ℕ}
    → n ≤ m
      ----------
    → Total′ m n

{-
Each parameter of the type translates as an implicit parameter of each constructor.
Unlike an indexed datatype, where the indexes can vary (as in zero ≤ n and suc m ≤ suc n),
in a parameterised datatype the parameters must always be the same (as in Total m n).

Parameterised declarations are shorter, easier to read,
and occasionally aid Agda’s termination checker.
Use them in preference to indexed types when possible.

prove totality: by induction over 1st and 2nd args

uses WITH
-}

≤-total : ∀ (m n : ℕ) → Total m n

-- base : 1st arg zero so forward case holds with z≤n as evidence that zero ≤ n
≤-total zero    n                         =  forward z≤n

-- base : 2nd arg zero so flipped case holds with z≤n as evidence that zero ≤ suc m
≤-total (suc m) zero                      =  flipped z≤n

-- inductive : IH establishes one of
≤-total (suc m) (suc n) with ≤-total m n
-- forward case of IH with m≤n as evidence that m ≤ n
-- follows that forward case of prop holds with s≤s m≤n as evidence that suc m ≤ suc n
...                        | forward m≤n  =  forward (s≤s m≤n)
-- flipped case of IH with n≤m as evidence that n ≤ m
-- follows that flipped case of prop holds with s≤s n≤m as evidence that suc n ≤ suc m
...                        | flipped n≤m  =  flipped (s≤s n≤m)

{-
WITH expression and one or more subsequent lines, each line begins ... | followed by pattern and right-hand side

WITH equivalent to defining a helper function
-}

≤-total′ : ∀ (m n : ℕ) → Total m n
≤-total′ zero    n        =  forward z≤n
≤-total′ (suc m) zero     =  flipped z≤n
≤-total′ (suc m) (suc n)  =  helper (≤-total′ m n)
  where
  helper : Total m n → Total (suc m) (suc n)
  helper (forward m≤n)  =  forward (s≤s m≤n)
  helper (flipped n≤m)  =  flipped (s≤s n≤m)

{-
WHERE

vars bound on the left-hand side of the preceding equation (e.g., m n) in scope in nested def
any identifiers bound in nested definition (e.g., helper) in scope in right-hand side of preceding equation

If both arguments are equal, then both cases hold and we could return evidence of either. In the code above we return the forward case, but there is a variant that returns the flipped case:
-}

≤-total″ : ∀ (m n : ℕ) → Total m n
≤-total″ m       zero                      =  flipped z≤n
≤-total″ zero    (suc n)                   =  forward z≤n
≤-total″ (suc m) (suc n) with ≤-total″ m n
...                        | forward m≤n   =  forward (s≤s m≤n)
...                        | flipped n≤m   =  flipped (s≤s n≤m)

-- differs from original : pattern matches on 2nd arg before 1st

{-
------------------------------------------------------------------------------
MONOTONICITY ∀ {m n p q : ℕ} → m ≤ n → p ≤ q → m + p ≤ n + q

addition is monotonic on the right
proof by induction on 1st arg
-}

+-monoʳ-≤ : ∀ (n p q : ℕ)
  → p ≤ q
    -------------
  → n + p ≤ n + q
-- base      : 1st is zero;        zero + p  ≤   zero + q
--                  def/eq                p  ≤          q
-- evidence given by arg p≤q
+-monoʳ-≤ zero    p q p≤q  =  p≤q
-- inductive : 1st is suc n;      suc n + p  ≤  suc n + q
--                  def/eq;      suc (n + p) ≤ suc (n + q)
-- IH +-monoʳ-≤ n p q p≤q establishes n + p ≤ n + q
-- goal follows by applying s≤s
+-monoʳ-≤ (suc n) p q p≤q  =  s≤s (+-monoʳ-≤ n p q p≤q)

{-
addition is monotonic on the left
via previous result and commutativity of addition:
-}
+-monoˡ-≤ : ∀ (m n p : ℕ)
  → m ≤ n
    -------------
  → m + p ≤ n + p
+-monoˡ-≤ m n p m≤n         -- m + p ≤ n + p
  rewrite
    +-comm m p              -- p + m ≤ n + p
  | +-comm n p              -- p + m ≤ p + n
 = +-monoʳ-≤ p m n m≤n

-- combine above results:

+-mono-≤ : ∀ (m n p q : ℕ)
  → m ≤ n
  → p ≤ q
    -------------
  → m + p ≤ n + q
+-mono-≤ m n p q m≤n p≤q
  = ≤-trans               -- combining below via transitivity proves m + p ≤ n + q
    (+-monoˡ-≤ m n p m≤n) -- proves m + p ≤ n + p
    (+-monoʳ-≤ n p q p≤q) -- proves n + p ≤ n + q

{-
------------------------------------------------------------------------------
Exercise *-mono-≤ (stretch) Show that multiplication is monotonic with regard to inequality.
-}

-------------------------
-- multiplication is monotonic on the right
*-monoʳ-≤ : ∀ (n p q : ℕ)
  →     p ≤     q
    -------------
  → n * p ≤ n * q

*-monoʳ-≤ zero p q p≤q          --  zero * p ≤  zero * q
                                --  zero     ≤  zero
  = z≤n

*-monoʳ-≤ n zero q p≤q          --  n * zero ≤ n * q
  rewrite
    *-comm n zero               --         0 ≤ n * q
  = z≤n

*-monoʳ-≤ n p zero p≤q          --     n * p ≤ n * zero
  rewrite
    *-comm n zero               --     n * p ≤ 0
  | inv-z≤n p≤q                 --     n * 0 ≤ 0
  | *-comm n zero               --         0 ≤ 0
  = z≤n

*-monoʳ-≤ n (suc p) (suc q) p≤q -- n * suc p ≤ n * suc q
  rewrite
    *-comm n (suc p)            -- n + p * n ≤ n * suc q
  | *-comm n (suc q)            -- n + p * n ≤ n + q * n
  | *-comm p n                  -- n + n * p ≤ n + q * n
  | *-comm q n                  -- n + n * p ≤ n + n * q
  = +-monoʳ-≤ n (n * p) (n * q) (*-monoʳ-≤ n p q (inv-s≤s p≤q))

-------------------------
-- multiplication is monotonic on the left
*-monoˡ-≤ : ∀ (m n p : ℕ)
  → m     ≤ n
    -------------
  → m * p ≤ n * p

*-monoˡ-≤ zero n p m≤n          --  zero * p ≤ n * p
                                --  zero     ≤ n * p
  = z≤n

*-monoˡ-≤ m zero p m≤n          --     m * p ≤ zero * p
                                --     m * p ≤ zero
  rewrite inv-z≤n m≤n           --     0 * p ≤ 0
                                --      zero ≤ 0
  = z≤n

*-monoˡ-≤ (suc m) (suc n) p m≤n -- suc m * p ≤ suc n * p
                                -- p + m * p ≤ p + n * p
  = +-monoʳ-≤ p (m * p) (n * p) (*-monoˡ-≤ m n p (inv-s≤s m≤n))

-------------------------

*-mono-≤ : ∀ (m n p q : ℕ)
  → m ≤ n
  → p ≤ q
    -------------
  → m * p ≤ n * q
*-mono-≤ m n p q m≤n p≤q
  = ≤-trans
    (*-monoˡ-≤ m n p m≤n)
    (*-monoʳ-≤ n p q p≤q)
