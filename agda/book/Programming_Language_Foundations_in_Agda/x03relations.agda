module x03relations where

import      Relation.Binary.PropositionalEquality as Eq
open        Eq                  using (_≡_; refl; cong)
open import Data.Nat            using (ℕ; zero; suc; _+_)
open import Data.Nat.Properties using (+-comm; +-identityʳ)

{-
-- defining relations

z≤n --------
    zero ≤ n

    m ≤ n
s≤s -------------
    suc m ≤ suc n
-}

data _≤_ : ℕ → ℕ → Set where

  -- base case holds for all naturals
  z≤n : ∀ {n : ℕ}    -- implicit args
      --------
    → zero ≤ n

  -- inductive case holds if m ≤ n
  s≤s : ∀ {m n : ℕ}  -- implicit args
    → m ≤ n          -- indexed datatype : the type m ≤ n is indexed by two naturals, m and n
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

-- decidability : can compute ≤ : see Chapter Decidable

{-
-- inversion

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
-- properties of ordering relations

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

prove totality:
-}

≤-total : ∀ (m n : ℕ) → Total m n
≤-total zero    n                         =  forward z≤n
≤-total (suc m) zero                      =  flipped z≤n
≤-total (suc m) (suc n) with ≤-total m n
...                        | forward m≤n  =  forward (s≤s m≤n)
...                        | flipped n≤m  =  flipped (s≤s n≤m)

{-
...
-}
