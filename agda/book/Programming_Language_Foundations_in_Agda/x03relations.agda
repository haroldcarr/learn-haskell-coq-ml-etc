module x03relations where

import      Relation.Binary.PropositionalEquality as Eq
open        Eq                  using (_≡_; refl; cong; sym)
open import Data.Nat            using (ℕ; zero; suc; _+_; _*_)
open import Data.Nat.Properties using (+-comm; +-identityʳ; *-comm; +-suc)

{-
------------------------------------------------------------------------------
-- DEFINING RELATIONS : INEQUALITY ≤

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

_ : 2 ≤ 4 -- with implicit args
_ = s≤s {1} {3} (s≤s {0} {2} (z≤n {2}))

_ : 2 ≤ 4 -- with named implicit args
_ = s≤s {m = 1} {n = 3} (s≤s {m = 0} {n = 2} (z≤n {n = 2}))

_ : 2 ≤ 4 -- with some implicit named args
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
  →     m ≤     n
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
1st proves
   suc m ≤ suc n
2nd proves
           suc n ≤ suc p
must now show
   suc m         ≤ suc p

IH '≤-trans m≤n n≤p' establishes m ≤ p

goal follows by applying s≤s

≤-trans (s≤s m≤n) z≤n case cannot arise since
- 1st inequality implies middle value is suc n
- 2nd inequality implies that it is zero

-}

≤-trans′ : ∀ (m n p : ℕ) -- ALTERNATIVE with explicit args
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

-- 842
-- proof via rewrite
≤-antisym' : ∀ {m n : ℕ}
  → m ≤ n
  → n ≤ m
    -----
  → m ≡ n
≤-antisym'      z≤n       z≤n  = refl
≤-antisym' (s≤s m≤n) (s≤s n≤m) rewrite ≤-antisym' m≤n n≤m = refl

-- proof via induction over the evidence that m ≤ n and n ≤ m hold
≤-antisym : ∀ {m n : ℕ}
  → m ≤ n
  → n ≤ m
    -----
  → m ≡ n
≤-antisym      z≤n       z≤n  = refl
≤-antisym (s≤s m≤n) (s≤s n≤m) = cong suc (≤-antisym m≤n n≤m)

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

-------------------------

show ≤ is a total order by induction over 1st and 2nd args

uses WITH
-}

≤-total : ∀ (m n : ℕ) → Total m n

-- base : 1st arg zero so forward case holds with z≤n as evidence that zero ≤ n
≤-total   zero       n = forward z≤n

-- base : 2nd arg zero so flipped case holds with z≤n as evidence that zero ≤ suc m
≤-total (suc m)   zero = flipped z≤n

-- inductive : IH establishes one of
≤-total (suc m) (suc n) with ≤-total m n
-- forward case of IH with m≤n as evidence that m ≤ n
-- follows that forward case of prop holds with s≤s m≤n as evidence that suc m ≤ suc n
... | forward m≤n = forward (s≤s m≤n)
-- flipped case of IH with n≤m as evidence that n ≤ m
-- follows that flipped case of prop holds with s≤s n≤m as evidence that suc n ≤ suc m
... | flipped n≤m = flipped (s≤s n≤m)

{-
WITH expression and one or more subsequent lines
each line begins ... | followed by pattern and right-hand side

WITH equivalent to defining a helper function:
-}

≤-total′ : ∀ (m n : ℕ) → Total m n
≤-total′   zero       n  = forward z≤n
≤-total′ (suc m)   zero  = flipped z≤n
≤-total′ (suc m) (suc n) = helper (≤-total′ m n)
 where
  helper : Total m n → Total (suc m) (suc n)
  helper (forward m≤n) = forward (s≤s m≤n)
  helper (flipped n≤m) = flipped (s≤s n≤m)

{-
WHERE

vars bound on the left-hand side of the preceding equation (e.g., m n) in scope in nested def
any identifiers bound in nested definition (e.g., helper)
in scope in right-hand side of preceding equation

If both arguments are equal, then both cases hold and we could return evidence of either.
In the code above we return the forward case, but there is a variant that returns the flipped case:
-}

≤-total″ : ∀ (m n : ℕ) → Total m n
≤-total″      m    zero  = flipped z≤n
≤-total″   zero  (suc n) = forward z≤n
≤-total″ (suc m) (suc n) with ≤-total″ m n
... | forward m≤n = forward (s≤s m≤n)
... | flipped n≤m = flipped (s≤s n≤m)

-- differs from original : pattern matches on 2nd arg before 1st

{-
------------------------------------------------------------------------------
MONOTONICITY ∀ {m n p q : ℕ} → m ≤ n → p ≤ q → m + p ≤ n + q

addition is monotonic on the right
proof by induction on 1st arg
-}

+-monoʳ-≤ : ∀ (n p q : ℕ)
  →     p ≤     q
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
  → m     ≤ n
    -------------
  → m + p ≤ n + p
+-monoˡ-≤ m n p m≤n         -- m + p ≤ n + p
  rewrite
    +-comm m p              -- p + m ≤ n + p
  | +-comm n p              -- p + m ≤ p + n
 = +-monoʳ-≤ p m n m≤n

-- combine above results:

+-mono-≤ : ∀ (m n p q : ℕ)
  → m     ≤ n
  →     p ≤     q
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
  → m     ≤ n
  →     p ≤     q
    -------------
  → m * p ≤ n * q
*-mono-≤ m n p q m≤n p≤q
  = ≤-trans
    (*-monoˡ-≤ m n p m≤n)
    (*-monoʳ-≤ n p q p≤q)

{-
------------------------------------------------------------------------------
STRICT INEQUALITY <
-}

infix 4 _<_

data _<_ : ℕ → ℕ → Set where

  z<s : ∀ {n : ℕ}
      ------------
    → zero < suc n -- diff from '≤' : 'zero ≤ n'

  s<s : ∀ {m n : ℕ}
    →     m <     n
      -------------
    → suc m < suc n

{-
NOT REFLEXIVE

IRREFLEXIVE : n < n never holds for any n

TRANSITIVE

NOT TOTAL

TRICHOTOMY: ∀ m n, one of m < n, m ≡ n, or m > n holds
- where m > n hold when n < m

MONOTONIC with regards to ADDITION and MULTIPLICATION

negation required to prove (so deferred to Chapter Negation)
- irreflexivity
- trichotomy case are mutually exclusive

------------------------------------------------------------------------------
show suc m ≤ n implies m < n (and conversely)
-}

m≤n→m<n : ∀ {m n : ℕ}
        → suc m ≤ n
          ---------
        →     m < n
m≤n→m<n  {zero}  {zero} ()
m≤n→m<n  {zero} {suc n} m≤n = z<s
m≤n→m<n {suc m} {suc n} m≤n = s<s (m≤n→m<n {m} {n} (inv-s≤s m≤n))

inv-s<s : ∀ {m n : ℕ}
        → suc m < suc n
          -------------
        →     m <     n
inv-s<s (s<s m<n) = m<n

m<n→m≤n : ∀ {m n : ℕ}
        → m < n
        → m ≤ n
m<n→m≤n  {zero} {suc n} m<n = z≤n
m<n→m≤n {suc m} {suc n} m<n = s≤s (m<n→m≤n {m} {n} (inv-s<s m<n))

{-
can then give an alternative derivation of the properties of strict inequality
(e.g., transitivity) by exploiting the corresponding properties of inequality

------------------------------------------------------------------------------
Exercise <-trans (recommended) : strict inequality is transitive.
-}

<-trans : ∀ {m n p : ℕ}
  → m < n
  → n < p
    -----
  → m < p
<-trans  {zero}     {n}     {p}      z<s  (s<s n<p) = z<s
<-trans {suc m} {suc n} {suc p} (s<s m<n) (s<s n<p) = s<s (<-trans {m} {n} {p} m<n n<p)

<-trans' : ∀ {m n p : ℕ}
  → m < n
  → n < p
    -----
  → m < p
<-trans'      z<s  (s<s n<p) = z<s
<-trans' (s<s m<n) (s<s n<p) = s<s (<-trans' m<n n<p)

{-
------------------------------------------------------------------------------
Exercise trichotomy (practice)

Show that strict inequality satisfies a weak version of trichotomy,
in the sense that for any m and n that one of the following holds:
- m < n
- m ≡ n
- m > n

Define m > n to be the same as n < m.

Need a data declaration, similar to that used for totality.
(Later will show that the three cases are exclusive after negation is introduced.)
-}

-- from 842
data Trichotomy (m n : ℕ) : Set where
  is-< : m < n → Trichotomy m n
  is-≡ : m ≡ n → Trichotomy m n
  is-> : n < m → Trichotomy m n

-- hc
<-trichotomy : ∀ (m n : ℕ) → Trichotomy m n
<-trichotomy   zero    zero  = is-≡ refl
<-trichotomy   zero  (suc n) = is-< z<s
<-trichotomy (suc m)   zero  = is-> z<s
<-trichotomy (suc m) (suc n) = helper (<-trichotomy m n)
 where
  helper : Trichotomy m n -> Trichotomy (suc m) (suc n)
  helper (is-< x) = is-< (s<s x)
  helper (is-≡ x) = is-≡ (cong suc x)
  helper (is-> x) = is-> (s<s x)

{-
------------------------------------------------------------------------------
Exercise +-mono-< (practice)

Show that addition is monotonic with respect to strict inequality.
As with inequality, some additional definitions may be required.
-}

-- HC
-- these proof are literal cut/paste/change '≤' to '<'
-- no additional defs were required

+-monoʳ-< : ∀ (n p q : ℕ)
  → p < q
    -------------
  → n + p < n + q
+-monoʳ-< zero    p q p<q  =  p<q
+-monoʳ-< (suc n) p q p<q  =  s<s (+-monoʳ-< n p q p<q)

+-monoˡ-< : ∀ (m n p : ℕ)
  → m < n
    -------------
  → m + p < n + p
+-monoˡ-< m n p m<n
  rewrite
    +-comm m p
  | +-comm n p
 = +-monoʳ-< p m n m<n

+-mono-< : ∀ (m n p q : ℕ)
  → m < n
  → p < q
    -------------
  → m + p < n + q
+-mono-< m n p q m<n p<q
  = <-trans
    (+-monoˡ-< m n p m<n)
    (+-monoʳ-< n p q p<q)

{-
------------------------------------------------------------------------------
Exercise ≤-iff-< (recommended) : suc m ≤ n implies m < n, and conversely
-}

-- 842 exercise: LEtoLTS (1 point) ≤-<-to

m≤n→m<sucn : ∀ {m n : ℕ}
  → m ≤     n
  → m < suc n
m≤n→m<sucn  {zero}         m≤n = z<s
m≤n→m<sucn {suc m} {suc n} m≤n = s<s (m≤n→m<sucn {m} {n} (inv-s≤s m≤n))

-- 842 exercise: LEStoLT (1 point) ≤-<--to′

sucm≤n→m<n : ∀ {m n : ℕ}
  → suc m ≤ n
  →     m < n
sucm≤n→m<n  {zero} {suc n} sucm≤n = z<s
sucm≤n→m<n {suc m} {suc n} sucm≤n = s<s (sucm≤n→m<n {m} {n} (inv-s≤s sucm≤n))

-- 842 exercise: LTtoSLE (1 point) ≤-<-from

m<n→sucm≤n : ∀ {m n : ℕ}
  →     m < n
  → suc m ≤ n
m<n→sucm≤n  {zero} {suc n} m<n = s≤s z≤n
m<n→sucm≤n {suc m} {suc n} m<n = s≤s (m<n→sucm≤n {m} {n} (inv-s<s m<n))

-- 842 exercise: LTStoLE (1 point) ≤-<-from′

m<sucn→m≤n : ∀ {m n : ℕ}
  → m < suc n
  → m ≤     n
m<sucn→m≤n  {zero}              z<n  = z≤n
m<sucn→m≤n {suc m} {suc n} (s<s m<n) = s≤s (m<sucn→m≤n {m} {n} m<n)
--                          ^
--                   critical move

{-
------------------------------------------------------------------------------
Exercise <-trans-revisited (practice) TODO

Give an alternative proof that strict inequality is transitive,
using the relation between strict inequality and inequality
and the fact that inequality is transitive.

-- 842: use the above to give a proof of <-trans that uses ≤-trans
-}

n<p→n<sucp : ∀ {n p : ℕ}
  → n <     p
  → n < suc p
n<p→n<sucp                 z<s = z<s
n<p→n<sucp {suc n} {suc p} n<p = s<s (n<p→n<sucp {n} {p} (inv-s<s n<p))

<-trans'' : ∀ {m n p : ℕ}
  → m < n
  → n < p
    -----
  → m < p
<-trans''  {zero} {suc n} {suc p}      z<s         _  = z<s
<-trans'' {suc m} {suc n} {suc p} (s<s m<n) (s<s n<p)
  = sucm≤n→m<n
      (≤-trans (m<n→sucm≤n (s<s             m<n))
               (m<sucn→m≤n (s<s (n<p→n<sucp n<p))))

{-
------------------------------------------------------------------------------
EVEN and ODD : MUTUALLY RECURSIVE DATATYPE DECLARATION and OVERLOADED CONSTRUCTORS

inequality and strict inequality are BINARY RELATIONS

even and odd are UNARY RELATIONS, sometimes called PREDICATES
-}

-- identifier must be defined before it is used, so declare both before giving constructors.
data even : ℕ → Set
data odd  : ℕ → Set

data even where

  zero :           -- overloaded with nat def
      ---------
      even zero

  suc  : ∀ {n : ℕ}
    → odd       n
      ------------
    → even (suc n)

data odd where

  suc  : ∀ {n : ℕ} -- suc is overloaded with one above and nat def
    → even     n
      -----------
    → odd (suc n)
{-
overloading constructors is OK (handled by type inference
overloading defined names NOT OK
best practice : only overload related meanings
-}

-- mutually recursive proof functions : give types first, then impls

-- sum of two even numbers is even
e+e≡e : ∀ {m n : ℕ}
  → even m
  → even n
    ------------
  → even (m + n)

-- sum of even and odd is odd
o+e≡o : ∀ {m n : ℕ}
  → odd m
  → even n
    -----------
  → odd (m + n)

-- given: zero is evidence that 1st is even
-- given: 2nd is even
-- result: even because 2nd is even
e+e≡e zero     en = en
-- given: 1st is even because it is suc of odd
-- given: 2nd is even
-- result: even because it is suc of sum of odd and even number, which is odd
e+e≡e (suc om) en = suc (o+e≡o om en)

-- given: 1st odd because it is suc of even
-- given: 2nd is event
-- result: odd because it is suc of sum of two evens, which is even
o+e≡o (suc em) en = suc (e+e≡e em en)

------------------------------------------------------------------------------
-- Exercise o+o≡e (stretch) : sum of two odd numbers is even
-- 842 Hint: need to define another theorem and prove both by mutual induction

-- sum of odd and even is odd
e+o≡o : ∀ {m n : ℕ}
  → even m
  → odd n
    -----------
  → odd (m + n)

-- sum of odd and odd is even
o+o≡e : ∀ {m n : ℕ}
  → odd m
  → odd n
  --------------
  → even (m + n)

e+o≡o    zero  on = on
e+o≡o (suc em) on = suc (o+o≡e em on)

o+o≡e (suc om) on = suc (e+o≡o om on)

{-
------------------------------------------------------------------------------
Exercise Bin-predicates (stretch) TODO

representations not unique due to leading zeros

eleven:
  ⟨⟩     I O I I --     canonical
  ⟨⟩ O O I O I I -- not canonical
-}

data Bin : Set where
  ⟨⟩ : Bin
  _O : Bin → Bin
  _I : Bin → Bin

inc : Bin → Bin
inc  ⟨⟩    = ⟨⟩      I
inc (b  O) =      b  I
inc (b  I) = (inc b) O

to : ℕ → Bin
to   zero  = ⟨⟩ O
to (suc m) = inc (to m)

dbl : ℕ → ℕ
dbl   zero  = zero
dbl (suc m) = suc (suc (dbl m))

from : Bin → ℕ
from     ⟨⟩ = 0
from (b  O) =      dbl (from b)
from (b  I) = suc (dbl (from b))

-- httpS://www.reddit.com/r/agda/comments/hrvk07/plfa_quantifiers_help_with_binisomorphism/

-- holds if bitstring has a leading one
data One : Bin → Set where
  one    :                 One (⟨⟩ I)
  _withO : ∀ {b} → One b → One  (b O)
  _withI : ∀ {b} → One b → One  (b I)

{-
Hint: to prove below
- first state/prove properties of One
-}

n≤1+n : ∀ (n : ℕ) → n ≤ 1 + n
n≤1+n   zero  = z≤n
n≤1+n (suc n) = s≤s (n≤1+n n)

dbl-mono : ∀ (n : ℕ) → n ≤ dbl n
dbl-mono  zero   = z≤n
dbl-mono (suc n) = s≤s (≤-trans (dbl-mono n) (n≤1+n (dbl n)))

-- next one (and its cleaned up version) I thought would be useful - but not used
-- first try
n≤fromb→n≤dblfromb' : ∀ {n : ℕ} {b : Bin} → n ≤ from b → n ≤ dbl (from b)
n≤fromb→n≤dblfromb'  {zero} {b}        p  = z≤n
n≤fromb→n≤dblfromb' {suc n} {b O}      p  =
                ≤-trans p (dbl-mono      (from (b O)))
n≤fromb→n≤dblfromb' {suc n} {b I} (s≤s p) =
  s≤s (≤-trans (≤-trans p (dbl-mono (dbl (from b))))
               (n≤1+n (dbl (dbl (from b)))))

-- cleaned up
n≤fromb→n≤dblfromb : ∀ {n : ℕ} {b : Bin}
  → n ≤      from b
  → n ≤ dbl (from b)
n≤fromb→n≤dblfromb {_} {b} p = ≤-trans p (dbl-mono (from b))

-- HINT: prove if One b then 1 is less or equal to the result of from b
oneb→1≤fromb : ∀ {b : Bin} → One b → 1 ≤ from b
oneb→1≤fromb {b I}        _  = s≤s z≤n
oneb→1≤fromb {b O} (p withO) = ≤-trans (oneb→1≤fromb p) (dbl-mono (from b))

oneb→0<fromb : ∀ {b : Bin} → One b → 0 < from b
oneb→0<fromb p = m≤n→m<n (oneb→1≤fromb p)

num-trailing-zeros : (b : Bin) {p : One b} → ℕ
num-trailing-zeros (b I)           = 0
num-trailing-zeros (b O) {p withO} = 1 + num-trailing-zeros b {p}

do-dbls : ℕ -> ℕ
do-dbls   zero  = suc zero
do-dbls (suc n) = dbl (do-dbls n)

xxx : {b : Bin} {n : ℕ}
  → (p : One b)
  →         n ≡ num-trailing-zeros b {p}
  → do-dbls n ≤ from b
xxx {⟨⟩ I}  {zero}       one  ntz = s≤s z≤n
xxx {b  I}  {zero} (ob withI) ntz = s≤s z≤n
xxx {b  O}  {zero} (ob withO) ntz = ≤-trans (oneb→1≤fromb ob) (dbl-mono (from b))
xxx {b  O} {suc n} (ob withO) ntz = {!!}

-- bitstring is canonical if
data Can : Bin → Set where
  czero : Can (⟨⟩ O)  -- it consists of a single zero (representing zero)

  cone : ∀ {b : Bin}
    → One b           -- it has a leading one (representing a positive number)
      -----
    → Can b
{-
--------------------------------------------------
Show that increment preserves canonical bitstrings: TODO
-}

one→inc-one : ∀ {b : Bin} → One b -> One (inc b)
one→inc-one {⟨⟩}   ()
one→inc-one {b  O} (ob withO) = {!!}
one→inc-one {b  I} (ob withI) = (one→inc-one {b} ob) withO
one→inc-one {⟨⟩ I}       one  = one→inc-one {!!} withO

canb→canincb : ∀ {b : Bin}
  → Can      b
    ----------
  → Can (inc b)
canb→canincb {⟨⟩ O}   czero  = cone {!!}
canb→canincb {b  O} (cone x) = cone {!!}
canb→canincb {b  I} (cone x) = {!!}

{-
--------------------------------------------------
Show that converting a natural to a bitstring always yields a canonical bitstring: TODO
-}

can-to-n : ∀ {n : ℕ} → Can (to n)
can-to-n  {zero} = czero
can-to-n {suc n} = {!!}

{-
--------------------------------------------------
Show that converting a canonical bitstring to a natural and back is the identity: TODO
-}

canb→tofromb≡b : ∀ {b : Bin}
  → Can      b
    ---------------
  → to (from b) ≡ b
canb→tofromb≡b   czero  = refl
canb→tofromb≡b {b O} (cone x) = {!!}
canb→tofromb≡b {b I} (cone x) = {!!}

{-
------------------------------------------------------------------------------
Standard library

defs in this chapter in standard library:

import Data.Nat using (_≤_; z≤n; s≤s)
import Data.Nat.Properties using (≤-refl; ≤-trans; ≤-antisym; ≤-total;
                                  +-monoʳ-≤; +-monoˡ-≤; +-mono-≤)

------------------------------------------------------------------------------
Unicode

≤  U+2264  LESS-THAN OR EQUAL TO    (\<=, \le)
≥  U+2265  GREATER-THAN OR EQUAL TO (\>=, \ge)
ˡ  U+02E1  MODIFIER LETTER SMALL L  (\^l)
ʳ  U+02B3  MODIFIER LETTER SMALL R  (\^r)
-}
