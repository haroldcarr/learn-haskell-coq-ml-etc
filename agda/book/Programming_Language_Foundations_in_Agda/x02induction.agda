module x02induction where

-- prove properties of inductive naturals and operations on them via induction

import      Relation.Binary.PropositionalEquality as Eq
open        Eq             using (_≡_; refl; cong; sym)
open        Eq.≡-Reasoning using (begin_; _≡⟨⟩_; step-≡; _∎)
open import Data.Nat       using (ℕ; zero; suc; _+_; _*_; _∸_)

{-
------------------------------------------------------------------------------
## Properties of operators

* _Identity_.   left/right/both; sometimes called _unit_

* _Associativity_.   e.g., `(m + n) + p ≡ m + (n + p)`

* _Commutativity_.   e.g., `m + n ≡ n + m`

* _Distributivity_.  e.g., from the left  `(m + n) * p ≡ (m * p) + (n * p)`
                           from the right `m * (p + q) ≡ (m * p) + (m * q)`

#### Exercise `operators` (practice) {name=operators} TODO

pair of operators
- have an identity
- are associative, commutative, and distribute over one another
(do not prove)

operator
- has identity
- is associative
- not commutative
(do not prove)
-}

{-
HC not associative; right identity
-}
_ =
  begin
    (3 ∸ 1) ∸ 1 ∸ 0
  ≡⟨⟩
     2      ∸ 1 ∸ 0
  ≡⟨⟩
     1          ∸ 0
  ≡⟨⟩
     1          ∸ 0
  ∎
{-
------------------------------------------------------------------------------
## ASSOCIATIVITY

    (m + n) + p ≡ m + (n + p)
-}

_ : (3 + 4) + 5 ≡ 3 + (4 + 5)
_ =
  begin
    (3 +  4) + 5
  ≡⟨⟩
     7       + 5
  ≡⟨⟩
    12
  ≡⟨⟩
     3 +  9
  ≡⟨⟩
     3 + (4  + 5)
  ∎

{-
useful to read chains like above
- from top down  until reaching simplest term (i.e., `12`)
- from bottom up until reaching the same term

Why should `7 + 5` be the same as `3 + 9`?
Perhaps gather more evidence, testing the proposition by choosing other numbers.
But infinite naturals so testing can never be complete.

## Proof by induction

definition of naturals has
- _base case_
- _inductive case_

proof by induction follows structure of that definition
prove two cases
- _base case_ : show property holds for `zero`
- _inductive case_ :
  - assume property holds for an arbitrary natural `m` (the _inductive hypothesis_)
  - then show that the property holds for `suc m`

    ------
    P  zero

    P      m
    ---------
    P (suc m)

- initially, no properties are known.
- base case : `P zero` holds : add it to set of known properties
    -- On the first day, one property is known.
    P zero
- inductive case tells us that if `P m` holds
    -- On the second day, two properties are known.
    P zero
    P (suc zero)
  then `P (suc m)` also holds

    -- On the third day, three properties are known.
    P zero
    P (suc zero)
    P (suc (suc zero))

    -- On the fourth day, four properties are known.
    P zero
    P (suc zero)
    P (suc (suc zero))
    P (suc (suc (suc zero)))

the process continues: property `P n` first appears on day _n+1_

------------------------------------------------------------------------------
## ASSOCIATIVITY

to prove associativity, take `P m` to be the property:

    (m + n) + p ≡ m + (n + p)

`n` and `p` are arbitrary natural numbers
show the equation holds for all `m` it will also hold for all `n` and `p`


    -------------------------------
     (zero + n) + p ≡  zero + (n + p)


        (m + n) + p ≡     m + (n + p)
    ---------------------------------
    (suc m + n) + p ≡ suc m + (n + p)


demonstrate both of above, then associativity of addition follows by induction
-}

-- signature says providing evidence for proposition
+-assoc : ∀ (m n p : ℕ)
        → (m +  n) + p
        ≡  m + (n  + p)

-- Evidence is a function that accepts three natural numbers, binds them to `m`, `n`, and `p`,
-- and returns evidence for the corresponding instance of the equation.

-- base case : show: (zero +  n) + p
--                  ≡ zero + (n  + p)
+-assoc zero n p =
  begin
    (zero +  n) + p
  ≡⟨⟩
             n  + p  -- using _+_ base case : "simplifying" both sides yields n + p ≡ n + p
  ≡⟨⟩
     zero + (n  + p)
  ∎

-- inductive case : show: (suc m +  n) + p
--                       ≡ suc m + (n  + p)
+-assoc (suc m) n p =
  begin
    (suc   m +  n) + p
  ≡⟨⟩
     suc  (m +  n) + p  -- via _+_ inductive case (left to right)
  ≡⟨⟩
     suc ((m +  n) + p) -- simplifying both sides : suc ((m + n) + p) ≡ suc (m + (n + p))
                        -- follows by prefacing `suc` to both sides of the induction hypothesis:
                        --                               (m + n) + p  ≡      m + (n + p)
  ≡⟨ cong suc (+-assoc m n p) ⟩
     suc  (m + (n  + p))
  ≡⟨⟩
     suc   m + (n  + p) -- via _+_ inductive case (right to left)
  ∎

-- HC minimal version

+-assoc' : ∀ (m n p : ℕ)
        → (m +  n) + p
        ≡  m + (n  + p)
+-assoc' zero    n p = refl
+-assoc' (suc m) n p = cong suc (+-assoc m n p)

{-
identifiers can have any characters NOT including spaces or the characters @.(){};_

the "middle" equation, does not follow from applying _+_ (i.e., "simplification") alone
- `_≡⟨_⟩_` : called "chain reasoning"
- justification for equation given in angle brackets:
  - empty means "simplification", or
  - something more, e.g.,:

    ⟨ cong suc (+-assoc m n p) ⟩

recursive invocation `+-assoc m n p`
- has type of the induction hypothesis
- `cong suc` prefaces `suc` to each side of inductive hypothesis

A relation is a CONGRUENCE for a given function if the relation
is preserved by applying the function.

if `e` is evidence that `x ≡ y`, then `cong f e` is evidence `f x ≡ f y`, for any `f`

here the inductive hypothesis is not assumed

instead, proved by recursive invocation of the function being defined, `+-assoc m n p`

well founded : associativity of larger numbers is proved in of associativity of smaller numbers.

e.g., `assoc (suc m) n p` is proved using `assoc m n p`.

------------------------------------------------------------------------------
## Induction as recursion

Concrete example of how induction corresponds to recursion : instantiate `m` to `2`
-}

+-assoc-2 : ∀ (n p : ℕ)
          → (2 +  n) + p
          ≡  2 + (n  + p)
+-assoc-2 n p =
  begin
         (2 +  n) + p
  ≡⟨⟩
    suc  (1 +  n) + p
  ≡⟨⟩
    suc ((1 +  n) + p)
  ≡⟨ cong suc (+-assoc-1 n p) ⟩
    suc  (1 + (n  + p))
  ≡⟨⟩
          2 + (n  + p)
  ∎
 where
  +-assoc-1 : ∀ (n p : ℕ) → (1 + n) + p ≡ 1 + (n + p)
  +-assoc-1 n p =
    begin
           (1 +  n) + p
    ≡⟨⟩
      suc  (0 +  n) + p
    ≡⟨⟩
      suc ((0 +  n) + p)
    ≡⟨ cong suc (+-assoc-0 n p) ⟩
      suc  (0 + (n  + p))
    ≡⟨⟩
            1 + (n  + p)
    ∎
   where
    +-assoc-0 : ∀ (n p : ℕ) → (0 + n) + p ≡ 0 + (n + p)
    +-assoc-0 n p =
      begin
        (0 + n) + p
      ≡⟨⟩
             n  + p
      ≡⟨⟩
        0 + (n  + p)
      ∎
{-
------------------------------------------------------------------------------
## Terminology and notation

Evidence for a universal quantifier is a function.  The notations

    +-assoc : ∀ (m n p : ℕ)
            → (m +  n) + p
            ≡  m + (n  + p)

and

    +-assoc : ∀ (m : ℕ)
            → ∀ (n : ℕ)
            → ∀ (p : ℕ)
            → (m +  n) + p
            ≡  m + (n  + p)

are equivalent.

differ from function type such as `ℕ → ℕ → ℕ`
- variables are associated with each argument type
- the result type may mention (or depend upon) these variables
- hence called _DEPENDENT functions_

------------------------------------------------------------------------------
## COMMUTATIVITY

    m + n ≡ n + m

two lemmas used in proof

### first lemma

The base case of the definition of addition states that zero is a left-identity:

    zero + n ≡ n

First lemma states that zero is also a right-identity:
-}

-- proof by induction on `m`
+-identityʳ : ∀ (m : ℕ)
            → m + zero
            ≡ m

-- base case : show:
--    zero + zero
--  ≡ zero
+-identityʳ zero =
  begin
    zero + zero
  ≡⟨⟩ -- via _+_ base
    zero
  ∎

-- inductive case : show:
--     (suc m) + zero
--    = suc m
+-identityʳ (suc m) =
  begin
    suc  m + zero
  ≡⟨⟩
    suc (m + zero)              -- via _+_ inductive
  ≡⟨ cong suc (+-identityʳ m) ⟩ -- recursive invocation `+-identityʳ m`
                                -- has type of induction hypothesis
                                --      m + zero  ≡     m
                                -- `cong suc` prefaces `suc` to each side of that type, yielding
                                -- suc (m + zero) ≡ suc m
    suc  m
  ∎
{-
### second lemma

inductive case of _+_ pushes `suc` on 1st arg to the outside:

    suc m + n ≡ suc (m + n)

second lemma does same for `suc` on 2nd arg:

    m + suc n ≡ suc (m + n)
-}

-- signature states defining `+-suc` which provides evidence for the proposition/type
+-suc : ∀ (m n : ℕ)
      →      m + suc n
      ≡ suc (m +     n)

-- evidence is fun that takes two nats, binds to `m` and `n`
-- returns evidence for the corresponding instance of the equation
-- proof is by induction on `m`

-- base case
+-suc zero n =
  begin
         zero + suc n
  ≡⟨⟩
                suc n  -- via _+_ base
  ≡⟨⟩
    suc (zero +     n) -- via _+_ base
  ∎
-- inductive case : show:      suc m + suc n
--                      ≡ suc (suc m +     n)
+-suc (suc m) n =
  begin
    suc       m + suc n
  ≡⟨⟩
    suc      (m + suc n)    -- via _+_ inductive
  ≡⟨ cong suc (+-suc m n) ⟩
    suc (suc (m +     n))   -- induction
  ≡⟨⟩
    suc (suc  m +     n)    -- via _+_ inductive
  ∎

{-
------------------------------------------------------------------------------
-}

+-comm : ∀ (m n : ℕ)
       → m + n
       ≡ n + m
+-comm m zero =
  begin
           m + zero
  ≡⟨ +-identityʳ m ⟩
           m
  ≡⟨⟩
    zero + m
  ∎
+-comm m (suc n) =
  begin
         m + suc n           -- _+_ inductive
  ≡⟨ +-suc m n ⟩
    suc (m +     n)          -- via +-suc
  ≡⟨ cong suc (+-comm m n) ⟩ -- congruence and induction hypothesis
    suc (n +     m)          -- _+_ inductive
  ≡⟨⟩
    suc  n +     m
  ∎

{-
Agda requires defining identifiers before use, thus lemmas before props using them.

------------------------------------------------------------------------------
## COROLLARY: REARRANGING : apply associativity to rearrange parentheses
-}

+-rearrange : ∀ (m n p q : ℕ)
            → (m +  n) + (p  + q)
            ≡  m + (n  +  p) + q
+-rearrange m n p q =
  begin
    (m +   n) + (p   + q)
  ≡⟨ +-assoc m n (p + q) ⟩
     m +  (n  + (p   + q))
  ≡⟨ cong (m +_) (sym (+-assoc n p q)) ⟩
     m + ((n  +  p)  + q)
  ≡⟨ sym (+-assoc m (n + p) q) ⟩
    (m +  (n  +  p)) + q
  ≡⟨⟩
     m +  (n  +  p)  + q
  ∎

{-
no induction is required
A few points are worthy of note.

addition is left associative : m + (n + p)  + q
                            = (m + (n + p)) + q

`sym` : interchange sides of an equation, e.g.,
- `+-assoc n p q` shifts parens right to left:

    (n + p) + q ≡ n + (p + q)

`sym (+-assoc n p q)`: to shift them left-to-right

    n + (p + q) ≡ (n + p) + q

general
- if `e` provides evidence for `x ≡ y`
- then `sym e` provides evidence for `y ≡ x`

_section_ notation (introduced by Richard Bird) : `(x +_)`    `(_+ x)`

------------------------------------------------------------------------------
## Creation, one last time

base case : `(zero + n) + p ≡ zero + (n + p)`

inductive case :
- if `(m + n) + p ≡     m + (n + p)` then
 `(suc m + n) + p ≡ suc m + (n + p)`

using base case, associativity of zero on left:
    (0 + 0) + 0 ≡ 0 + (0 + 0)   ...   (0 + 4) + 5 ≡ 0 + (4 + 5)   ...
using inductive case
    (1 + 0) + 0 ≡ 1 + (0 + 0)   ...   (1 + 4) + 5 ≡ 1 + (4 + 5)   ...
    (2 + 0) + 0 ≡ 2 + (0 + 0)   ...   (2 + 4) + 5 ≡ 2 + (4 + 5)   ...
    (3 + 0) + 0 ≡ 3 + (0 + 0)   ...   (3 + 4) + 5 ≡ 3 + (4 + 5)   ...
    ...

there is a finite approach to generating the same equations (following exercise)

------------------------------------------------------------------------------
#### Exercise `finite-|-assoc` (stretch) {name=finite-plus-assoc}

Write out what is known about associativity of addition on each of the
first four days using a finite story of creation, as
[earlier](/Naturals/#finite-creation).

```
-- TODO
```

------------------------------------------------------------------------------
## proof of Associativity using `rewrite` (rather than chains of equations)

avoids chains of equations and the need to invoke `cong`
-}

+-assoc′ : ∀ (m n p : ℕ)
         → (m +  n) + p
         ≡  m + (n  + p)

-- base
-- show: (zero + n) + p ≡ zero + (n + p)
-- _+_ base applied "invisibly", then terms equal/refl
+-assoc′ zero    n p                         = refl

-- inductive
-- show: (suc m + n) + p ≡ suc m + (n + p)
-- _+_ inductive applied "invisibly" giving: suc ((m + n) + p) ≡ suc (m + (n + p))
-- rewrite with the inductive hypothesis
-- then terms are equal/refl
+-assoc′ (suc m) n p  rewrite +-assoc′ m n p = refl

{-
rewriting by a given equation
- indicated by keyword `rewrite`
- followed by a proof of that equation

------------------------------------------------------------------------------
## Commutativity with rewrite
-}

+-identityL : ∀ (n : ℕ) → n + zero ≡ n
+-identityL zero = refl
+-identityL (suc n) rewrite +-identityL n = refl

+-suc′ : ∀ (m n : ℕ) → m + suc n ≡ suc (m + n)
+-suc′ zero n = refl
+-suc′ (suc m) n rewrite +-suc′ m n = refl

+-comm′ : ∀ (m n : ℕ) → m + n ≡ n + m
+-comm′ m zero rewrite +-identityL m = refl
-- rewriting with two equations indicated by separating the two proofs
-- of the relevant equations by a vertical bar
-- left rewrite performed before right
+-comm′ m (suc n)  -- m + suc n ≡ suc  n + m
                   -- m + suc n ≡ suc (n + m) -- def/eq
  rewrite
    +-suc′  m n    -- suc (m + n) ≡ suc (n + m)
  | +-comm′ m n    -- suc (n + m) ≡ suc (n + m)
  = refl

{-
------------------------------------------------------------------------------
HC
-}

*-identityʳ : ∀ (n : ℕ) → n * 1 ≡ n
*-identityʳ zero = refl
*-identityʳ (suc n) rewrite *-identityʳ n = refl

*-identityL : ∀ (n : ℕ) → 1 * n ≡ n
*-identityL zero = refl
*-identityL (suc n) rewrite *-identityL n = refl

{-
------------------------------------------------------------------------------
#### Exercise `+-swap` (recommended) {name=plus-swap}

Show

    m + (n + p) ≡ n + (m + p)

for all naturals `m`, `n`, and `p`
- no induction
- use associativity and commutativity of addtion
-}

+-swap : ∀ (m n p : ℕ)
       → m + (n + p)
       ≡ n + (m + p)
+-swap m n p =
  begin
     m + (n  + p)
  ≡⟨ sym (+-assoc m n p) ⟩
    (m +  n) + p
  ≡⟨ cong (_+ p) (sym (+-comm n m)) ⟩
    (n +  m) + p
  ≡⟨ +-assoc n m p ⟩
     n + (m  + p)
  ∎

{-
------------------------------------------------------------------------------
#### Exercise `*-distrib-+` (recommended) {name=times-distrib-plus}

Show multiplication distributes over addition

    (m + n) * p ≡ m * p + n * p

for all naturals `m`, `n`, and `p`.
-}
{-
_*r_ : ℕ → ℕ → ℕ
m *r zero    = zero        --  m  *  0      ≡  0
m *r (suc n) = m + (n * m) --  m  * (1 + n) ≡  m + (n * m)

*0 : ∀ (m : ℕ) -> m * 0 ≡ 0
*0 zero = refl
*0 (suc m) rewrite *0 m = refl

*-distrib-+ : ∀ (m n p : ℕ)
            → (m     + n) * p
            ≡  m * p + n  * p
*-distrib-+ m n zero    -- (m + n) * zero ≡ m * zero + n * zero
  rewrite
    *0 (m + n)          --              0 ≡ m * 0    + n * 0
  | *0 m                --              0 ≡            n * 0
  | *0 n                --              0 ≡                0
  = refl
*-distrib-+ m n (suc p) -- (m + n) * suc p ≡ m * suc p + n * suc p
  = {!!}
-}

*-distrib-+r : ∀ (m n p : ℕ)
             → (m     + n) * p
             ≡  m * p + n  * p
*-distrib-+r  zero   y z = refl
*-distrib-+r (suc x) y z -- (suc x     + y) * z  ≡ suc x * z + y * z
                         -- z + (x     + y) * z  ≡ z + x * z + y * z
  rewrite
    *-distrib-+r x y z   -- z + (x * z + y  * z) ≡ z + x * z + y * z
  = (sym (+-assoc z (x * z) (y * z)))

{-
------------------------------------------------------------------------------
#### Exercise `*-assoc` (recommended) {name=times-assoc}

Show multiplication is associative, that is,

    (m * n) * p ≡ m * (n * p)

for all naturals `m`, `n`, and `p`.
TODO
-}
{-
*-assoc : ∀ (m n p : ℕ)
        → (m *  n) * p
        ≡  m * (n  * p)
-- base case : show: (zero *  n) * p
--                  ≡ zero * (n  * p)
*-assoc zero n p = refl
-- inductive case : show: (suc m *  n) * p
--                       ≡ suc m * (n  * p)
*-assoc (suc m) n p = {!!}
  begin
    (suc   m *  n)  * p
  ≡⟨⟩
    (n +  (m *  n)) * p
  ≡⟨⟩
    suc m * (n  * p)
  ∎
-}
{-
------------------------------------------------------------------------------
#### Exercise `*-comm` (practice) {name=times-comm}

Show multiplication is commutative, that is,

    m * n ≡ n * m

for all naturals `m` and `n`.  As with commutativity of addition,
you will need to formulate and prove suitable lemmas.

```
-- Your code goes here
```

------------------------------------------------------------------------------
#### Exercise `0∸n≡0` (practice) {name=zero-monus}

Show

    zero ∸ n ≡ zero

for all naturals `n`. Did your proof require induction?

```
-- Your code goes here
```

------------------------------------------------------------------------------
#### Exercise `∸-|-assoc` (practice) {name=monus-plus-assoc}

Show that monus associates with addition, that is,

    m ∸ n ∸ p ≡ m ∸ (n + p)

for all naturals `m`, `n`, and `p`.

```
-- Your code goes here
```

------------------------------------------------------------------------------
#### Exercise `+*^` (stretch)

Show the following three laws

     m ^ (n + p) ≡ (m ^ n) * (m ^ p)  (^-distribˡ-|-*)
     (m * n) ^ p ≡ (m ^ p) * (n ^ p)  (^-distribʳ-*)
     (m ^ n) ^ p ≡ m ^ (n * p)        (^-*-assoc)

for all `m`, `n`, and `p`.

------------------------------------------------------------------------------
#### Exercise `Bin-laws` (stretch) {name=Bin-laws}

Recall that
Exercise [Bin](/Naturals/#Bin)
defines a datatype `Bin` of bitstrings representing natural numbers,
and asks you to define functions

    inc   : Bin → Bin
    to    : ℕ → Bin
    from  : Bin → ℕ

Consider the following laws, where `n` ranges over naturals and `b`
over bitstrings:

    from (inc b) ≡ suc (from b)
    to (from b) ≡ b
    from (to n) ≡ n

For each law: if it holds, prove; if not, give a counterexample.

```
-- Your code goes here
```

------------------------------------------------------------------------------
## Standard library

Definitions similar to those in this chapter can be found in the standard library:
```
import Data.Nat.Properties using (+-assoc; +-identityʳ; +-suc; +-comm)
```

------------------------------------------------------------------------------
## Unicode

This chapter uses the following unicode:

    ∀  U+2200  FOR ALL (\forall, \all)
    ʳ  U+02B3  MODIFIER LETTER SMALL R (\^r)
    ′  U+2032  PRIME (\')
    ″  U+2033  DOUBLE PRIME (\')
    ‴  U+2034  TRIPLE PRIME (\')
    ⁗  U+2057  QUADRUPLE PRIME (\')

Similar to `\r`, the command `\^r` gives access to a variety of
superscript rightward arrows, and also a superscript letter `r`.
The command `\'` gives access to a range of primes (`′ ″ ‴ ⁗`).
-}
