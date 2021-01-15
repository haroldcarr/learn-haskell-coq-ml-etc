module x02induction where

-- prove properties of inductive naturals and operations on them via induction

import      Relation.Binary.PropositionalEquality as Eq
open        Eq             using (_≡_; refl; cong; sym)
open        Eq.≡-Reasoning using (begin_; _≡⟨⟩_; step-≡; _∎)
open import Data.Nat       using (ℕ; zero; suc; _+_; _*_; _∸_; _^_)

{-
------------------------------------------------------------------------------
## Properties of operators : identity, associativity, commutativity, distributivity

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
    (3 ∸ 1) ∸ 1 ∸ 0 ≡⟨⟩
     2      ∸ 1 ∸ 0 ≡⟨⟩
     1          ∸ 0 ≡⟨⟩
     1          ∸ 0
  ∎
{-
------------------------------------------------------------------------------
## ASSOCIATIVITY of ADDITION : (m + n) + p ≡ m + (n + p)
-}

_ : (3 + 4) + 5 ≡ 3 + (4 + 5)
_ =
  begin
    (3 +  4) + 5  ≡⟨⟩
     7       + 5  ≡⟨⟩
    12            ≡⟨⟩
     3 +  9       ≡⟨⟩
     3 + (4  + 5)
  ∎

{-
useful to read chains like above
- from top down  until reaching simplest term (i.e., `12`)
- from bottom up until reaching the same term

Why should `7 + 5` be the same as `3 + 9`?
Perhaps gather more evidence, testing the proposition by choosing other numbers.
But infinite naturals so testing can never be complete.

## PROOF BY INDUCTION

natural definition has
- base case
- inductive case

inductive proof follows structure of definition: prove two cases
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
## PROVE ASSOCIATIVITY of ADDITION

take `P m` to be the property:

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

-- Evidence is a function that
-- - takes three natural numbers, binds them to `m`, `n`, and `p`
-- - returns evidence for the corresponding instance of the equation

-- base case : show: (zero +  n) + p
--                  ≡ zero + (n  + p)
+-assoc zero n p =
  begin
  (zero +  n) + p ≡⟨⟩ -- _+_ base case
           n  + p ≡⟨⟩
   zero + (n  + p)
  ∎

-- inductive case : show: (suc m +  n) + p
--                       ≡ suc m + (n  + p)
+-assoc (suc m) n p =
  begin
  (suc   m +  n) + p   ≡⟨⟩ -- _+_ inductive case (left to right)
   suc  (m +  n) + p   ≡⟨⟩
   suc ((m +  n) + p)  ≡⟨ cong suc (+-assoc m n p) ⟩
                           -- simplifying both sides : suc ((m + n) + p) ≡ suc (m + (n + p))
                           -- follows by prefacing `suc` to both sides of the induction hypothesis:
                           --                               (m + n) + p  ≡      m + (n + p)
   suc  (m + (n  + p)) ≡⟨⟩ -- _+_ inductive case (right to left)
   suc   m + (n  + p)
  ∎

-- HC minimal version

+-assoc' : ∀ (m n p : ℕ)
        → (m +  n) + p
        ≡  m + (n  + p)
+-assoc'   zero  n p = refl
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

A relation is a CONGRUENCE for a given function
if the relation is preserved by applying the function.

if `e` is evidence that `x ≡ y`, then `cong f e` is evidence `f x ≡ f y`, for any `f`

here the inductive hypothesis is not assumed

instead, proved by recursive invocation of the function being defined, `+-assoc m n p`

WELL FOUNDED : associativity of larger numbers is proved in of associativity of smaller numbers.

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
         (2 +  n) + p   ≡⟨⟩
    suc  (1 +  n) + p   ≡⟨⟩
    suc ((1 +  n) + p)  ≡⟨ cong suc (+-assoc-1 n p) ⟩
    suc  (1 + (n  + p)) ≡⟨⟩
          2 + (n  + p)
  ∎
 where
  +-assoc-1 : ∀ (n p : ℕ) → (1 + n) + p ≡ 1 + (n + p)
  +-assoc-1 n p =
    begin
           (1 +  n) + p   ≡⟨⟩
      suc  (0 +  n) + p   ≡⟨⟩
      suc ((0 +  n) + p)  ≡⟨ cong suc (+-assoc-0 n p) ⟩
      suc  (0 + (n  + p)) ≡⟨⟩
            1 + (n  + p)
    ∎
   where
    +-assoc-0 : ∀ (n p : ℕ) → (0 + n) + p ≡ 0 + (n + p)
    +-assoc-0 n p =
      begin
        (0 + n) + p  ≡⟨⟩
             n  + p  ≡⟨⟩
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
## COMMUTATIVITY of ADDITION : m + n ≡ n + m

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
    zero + zero ≡⟨⟩ -- _+_ base
    zero
  ∎

-- inductive case : show:
--     (suc m) + zero
--    = suc m
+-identityʳ (suc m) =
  begin
    suc  m + zero  ≡⟨⟩ -- _+_ inductive
    suc (m + zero) ≡⟨ cong suc (+-identityʳ m) ⟩
                       -- recursive invocation `+-identityʳ m`
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
         zero + suc n  ≡⟨⟩ -- _+_ base
                suc n  ≡⟨⟩ -- _+_ base
    suc (zero +     n)
  ∎
-- inductive case : show:      suc m + suc n
--                      ≡ suc (suc m +     n)
+-suc (suc m) n =
  begin
    suc       m + suc n   ≡⟨⟩                       -- _+_ inductive
    suc      (m + suc n)  ≡⟨ cong suc (+-suc m n) ⟩ -- induction
    suc (suc (m +     n)) ≡⟨⟩                       -- _+_ inductive
    suc (suc  m +     n)
  ∎

-------------------------

+-comm : ∀ (m n : ℕ)
       → m + n
       ≡ n + m
+-comm m zero =
  begin
           m + zero ≡⟨ +-identityʳ m ⟩
           m        ≡⟨⟩
    zero + m
  ∎
+-comm m (suc n) =
  begin
         m + suc n  ≡⟨ +-suc m n ⟩
    suc (m +     n) ≡⟨ cong suc (+-comm m n) ⟩ -- congruence and induction hypothesis
    suc (n +     m) ≡⟨⟩                        -- _+_ inductive
    suc  n +     m
  ∎

{-
definition required BEFORE using them

------------------------------------------------------------------------------
## COROLLARY: REARRANGING : apply associativity to rearrange parentheses (SYM; sections)
-}

+-rearrange : ∀ (m n p q : ℕ)
            → (m +  n) + (p  + q)
            ≡  m + (n  +  p) + q
+-rearrange m n p q =
  begin
    (m +   n) + (p   + q)  ≡⟨ +-assoc m n (p + q) ⟩
     m +  (n  + (p   + q)) ≡⟨ cong (m +_) (sym (+-assoc n p q)) ⟩
     m + ((n  +  p)  + q)  ≡⟨ sym (+-assoc m (n + p) q) ⟩
    (m +  (n  +  p)) + q   ≡⟨⟩
     m +  (n  +  p)  + q
  ∎

{-
no induction is required

NOTE:

addition is left associative : m + (n + p)  + q
                            = (m + (n + p)) + q

SYM : interchange sides of an equation, e.g.,
- `+-assoc n p q` shifts parens right to left:

    (n + p) + q ≡ n + (p + q)

`sym (+-assoc n p q)`: to shift them left-to-right

    n + (p + q) ≡ (n + p) + q

general
- if `e` provides evidence for `x ≡ y`
- then `sym e` provides evidence for `y ≡ x`

SECTION NOTATION (introduced by Richard Bird) : `(x +_)`    `(_+ x)`

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
#### Exercise `finite-|-assoc` (stretch) {name=finite-plus-assoc} TODO

Write out what is known about associativity of addition on each of the
first four days using a finite story of creation, as
[earlier](/Naturals/#finite-creation).

------------------------------------------------------------------------------
## proof of ASSOCIATIVITY using `rewrite` (rather than chains of equations)

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
## COMMUTATIVITY with rewrite
-}

+-suc′ : ∀ (m n : ℕ) → m + suc n ≡ suc (m + n)
+-suc′ zero n = refl
+-suc′ (suc m) n rewrite +-suc′ m n = refl

+-comm′ : ∀ (m n : ℕ) → m + n ≡ n + m
+-comm′ m zero rewrite +-identityʳ m = refl
-- rewriting with two equations indicated by separating the two proofs
-- of the relevant equations by a vertical bar
-- left rewrite performed before right
+-comm′ m (suc n)  -- m + suc n ≡ suc  n + m
                   -- m + suc n ≡ suc (n + m) -- def/eq
  rewrite
    +-suc′  m n    -- suc (m + n) ≡ suc (n + m)
  | +-comm′ m n    -- suc (n + m) ≡ suc (n + m)
  = refl

+-comm′′ : ∀ (m n : ℕ) → m + n ≡ n + m
+-comm′′ m   zero rewrite +-identityʳ m = refl
+-comm′′ m (suc n) =
  begin
    m +  suc n  ≡⟨ +-suc′  m n ⟩
    suc (m + n) ≡⟨ cong suc (+-comm′′ m n) ⟩
    suc (n + m) ≡⟨⟩ -- def/eq
    suc  n + m
  ∎

{-
------------------------------------------------------------------------------
HC
-}

*0 : ∀ (m : ℕ) → m * 0 ≡ 0
*0 zero    = refl
*0 (suc m) = *0 m

0* : ∀ (m : ℕ) → 0 * m ≡ 0
0* m = refl

*1 : ∀ (n : ℕ) → n * 1 ≡ n
*1 zero = refl
*1 (suc n) rewrite *1 n = refl

1* : ∀ (n : ℕ) → 1 * n ≡ n
1* zero = refl
1* (suc n) rewrite 1* n = refl

{-
------------------------------------------------------------------------------
## Building proofs interactively

    +-assoc′ : ∀ (m n p : ℕ) → (m + n) + p ≡ m + (n + p)
    +-assoc′ m n p = ?

C-c C-l

    +-assoc′ : ∀ (m n p : ℕ) → (m + n) + p ≡ m + (n + p)
    +-assoc′ m n p = { }0

new window at the bottom:

    ?0 : ((m + n) + p) ≡ (m + (n + p))

indicates hole 0 needs to be filled with a proof of the stated judgment

to prove the proposition by induction on `m`
move cursor into hole
C-c C-c
prompt: pattern variables to case (empty for split on result):

type `m` to case split on that variable

    +-assoc′ : ∀ (m n p : ℕ) → (m + n) + p ≡ m + (n + p)
    +-assoc′ zero n p = { }0
    +-assoc′ (suc m) n p = { }1

There are now two holes, and the window at the bottom tells you what
each is required to prove:

    ?0 : ((zero + n) + p) ≡ (zero + (n + p))
    ?1 : ((suc m + n) + p) ≡ (suc m + (n + p))

goto hole 0
C-c C-,

    Goal: (n + p) ≡ (n + p)
    ————————————————————————————————————————————————————————————
    p : ℕ
    n : ℕ

indicates that after simplification the goal for hole 0 is as stated,
 and that variables `p` and `n` of the stated types are available to use in the proof.

the proof of the given goal is simple
goto goal
C-c C-r
fills in with refl
C-c C-l renumbers remaining hole to 0:

    +-assoc′ : ∀ (m n p : ℕ) → (m + n) + p ≡ m + (n + p)
    +-assoc′ zero n p = refl
    +-assoc′ (suc m) n p = { }0

goto hole 0
C-c C-,

    Goal: suc ((m + n) + p) ≡ suc (m + (n + p))
    ————————————————————————————————————————————————————————————
    p : ℕ
    n : ℕ
    m : ℕ

gives simplified goal and available variables

need to rewrite by the induction hypothesis

    +-assoc′ : ∀ (m n p : ℕ) → (m + n) + p ≡ m + (n + p)
    +-assoc′ zero n p = refl
    +-assoc′ (suc m) n p rewrite +-assoc′ m n p = { }0

goto hole
C-c C-,

    Goal: suc (m + (n + p)) ≡ suc (m + (n + p))
    ————————————————————————————————————————————————————————————
    p : ℕ
    n : ℕ
    m : ℕ

goto goal
C-c C-r
fills in, completing proof

    +-assoc′ : ∀ (m n p : ℕ) → (m + n) + p ≡ m + (n + p)
    +-assoc′ zero n p = refl
    +-assoc′ (suc m) n p rewrite +-assoc′ m n p = refl

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
     m + (n  + p) ≡⟨ sym (+-assoc m n p) ⟩
    (m +  n) + p  ≡⟨ cong (_+ p) (sym (+-comm n m)) ⟩
    (n +  m) + p  ≡⟨ +-assoc n m p ⟩
     n + (m  + p)
  ∎

{-
------------------------------------------------------------------------------
#### Exercise `*-distrib-+` (recommended) {name=times-distrib-plus} (m + n) * p ≡ m * p + n * p
-}

*-distrib-+r : ∀ (m n p : ℕ)
             → (m     + n) * p
             ≡  m * p + n  * p
*-distrib-+r  zero   y z = refl
*-distrib-+r (suc x) y z            -- (suc x     + y) * z  ≡ suc x * z + y * z
                                    -- z + (x     + y) * z  ≡ z + x * z + y * z
  rewrite
    *-distrib-+r x y z              -- z + (x * z + y  * z) ≡ z + x * z + y * z
  | sym (+-assoc z (x * z) (y * z)) -- z +  x * z + y  * z  ≡ z + x * z + y * z
  = refl

{- TODO: agda loops on this
*-distrib-+r' : ∀ (m n p : ℕ)
             → (m     + n) * p
             ≡  m * p + n  * p
*-distrib-+r'  zero   y z = refl
*-distrib-+r' (suc x) y z
  begin
    (suc x     + y) * z  ≡⟨⟩
    z + (x     + y) * z  ≡⟨ *-distrib-+r' x y z ⟩
    z + (x * z + y  * z) ≡⟨ sym (+-assoc z (x * z) (y * z)) ⟩
    z +  x * z + y  * z  ≡⟨⟩
     suc x * z + y  * z
  ∎
-}
{-
------------------------------------------------------------------------------
#### Exercise `*-assoc` (recommended) {name=times-assoc} (m * n) * p ≡ m * (n * p)
-}

*-assoc : ∀ (m n p : ℕ)
        → (m *  n) * p
        ≡  m * (n  * p)
-- base case : show: (zero *  n) * p
--                  ≡ zero * (n  * p)
*-assoc zero n p = refl      -- zero * n * p        ≡ zero * (n * p)
                             -- zero                ≡ zero -- def/eq
-- inductive case : show: (suc m *  n) * p
--                       ≡ suc m * (n  * p)
*-assoc (suc m) n p          --     suc m * n  * p  ≡     suc m * (n * p)
                             --    (n + m * n) * p  ≡ n * p + m * (n * p)
  rewrite
    *-distrib-+r n (m * n) p -- n * p + m *  n * p  ≡ n * p + m * (n * p)
  | *-assoc m n p            -- n * p + m * (n * p) ≡ n * p + m * (n * p)
  = refl

{-
------------------------------------------------------------------------------
#### Exercise `*-comm` (practice) {name=times-comm} MULTIPLICATION is COMMUTATIVE : m * n ≡ n * m
-}

*-suc : ∀ (x y : ℕ) → x * (suc y) ≡ x + (x * y)
*-suc zero y = refl
*-suc (suc x) y               --       suc x * suc y  ≡ suc  x +  suc  x * y
                              -- suc (y  + x * suc y) ≡ suc (x + (y +  x * y))
  rewrite
    +-comm y (x * suc y)      -- suc (x  * suc y + y) ≡ suc (x + (y +  x * y))
  | *-suc x y                 -- suc (x + x  * y + y) ≡ suc (x + (y +  x * y))
  | +-comm y (x * y)          -- suc (x + x  * y + y) ≡ suc (x + (x *  y + y))
  | sym (+-assoc x (x * y) y) -- suc (x + x  * y + y) ≡ suc (x +  x *  y + y)
  = refl

*-comm : ∀ (m n : ℕ) → m * n  ≡ n * m
*-comm m zero rewrite *0 m = refl
*-comm m (suc n) -- m * suc n ≡ suc n * m
                 -- m * suc n ≡ m + n * m
  rewrite
    *-suc m n    -- m + m * n ≡ m + n * m
  | *-comm m n   -- m + n * m ≡ m + n * m
  = refl

{-
------------------------------------------------------------------------------
#### Exercise `0∸n≡0` (practice) {name=zero-monus} : Show zero ∸ n ≡ zero

for all naturals `n`. Did your proof require induction?
-}

0∸n≡0 : ∀ (n : ℕ) → 0 ∸ n ≡ 0
0∸n≡0  zero   = refl
0∸n≡0 (suc n) = refl

{-
------------------------------------------------------------------------------
#### Exercise `∸-|-assoc` (practice) {name=monus-plus-assoc} : m ∸ n ∸ p ≡ m ∸ (n + p)

show that monus associates with addition
-}

∸-|-assoc : ∀ (m n p : ℕ) → m ∸ n ∸ p ≡ m ∸ (n + p)
∸-|-assoc      m       n   zero   --     m ∸     n ∸ zero   ≡     m ∸     (n + zero)
                                  --     m ∸     n          ≡     m ∸     (n + zero)
  rewrite +-identityʳ n = refl    --     m ∸     n          ≡     m ∸      n
∸-|-assoc      m   zero   (suc p) --     m ∸  zero ∸ suc p  ≡     m ∸  (zero + suc p)
  = refl                          --     m ∸         suc p  ≡     m ∸          suc p
∸-|-assoc  zero   (suc n) (suc p) --  zero ∸ suc n ∸ suc p  ≡  zero ∸ (suc n + suc p)
  = refl                          --  zero                  ≡  zero
∸-|-assoc (suc m) (suc n) (suc p) -- suc m ∸ suc n ∸ suc p  ≡ suc m ∸ (suc n + suc p)
                                  --     m ∸     n ∸ suc p  ≡     m ∸     (n + suc p)
  rewrite
    ∸-|-assoc m n (suc p)         --     m ∸    (n + suc p) ≡     m ∸     (n + suc p)
  = refl

{-
------------------------------------------------------------------------------
#### Exercise `+*^` (stretch)

     m ^ (n  + p) ≡ (m ^  n) * (m ^ p)  (^-distribˡ-|-*)
     (m * n) ^ p  ≡ (m ^  p) * (n ^ p)  (^-distribʳ-*)
     (m ^ n) ^ p  ≡  m ^ (n  *  p)      (^-*-assoc)
-}

-------------------------
-- this can be shortened
^-distribˡ-|-* : ∀ (m n p : ℕ)
               →  m ^ (n  +      p)
               ≡ (m ^  n) * (m ^ p)
^-distribˡ-|-* m n zero              -- (m ^ (n + zero))        ≡ (m ^ n) * (m ^ zero)
                                     -- (m ^ (n + zero))        ≡ (m ^ n) * 1
  rewrite
    +-identityʳ n                    -- (m ^  n)                ≡ (m ^ n) * 1
  | *1 (m ^ n)                       -- (m ^  n)                ≡ (m ^ n)
  = refl
^-distribˡ-|-* m n (suc p)           -- (m ^ (n + suc p))       ≡ (m ^ n) *      (m ^ suc p)
                                     -- (m ^ (n + suc p))       ≡ (m ^ n) * (m * (m ^     p))
  rewrite
    *-comm m (m ^ p)                 -- (m ^ (n + suc p))       ≡ (m ^ n) *     ((m ^     p) * m)
  | sym (*-assoc (m ^ n) (m ^ p) m)  -- (m ^ (n + suc p))       ≡ (m ^ n) *      (m ^     p) * m
  | +-comm n (suc p)                 -- (m ^ suc (p + n))       ≡ (m ^ n) *      (m ^     p) * m
                                     -- m * (m ^ (p + n))       ≡ (m ^ n) *      (m ^     p) * m
  | *-comm ((m ^ n) * (m ^     p)) m -- m * (m ^ (p + n))       ≡  m * ((m ^ n) * (m ^ p))
  | sym (*-assoc m (m ^ n) (m ^ p))  -- m * (m ^ (p + n))       ≡  m *  (m ^ n) * (m ^ p)
  | ^-distribˡ-|-* m p n             -- m * ((m ^ p) * (m ^ n)) ≡  m *  (m ^ n) * (m ^ p)
  | sym (*-comm (m ^ n) (m ^ p))     -- m * ((m ^ n) * (m ^ p)) ≡  m *  (m ^ n) * (m ^ p)
  | *-assoc m (m ^ n) (m ^ p)        -- m * ((m ^ n) * (m ^ p)) ≡  m * ((m ^ n) * (m ^ p))
  = refl

-------------------------
^-distribʳ-* : ∀ (m n p : ℕ)
             → (m * n) ^ p
             ≡ (m ^ p) * (n ^ p)
^-distribʳ-* m n zero = refl
^-distribʳ-* m n (suc p)            --      ((m * n) ^ suc p)      ≡(m ^ suc p) * (n ^ suc p)
                                    -- m * n * ((m      *  n) ^ p) ≡ m * (m ^ p) * (n * (n ^ p))
  rewrite
    ^-distribʳ-* m n p              -- m * n * ((m ^ p) * (n ^ p)) ≡ m * (m ^ p) * (n * (n ^ p))
  | *-comm (m * (m ^ p)) (n * (n ^ p))
                                    -- m * n * ((m ^ p) * (n ^ p)) ≡ n * (n ^ p) * (m * (m ^ p))
  | *-assoc m n ((m ^ p) * (n ^ p)) -- m *(n * ((m ^ p) * (n ^ p)))≡ n * (n ^ p) * (m * (m ^ p))
  | *-comm m (n * ((m ^ p) * (n ^ p)))
                                    -- n *((m ^ p) * (n ^ p))* m  ≡ n * (n ^ p) * (m * (m ^ p))
  | *-comm (m ^ p) (n ^ p)          -- n *((n ^ p) * (m ^ p))* m  ≡ n * (n ^ p) * (m * (m ^ p))
  | sym (*-assoc n (n ^ p) (m ^ p)) -- n * (n ^ p) * (m ^ p) * m  ≡ n * (n ^ p) * (m * (m ^ p))
  | *-assoc n (n ^ p) (m * (m ^ p)) -- n * (n ^ p) * (m ^ p) * m  ≡ n *((n ^ p) * (m * (m ^ p)))
  | *-comm m (m ^ p)                -- n * (n ^ p) * (m ^ p) * m  ≡ n *((n ^ p) * ((m ^ p) * m))
  | *-assoc n (n ^ p) (m ^ p)       -- n *((n ^ p) * (m ^ p))* m  ≡ n *((n ^ p) * ((m ^ p) * m))
  | *-assoc n ((n ^ p) * (m ^ p)) m -- n *((n ^ p) * (m ^ p) * m) ≡ n *((n ^ p) * ((m ^ p) * m))
  | *-assoc (n ^ p) (m ^ p) m       -- n *((n ^ p) *((m ^ p) * m))≡ n *((n ^ p) * ((m ^ p) * m))
  = refl

-------------------------
^-*-assoc : ∀ (m n p : ℕ)
          → (m ^  n) ^ p
          ≡  m ^ (n  * p)
^-*-assoc m n  zero                 --        ((m ^ n) ^ zero) ≡ (m ^ (n * zero))
                                    --                      1  ≡ (m ^ (n * zero))
  rewrite *0 n                      --                      1  ≡ (m ^ 0)
                                    --                      1  ≡ 1
  = refl
^-*-assoc m n (suc p)               --       ((m ^ n) ^ suc p) ≡ (m ^ (n * suc p))
                                    -- (m ^ n) * ((m ^ n) ^ p) ≡ (m ^ (n * suc p))
  rewrite
    *-suc n p                       -- (m ^ n) * ((m ^ n) ^ p) ≡ (m ^ (n + n * p))
  | ^-distribˡ-|-* m n (n * p)      -- (m ^ n) * ((m ^ n) ^ p) ≡ (m ^ n) * (m ^ (n * p))
  | sym (^-*-assoc m n p)           -- (m ^ n) * ((m ^ n) ^ p) ≡ (m ^ n) * ((m ^ n) ^ p)
  = refl

{-
------------------------------------------------------------------------------
#### Exercise `Bin-laws` (stretch) {name=Bin-laws}

Recall that
Exercise [Bin](/Naturals/#Bin)
defines a datatype `Bin` of bitstrings representing natural numbers,
and asks you to define functions
-}

-- begin duplicated from x01 (can't import because of "duplicate" pragma)

data Bin : Set where
  ⟨⟩ : Bin
  _O : Bin → Bin
  _I : Bin → Bin

inc : Bin → Bin
inc  ⟨⟩    = ⟨⟩      I
inc (b  O) =      b  I
inc (b  I) = (inc b) O

_ : inc (⟨⟩ I O I I) ≡ ⟨⟩ I I O O
_ = refl

-- end duplicated

dbl : ℕ → ℕ
dbl   zero  = zero
dbl (suc m) = suc (suc (dbl m))

to : ℕ → Bin
to   zero  = ⟨⟩ O
to (suc m) = inc (to m)

-- THIS IS THE CRITICAL STEP : defining in terms of 'dbl'
-- Got hint from : https://cs.uwaterloo.ca/~plragde/842/
from : Bin → ℕ
from     ⟨⟩ = 0
from (b  O) =      dbl (from b)
from (b  I) = suc (dbl (from b))

_ : to 6 ≡ ⟨⟩ I I O
_ = refl

_ : from (⟨⟩ I I O) ≡ 6
_ = refl

{-
Consider the following laws, where `n` ranges over naturals and `b`
over bitstrings:

    from (inc b) ≡ suc (from b)
    to (from b) ≡ b
    from (to n) ≡ n

For each law: if it holds, prove; if not, give a counterexample.
-}

-------------------------

+1 : ∀ (x : ℕ) → x + 1 ≡ suc x
+1 zero = refl
+1 (suc n) rewrite +1 n = refl

fromInc≡sucFrom : ∀ (b : Bin)
                → from (inc b) ≡ suc (from b)
fromInc≡sucFrom ⟨⟩       --       from (inc ⟨⟩)      ≡ suc (from ⟨⟩)
                         --                        1 ≡ 1
  = refl
fromInc≡sucFrom (⟨⟩ I)   --       from (inc (⟨⟩ I))  ≡ suc (from (⟨⟩ I))
                         --                        2 ≡ 2
  = refl
fromInc≡sucFrom (b O)    --       from (inc (b O))   ≡ suc (from (b O))
                         --       dbl (from b) + 1   ≡ suc (dbl (from b))
  rewrite
    +1 (dbl (from b))    --       suc (dbl (from b)) ≡ suc (dbl (from b))
  = refl
fromInc≡sucFrom (b I)    --       from (inc (b I))   ≡ suc (from (b I))
                         --       dbl (from (inc b)) ≡ suc (dbl (from b) + 1)
  rewrite
    +1 (dbl (from b))    --       dbl (from (inc b)) ≡ suc (suc (dbl (from b)))
  | fromInc≡sucFrom b    --       dbl (suc (from b)) ≡ suc (suc (dbl (from b)))
                         -- suc (suc (dbl (from b))) ≡ suc (suc (dbl (from b)))
  = refl

-------------------------

-- NOT USED
xx : ∀ (b : Bin)
   → (dbl (from b)) ≡ from (b O)
xx ⟨⟩        -- dbl (from ⟨⟩) ≡ from (⟨⟩ O)
             -- zero ≡ zero
  = refl
xx (b O)     -- dbl (from (b O)) ≡ from ((b O) O)
             -- dbl (dbl (from b)) ≡ dbl (dbl (from b))
  = refl
xx (b I)     -- dbl (from (b I)) ≡ from ((b I) O)
             -- suc (suc (dbl (dbl (from b)))) ≡ suc (suc (dbl (dbl (from b))))
  = refl

-- TODO
yy : ∀ (b : Bin)
   → to (dbl (from b)) ≡ (b O)
yy ⟨⟩        --                  to (dbl (from ⟨⟩)) ≡ (⟨⟩ O)
             --                              (⟨⟩ O) ≡ (⟨⟩ O)
  = refl
yy (b O)     --           to (dbl (from (b O)))     ≡ ((b O) O)
             --           to (dbl (dbl (from b)))   ≡ ((b O) O)
  rewrite
    yy b
  = {!!}
yy (b I)     --           to (dbl (from (b I)))     ≡ ((b I) O)
             -- inc (inc (to (dbl (dbl (from b))))) ≡ ((b I) O)
  = {!!}

to-from : ∀ (b : Bin)
       → to (from b) ≡ b
to-from ⟨⟩       --       to (from ⟨⟩)      ≡ ⟨⟩
                 --               (⟨⟩ O)    ≡ ⟨⟩ -- *****
  = {!!}
to-from (⟨⟩ I)   --       to (from (⟨⟩ I))  ≡ (⟨⟩ I)
                 --       (⟨⟩ I)            ≡ (⟨⟩ I)
  = refl
to-from (b O)    --       to (from (b O))   ≡ (b O)
                 --       to (dbl (from b)) ≡ (b O)
  rewrite
    yy b         --                   (b O) ≡ (b O)
  = refl
to-from (b I)    --         to (from (b I)) ≡ (b I)
                 -- inc (to (dbl (from b))) ≡ (b I)
  rewrite
    yy b         --                   (b I) ≡ (b I)
  = refl

-------------------------

-- https://github.com/billyang98/plfa/blob/master/plfa/Induction.agda

from-inc≡suc-from : ∀ x → from (inc x) ≡ suc (from x)
from-inc≡suc-from ⟨⟩    = refl
from-inc≡suc-from (x O) = refl
from-inc≡suc-from (x I)  --         from (inc (x I)) ≡ suc           (from (x I))
                         --    dbl (from (inc  x))   ≡ suc (suc (dbl (from  x)))
  rewrite
    from-inc≡suc-from x  -- suc (suc (dbl (from x))) ≡ suc (suc (dbl (from x)))
  = refl

from-to : ∀ n → from (to n) ≡ n
from-to zero = refl
from-to (suc n)              -- from (to (suc n)) ≡ suc n
                             -- from (inc (to n)) ≡ suc n
  rewrite
    from-inc≡suc-from (to n) -- suc (from (to n)) ≡ suc n
  | cong suc (from-to n)     -- suc           n   ≡ suc n
  = refl

{-
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


-- ============================================================================

-- this is here to ensure the Bin/in/from/to does not get broken if changed above

_ : inc (⟨⟩ I O I I) ≡ ⟨⟩ I I O O
_ = refl

_ : inc (⟨⟩     O) ≡ ⟨⟩     I
_ = refl
_ : inc (⟨⟩     I) ≡ ⟨⟩   I O
_ = refl
_ : inc (⟨⟩   I O) ≡ ⟨⟩   I I
_ = refl
_ : inc (⟨⟩   I I) ≡ ⟨⟩ I O O
_ = refl
_ : inc (⟨⟩ I O O) ≡ ⟨⟩ I O I
_ = refl

_ : from (⟨⟩     O) ≡ 0
_ = refl
_ : from (⟨⟩     I) ≡ 1
_ = refl
_ : from (⟨⟩   I O) ≡ 2
_ = refl
_ : from (⟨⟩   I I) ≡ 3
_ = refl
_ : from (⟨⟩ I O O) ≡ 4
_ = refl

_ : to 0 ≡ (⟨⟩     O)
_ = refl
_ : to 1 ≡ (⟨⟩     I)
_ = refl
_ : to 2 ≡ (⟨⟩   I O)
_ = refl
_ : to 3 ≡ (⟨⟩   I I)
_ = refl
_ : to 4 ≡ (⟨⟩ I O O)
_ = refl

_ : from (to 12) ≡ 12
_ = refl

_ : to (from (⟨⟩ I I O O)) ≡ ⟨⟩ I I O O
_ = refl
