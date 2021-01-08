module x01naturals where

{-
------------------------------------------------------------------------------
naturals : inductive datatype

definition as a pair of inference rules:

    ---------
    zero  : ℕ

    m     : ℕ    -- assuming m is Natural
    ---------
    suc m : ℕ    -- then suc m is also a Natural

Agda definition in Agda:
-}

data ℕ : Set where
  zero :     ℕ -- base case
  suc  : ℕ → ℕ -- inductive case

-- #### Exercise Write out `7` in longhand.

seven : ℕ
seven = suc (suc (suc (suc (suc (suc (suc zero))))))

{-
inference rules consists of
- zero or more _judgments_ written above a horizontal line, called the _hypotheses_
- single judgment written below, called the _conclusion_


`Set` : the way in Agda of saying that it is a type


Inductive case defines natural numbers in terms of natural numbers.
Note the crucial role of the base case to jump start the process of induction.

## Philosophy and history

The inductive definition of the natural numbers is relatively recent.
- 1888
  Richard Dedekind's paper "_Was sind und was sollen die Zahlen?_"
  (What are and what should be the numbers?)
- 1889
  Giuseppe Peano's book "_Arithmetices principia, nova methodo exposita_"
  (The principles of arithmetic presented by a new method)

------------------------------------------------------------------------------
## PRAGMA

Including the line
-}
{-# BUILTIN NATURAL ℕ #-}
{-
tells Agda that `ℕ` corresponds to the natural numbers
- pragma must be given a previously declared type (in this case `ℕ`)
- enables 0, 1, 2, ... shorthand
- enables more efficient internal Haskell representation

## IMPORTS

To write equations that hold between terms involving natural numbers
import the definition of equality and notations for reasoning about equality
from the Agda standard library:
-}

import Relation.Binary.PropositionalEquality as Eq
open Eq             using (_≡_; refl)
open Eq.≡-Reasoning using (begin_; _≡⟨⟩_; _∎)

{-
1st line : brings equality module into scope and gives it the name `Eq`.
2nd line : opens that module and adds names in `using` clause into scope.
3rd line : opens module that specifies operators to support reasoning about equivalence
           and adds names in `using` clause into scope.

Take these as givens for now, but will see how they are defined in Chapter Equality

underbars indicate where terms appear
- `_≡_` and `_≡⟨⟩_`  : terms on each side
- `begin_` is prefix : terms after
- : `_∎`             : terms before

------------------------------------------------------------------------------
## Operations on naturals are recursive functions
-}

_+_ : ℕ → ℕ → ℕ
zero    + n =          n  -- base case      :  0      + n  ≡  n
(suc m) + n = suc (m + n) -- inductive case : (1 + m) + n  ≡  1 + (m + n) : associativity

{-
Inductive definition works because addition of larger numbers is defined
in terms of addition of smaller numbers : _well founded_
-}

_ : 2 + 3 ≡ 5
_ =
  begin
    2 + 3
  ≡⟨⟩    -- is shorthand for
    (suc (suc zero)) + (suc (suc (suc zero)))
  ≡⟨⟩    -- inductive case
    suc ((suc zero)  + (suc (suc (suc zero))))
  ≡⟨⟩    -- inductive case
    suc (suc (zero   + (suc (suc (suc zero)))))
  ≡⟨⟩    -- base case
    suc (suc           (suc (suc (suc zero))))
  ≡⟨⟩    -- is longhand for
    5
  ∎ -- ∎ is QED

-- equivalent

_ : 2 + 3 ≡ 5
_ =
  begin
    2 + 3
  ≡⟨⟩
    suc      (1 + 3)
  ≡⟨⟩
    suc (suc (0 + 3))
  ≡⟨⟩
    suc (suc      3)
  ≡⟨⟩
    5
  ∎

-- A binary relation is said to be _reflexive_ if every value relates to itself.
-- Evidence that a value is equal to itself is written `refl`

_ : 2 + 3 ≡ 5
_ = refl

{-
above derivations consist of
- a signature (written with a colon, `:`), giving a type
- a binding (written with an equal sign, `=`), giving a term of the given type.

the dummy name `_` can be reused
consists of a series of terms separated by `≡⟨⟩`.

Duality of interpretation
- type as a proposition
- term as evidence (aka proof)

#### Exercise `+-example` Compute `3 + 4`
-}

_ : 3 + 4 ≡ 7
_ =
  begin
    3 + 4
  ≡⟨⟩
    suc           (2 + 4)
  ≡⟨⟩
    suc (suc      (1 + 4))
  ≡⟨⟩
    suc (suc (suc (0 + 4)))
  ≡⟨⟩
    suc (suc (suc      4))
  ≡⟨⟩
    7
  ∎

-- A binary relation is said to be _reflexive_ if every value relates to itself.
-- Evidence that a value is equal to itself is written `refl`

_ : 3 + 4 ≡ 7
_ = refl

{-
------------------------------------------------------------------------------
## Multiplication
-}

_*_ : ℕ → ℕ → ℕ
zero    * n  =  zero        --  0      * n  ≡  0
(suc m) * n  =  n + (m * n) -- (1 + m) * n  ≡  n + (m * n)

{-
Computing `m * n` returns the sum of `m` copies of `n`.

2nd line because multiplication distributes over addition:

    (m + n) * p  ≡  (m * p) + (n * p)

We get the second equation from the third by
- taking `m` to be `1`
- `n` to be `m`
- `p` to be `n`
- then use fact that one is identity for multiplication, so
- `1 * n ≡ n`

well founded : _*_ of larger numbers is defined in terms of _*_ of smaller numbers
-}

_ : 2 * 3 ≡ 6
_ =
  begin
    2 * 3
  ≡⟨⟩    -- inductive case
    3 + (1 * 3)
  ≡⟨⟩    -- inductive case
    3 + (3 + (0 * 3))
  ≡⟨⟩    -- base case
    3 + (3 + 0)
  ≡⟨⟩    -- simplify
    6
  ∎

{-
#### Exercise `*-example` Compute `3 * 4`
-}

_ : 3 * 4 ≡ 12
_ =
  begin
    3 * 4
  ≡⟨⟩
    4 + (2 * 4)
  ≡⟨⟩
    4 + (4 + (1 * 4))
  ≡⟨⟩
    4 + (4 + (4 + (0 * 4)))
  ≡⟨⟩   -- base case
    4 + (4 + (4 +  0))
  ≡⟨⟩   -- addition
    12
  ∎

-- HC

_*hc_ : ℕ → ℕ → ℕ
n *hc zero    =  zero
n *hc (suc m) =  (m * n) + n

_ : 2 *hc 3 ≡ 6
_ =
  begin
    2  *hc 3
  ≡⟨⟩
    (2 *hc 2)              + 2
  ≡⟨⟩
    ((1 *hc 2)         + 2) + 2
  ≡⟨⟩
    (((0 *hc 2)  + 2)  + 2) + 2
  ≡⟨⟩
     ((0         + 2)  + 2) + 2
  ≡⟨⟩
     (             2   + 2) + 2
  ≡⟨⟩
    6
  ∎

{-
------------------------------------------------------------------------------
#### Exercise `_^_` (recommended) Define exponentiation
- given by the following equations:

    m ^ 0        =  1
    m ^ (1 + n)  =  m * (m ^ n)

Check that `3 ^ 4` is `81`.
-}

_^_ : ℕ → ℕ → ℕ
m ^ 0       = 1
m ^ (suc n) = m * (m ^ n)

_ : 2 ^ 3 ≡ 8
_ = refl

_ : 3 ^ 4 ≡ 81
_ = refl

{-
------------------------------------------------------------------------------
## Monus subtraction

uses pattern matching against both arguments:
-}

_∸_ : ℕ → ℕ → ℕ
m     ∸ zero   =  m
zero  ∸ suc n  =  zero
suc m ∸ suc n  =  m ∸ n

{-
well founded : monus on bigger numbers is defined in terms of monus on smaller numbers.
-}

_ =
  begin
    3 ∸ 2
  ≡⟨⟩
    2 ∸ 1
  ≡⟨⟩
    1 ∸ 0
  ≡⟨⟩
    1
  ∎

_ =
  begin
    2 ∸ 3
  ≡⟨⟩
    1 ∸ 2
  ≡⟨⟩
    0 ∸ 1
  ≡⟨⟩
    0
  ∎

{-
#### Exercise Compute `5 ∸ 3` and `3 ∸ 5`, reasoning as a chain of equations.
-}

_ =
  begin
    5 ∸ 3
  ≡⟨⟩
    4 ∸ 2
  ≡⟨⟩
    3 ∸ 1
  ≡⟨⟩
    2 ∸ 0
  ≡⟨⟩
    2
  ∎

_ =
  begin
    3 ∸ 5
  ≡⟨⟩
    2 ∸ 4
  ≡⟨⟩
    1 ∸ 3
  ≡⟨⟩
    0 ∸ 2
  ≡⟨⟩
    0
  ∎

{-
------------------------------------------------------------------------------
## Precedence to avoid writing parentheses

Application higher than operators   : `suc m + n` means `(suc m) + n`

multiplication higher than addition : `n + m * n` means `n + (m * n)`

addition _associates to the left_   : `m + n + p` means `(m + n) + p`

declare precedence and associativity of infix operators
-}

infixl 6  _+_  _∸_
infixl 7  _*_

{-
`infixl` : associate to the left
`infixr` : associate to the right
`infix`  : indicates that parentheses are required to disambiguate

## Currying

a function of two arguments in terms of
- a function of the first argument
- that returns a function of the second argument

Function arrows associate to the right : `ℕ → ℕ → ℕ` stands for `ℕ → (ℕ → ℕ)`
Application associates to the left     : `_+_ 2 3` stands for `(_+_ 2) 3`

Named for Haskell Curry.

The idea actually appears in the _Begriffsschrift_ of Gottlob Frege, published in 1879.

## The story of creation, revisited

inductive definition defines naturals in terms of naturals
recursive definition defines addition in terms of addition

           n : ℕ
    --------------
    zero + n  =  n

         m  + n  =      p
    ---------------------
    (suc m) + n  =  suc p

## The story of creation, finitely {name=finite-creation}
skipped

------------------------------------------------------------------------------
## More pragmas
-}

{-# BUILTIN NATPLUS  _+_ #-}
{-# BUILTIN NATTIMES _*_ #-}
{-# BUILTIN NATMINUS _∸_ #-}

{-
tells Agda correspondance between operators correspond and the usual ones.
Enables using corresponding Haskell operators on arbitrary-precision integer type.

------------------------------------------------------------------------------
#### Exercise `Bin` (stretch) {name=Bin}

represent nat as bitstring
-}

data Bin : Set where
  ⟨⟩ : Bin
  _O : Bin → Bin
  _I : Bin → Bin

{-
bitstring 1011 (eleven) encoded as ⟨⟩ I O I I
or, with leading zeros,        ⟨⟩ O O I O I I

define

    inc : Bin → Bin

converts bitstring to bitstring for next higher number
-}

inc : Bin → Bin
inc  ⟨⟩    = ⟨⟩      I
inc (⟨⟩ O) = ⟨⟩      I
inc (⟨⟩ I) = ⟨⟩   I  O
inc (b  O) =      b  I
inc (b  I) = (inc b) O

-- since `1100` encodes twelve, we should have:

_ : inc (⟨⟩ I O I I) ≡ ⟨⟩ I I O O
_ = refl

-- Confirm correct answer for the bitstrings encoding zero through four.

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

{-
using above, define

    to   : ℕ → Bin
    from : Bin → ℕ

`to` should not have leading zeros, except represent zero as `⟨⟩ O`

confirm for zero through four
-}

to : ℕ → Bin
to zero    = ⟨⟩ O
to (suc m) = inc (to m)

from : Bin → ℕ
from     ⟨⟩ = 0
from (b  O) = 2 * from b
from (b  I) = 2 * from b + 1

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
_ : from (⟨⟩ I I O) ≡ 6
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
_ : to 6 ≡ (⟨⟩ I I O)
_ = refl

_ : from (to 12) ≡ 12
_ = refl

_ : to (from (⟨⟩ I I O O)) ≡ ⟨⟩ I I O O
_ = refl

-- 842 exercise

_bin-+_ : Bin → Bin → Bin
⟨⟩     bin-+ ⟨⟩     = ⟨⟩
⟨⟩     bin-+ b      = b
b      bin-+ ⟨⟩     = b
(bl O) bin-+ (br O) = let r = bl bin-+ br in      r  O
(bl O) bin-+ (br I) = let r = bl bin-+ br in      r  I
(bl I) bin-+ (br O) = let r = bl bin-+ br in      r  I
(bl I) bin-+ (br I) = let r = bl bin-+ br in (inc r) O

_ : (⟨⟩)       bin-+ (⟨⟩)     ≡ (⟨⟩)
_ = refl
_ : (⟨⟩)       bin-+ (⟨⟩   O) ≡ (⟨⟩       O)
_ = refl
_ : (⟨⟩     O) bin-+ (⟨⟩)     ≡ (⟨⟩       O)
_ = refl
_ : (⟨⟩     O) bin-+ (⟨⟩   O) ≡ (⟨⟩       O)
_ = refl
_ : (⟨⟩     O) bin-+ (⟨⟩   I) ≡ (⟨⟩       I)
_ = refl
_ : (⟨⟩     I) bin-+ (⟨⟩   I) ≡ (⟨⟩     I O)
_ = refl
_ : (⟨⟩   I O) bin-+ (⟨⟩ I O) ≡ (⟨⟩   I O O)
_ = refl
_ : (⟨⟩   I I) bin-+ (⟨⟩ I I) ≡ (⟨⟩   I I O)
_ = refl
_ : (⟨⟩ I O I) bin-+ (⟨⟩   I) ≡ (⟨⟩   I I O)
_ = refl
_ : (⟨⟩ I I I) bin-+ (⟨⟩   I) ≡ (⟨⟩ I O O O)
_ = refl

------------------------------------------------------------------------------
-- hc exercise

z1-bin-+ : ∀ (b1 b2 : Bin)
        → b1          ≡ ⟨⟩
        → b1 bin-+ b2 ≡ b2
z1-bin-+ b1 ⟨⟩ p     -- (b1 bin-+  ⟨⟩)    ≡ ⟨⟩
  rewrite
    p                -- (⟨⟩ bin-+  ⟨⟩)    ≡ ⟨⟩
                     --            ⟨⟩     ≡ ⟨⟩
  = refl
z1-bin-+ b1 (⟨⟩ O) p -- (b1 bin-+ (⟨⟩ O)) ≡ (⟨⟩ O)
  rewrite
    p                -- (⟨⟩ bin-+ (⟨⟩ O)) ≡ (⟨⟩ O)
                     --           (⟨⟩ O)  ≡ (⟨⟩ O)
  = refl
z1-bin-+ b1 (b O) p  -- (b1 bin-+ (b  O)) ≡ (b  O)
  rewrite
    p                -- (⟨⟩ bin-+  (b O)) ≡ (b  O)
                     --            (b O)  ≡ (b  O)
  = refl
z1-bin-+ b1 (b I) p  -- (b1 bin-+  (b I)) ≡ (b  I)
  rewrite
    p                -- (⟨⟩ bin-+  (b I)) ≡ (b  I)
                     --            (b I)  ≡ (b  I)
  = refl

z-bin-+ : ∀ (b1 b2 : Bin)
        → b1          ≡ (⟨⟩ O)
        → b1 bin-+ b2 ≡ b2
z-bin-+ b1 ⟨⟩ p     -- (b1     bin-+ ⟨⟩)     ≡ ⟨⟩
  rewrite
    p               -- ((⟨⟩ O) bin-+ ⟨⟩)     ≡ ⟨⟩
                    --  (⟨⟩ O)               ≡ ⟨⟩
  = {!!}
z-bin-+ b1 (⟨⟩ O) p -- (b1     bin-+ (⟨⟩ O)) ≡ (⟨⟩ O)
  rewrite
    p               -- ((⟨⟩ O) bin-+ (⟨⟩ O)) ≡ (⟨⟩ O)
                    --               (⟨⟩ O)  ≡ (⟨⟩ O)
  = refl
z-bin-+ b1 (b O) p  -- (b1     bin-+ (b  O)) ≡ (b  O)
  rewrite
    p               -- ((⟨⟩ O) bin-+ (b  O)) ≡ (b  O)
                    -- ((⟨⟩    bin-+ b)  O)  ≡ (b  O)
  = {!!}
z-bin-+ b1 (b I) p  -- (b1     bin-+ (b  I)) ≡ (b  I)
  rewrite
    p               -- ((⟨⟩ O) bin-+ (b  I)) ≡ (b  I)
                    -- ((⟨⟩    bin-+ b)  I)  ≡ (b  I)
  = {!!}

hc : ∀ (m n : ℕ) →  from (to m bin-+ to n) ≡ m + n
hc  zero   n     -- from (to zero bin-+ to n) ≡ zero + n
  = {!!}         -- from ( (⟨⟩ O) bin-+ to n) ≡        n
hc (suc m) n = {!!}

{-
------------------------------------------------------------------------------
## Standard library

where to find relevant definitions in the standard library

Naturals, constructors, operators :

import Data.Nat using (ℕ; zero; suc; _+_; _*_; _^_; _∸_)

https://agda.readthedocs.io/en/v2.6.1/language/pragmas.html

------------------------------------------------------------------------------
## Unicode

This chapter uses the following unicode:

 char  code    name                             emacs
------------------------------------------------------
    ℕ  U+2115  DOUBLE-STRUCK CAPITAL N          (\bN)
    →  U+2192  RIGHTWARDS ARROW                 (\to, \r, \->)
    ∸  U+2238  DOT MINUS                        (\.-)
    ≡  U+2261  IDENTICAL TO                     (\==)
    ⟨  U+27E8  MATHEMATICAL LEFT ANGLE BRACKET  (\<)
    ⟩  U+27E9  MATHEMATICAL RIGHT ANGLE BRACKET (\>)
    ∎  U+220E  END OF PROOF                     (\qed)


`\r` : variety of right arrows

`\l` : variety of left arrows

All the characters supported by `agda-mode`:

    M-x agda-input-show-translations

To see how to input an existing specific Unicode character in an agda file,
move cursor to character

    M-x quail-show-key
-}



