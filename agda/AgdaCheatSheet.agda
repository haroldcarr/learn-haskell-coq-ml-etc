module AgdaCheatSheet where

open import Level using (Level)
open import Data.Nat
open import Data.Bool hiding (_<?_)
open import Data.List using (List; []; _∷_; length)

-- https://alhassy.github.io/AgdaCheatSheet/CheatSheet.pdf

{-
------------------------------------------------------------------------------
-- dependent function

A function whose RESULT type depends on the VALUE of the argument.
  -- given  value  of type A
  -- return result of type B a
  (a : A) → B a

E.g., generic identity
-- input
--  the type of X
--  value of type X
-- output a function X → X
-}
id0 : (X : Set) → X → X
id0 X x = x

id1 id2 id3 : (X : Set) → X → X
id1 X = λ x → x

id2 = λ X x → x
id3 = λ (X : Set) (x : X) → x

-- call it
sad : ℕ
sad = id0 ℕ 3

{-
------------------------------------------------------------------------------
-- implicits

In above, type arg can be inferred.
Use curly braces to mark arg as implicit.
-}

id : {X : Set} → X → X
id x = x

-- call it
nice : ℕ
nice = id 3

-- call with explicit implicit
explicit : ℕ
explicit = id {ℕ} 3

-- call with explicit inferred implicit
explicit’ : ℕ
explicit’ = id0 _ 3

{-
------------------------------------------------------------------------------
-- specifying arguments

{x : _} {y : _} (z : _) → · · ·        ≈ ∀ {x y} z → · · ·


(a1 : A) → (a2 : A) → (b : B) → · · ·  ≈ (a1 a2 : A) (b : B) → · · ·

------------------------------------------------------------------------------
-- dependent datatypes

-- data
-- name
-- arguments
-- type of the datatype
-- constructors and their types
-}

data Vec {ℓ : Level} (A : Set ℓ) : ℕ → Set ℓ where
  []   :                         Vec A  0
  _::_ : {n : ℕ} → A → Vec A n → Vec A (1 + n)

{-
For a given A, type of Vec A is N → Set.
Means Vec A is a FAMILY of types indexed by natural numbers:
- For each n, there is a TYPE Vec A n.

Vec is
- parametrised by A (and ℓ)
- indexed by n

A is type of elements
n is length

only way to Vec A 0 is via []
only way to Vec A (1 + n) is via _::_

enables safe head (since empty impossible)
-}

head : {A : Set} {n : ℕ} → Vec A (1 + n) → A
head (x :: xs) = x

{-
------------------------------------------------------------------------------
-- universes

ℓ arg says Vec is universe polymorphic
- can vectors of numbers and also vectors of types

Levels are essentially natural numbers:
- constructurs
  - lzero
  - lsuc
- _t_ : max of two levels

NO UNIVERSE OF ALL UNIVERSES:
- Set n has type Set n+1 for any n
- the type (n : Level) → Set n
  - is not itself typeable
  — i.e., is not in Set ℓ for any ℓ
  — Agda errors saying it is a value of Set ω

------------------------------------------------------------------------------
-- functions defined by pattern matching ALL cases

must terminate : so recursive calls must be made on structurally smaller arg
-}

infixr 40 _++_

_++_ : {A : Set} {n m : ℕ} → Vec A n → Vec A m → Vec A (n + m)
[]        ++ ys =             ys
(x :: xs) ++ ys = x :: (xs ++ ys)
{-
Append type encodes property : length of catenation is sum of args lengths

- Different types can have the same constructor names.
- Mixifx operators can be written prefix by having all underscores mentioned; e.g.,
- In def, if an arg is not needed, use _ (wildcard pattern)

------------------------------------------------------------------------------
-- Curry-Howard Correspondence — Propositions as Types

Logic                     Programming                 Example Use in Programming
================================================================================
proof / proposition       element / type              “p is a proof of P” ≈ “p is of type P”

true                      singleton type              return type of side-effect only methods
false                     empty type                  return type for non-terminating methods

⇒                         function type           →   methods with an input and output type
∧                         product type            ×   simple records of data and methods
∨                         sum type                +   enumerations or tagged unions

∀                         dependent function type Π   return type varies according to input value
∃                         dependent product  type Σ   record fields depend on each other’s values

natural deduction         type system                 ensuring only “meaningful” programs
hypothesis                free variable               global variables, closures

modus ponens              function application        executing methods on arguments
⇒ -introduction           λ-abstraction               parameters acting as local variables
                                                    to method definitions

induction;
elimination rules         Structural recursion        for-loops are precisely N-induction

Signature, term           Syntax; interface, record type, class
Algebra, Interpretation   Semantics; implementation, instance, object
Free Theory               Data structure
Inference rule            Algebraic datatype constructor
Monoid                    Untyped programming / composition
Category                  Typed programming / composition

------------------------------------------------------------------------------
-- equality

example of propositions-as-types : definition of identity relation (the least reflexive relation)
-}
data _≡_ {A : Set} : A → A → Set where
  refl : {x : A} → x ≡ x
  {-

states that refl {x}
- is a proof of l ≡ r
- whenever l and r simplify to x
  - by definition chasing only

Use it to prove Leibniz’s substitutivity rule, “equals for equals”:
-}
subst : {A : Set} {P : A → Set} {l r : A}
      → l ≡ r  -- must be of the form refl {x} for some canonical form x
      → P l    -- if l and r are both x, then P l and P r are the same type
      → P r    -- matching on proof l ≡ r gave infor about the rest of the program’s type
subst refl it = it

------------------------------------------------------------------------------
-- modules - namespace management

--------------------------------------------------
-- SIMPLE MODULES
module M where
  N : Set
  N = ℕ
  private
    x : ℕ
    x = 3
  y : N
  y = x + 1

-- using it - public names accessible by qualification or by opening them locally or globally
use0 : M.N
use0 = M.y

use1 : ℕ
use1 = y where open M
{- if open, then causes y in M' to duplicate
open M
use2 : ℕ
use2 = y
-}

--------------------------------------------------
-- PARAMETERISED MODULES : by arbitrarily many values and types (but not by other modules)
module M’ (x : ℕ) where
  y : ℕ
  y = x + 1

  -- names are functions
  --exposed : (x : ℕ) → ℕ
  --exposed = M’.y -- TODO compile error

-- using it
use’0 : ℕ
use’0 = M’.y 3

module M” = M’ 3
use” : ℕ
use” = M”.y

use’1 : ℕ
use’1 = y where open M’ 3

{-

“Using Them”:
- names in parameterised modules are are treated as functions
- can instantiate some parameters and name the resulting module
- can still open them as usual

--------------------------------------------------
-- ANONYMOUS MODULES
-- named-then-immediately-opened modules

module _ {A : Set} {a : A} · · ·
  ≈
module T {A : Set} {a : A} · · ·
open T

-- use-case : to  approximate the informal phrase
--   “for any A : Set and a : A, we have · · · ”

-- so common that variable keyword was introduced
--  Names in · · · are functions of only those variable-s they actually mention.

variable
  A : Set
  a : A

--------------------------------------------------
-- opening, using, hiding, renaming

open M hiding (n0; ...; nk)               : treat ni as private

open M using  (n0; ...; nk)               : treat only ni as public

open M renaming (n0 to m0; ...; nk to mk) : use names mi instead of ni


import X.Y.Z       : Use the definitions of module Z which lives in file ./X/Y/Z.agda.
open M public      : Treat the contents of M as if it is the public contents of the current module

Splitting a program over several files improves type checking performance,
since only need to type check files influenced by the change.

------------------------------------------------------------------------------
-- records : record ≈ module + data with one constructor
-}

record PointedSet : Set1 where
  constructor MkIt {- optional -}
  field
    Carrier : Set
    point   : Carrier

  {- like a module, so can add derived definitions -}
  blind : {A : Set} → A → Carrier
  blind = λ a → point

-- construct without named constructor
ex0 : PointedSet
ex0 = record {Carrier = ℕ; point = 3}

-- construct with named constructor
ex1 : PointedSet
ex1 = MkIt ℕ 3

open PointedSet
ex2 : PointedSet
Carrier ex2 = ℕ
point ex2 = 3

{-
Start with ex2 = ?, then in the hole enter C-c C-c RET to obtain the co-pattern setup.

Two tuples are the same when they have the same components.
Likewise a record is defined by its projections, whence co-patterns.
If you’re using many local definitions, you likely want to use co-patterns.

To enable projection of the fields from a record,
each record type comes with a module of the same name.
This module is parameterised by an element of the record type and
contains projection functions for the fields.
-}

useR0 : ℕ
useR0 = PointedSet.point ex0
{-
use¹ : ℕ
use¹ = point where open PointedSet ex0 -- TODO compile error
-}
open PointedSet
use² : ℕ
use² = blind ex0 true

-- pattern match on records

use³ : (P : PointedSet) → Carrier P
use³ record {Carrier = C; point = x} = x

use4 : (P : PointedSet) → Carrier P
use4 (MkIt C x) = x

{-
------------------------------------------------------------------------------
-- TODO Interacting with the real world —Compilation, Haskell, and IO

------------------------------------------------------------------------------
-- absurd patterns

When no constructor are matchable
- match the pattern ()
- provide no right hand side (since no way to could provide an arg to the function)

E.g., numbers smaller than a given natural number
-}

{- Fin n ∼= numbers i with i < n -}
data Fin : ℕ → Set where
  fzero : {n : ℕ} →         Fin (suc n) -- smaller than suc n for any n
  fsuc  : {n : ℕ} → Fin n → Fin (suc n) -- if i smaller than n then fsuc i is smaller than suc n.

{-
for each n, the type Fin n contains n elements
- Fin 2 has elements fsuc fzero and fzero
- Fin 0 has no elements

safe indexing function
-}
_!!_ : {A : Set} {n : ℕ} → Vec A n → Fin n → A
[]        !! ()               -- n is necessarily 0, but no way to make an element of type Fin 0
                              -- so use absurd pattern
(x :: xs) !! fzero  = x
(x :: xs) !! fsuc i = xs !! i


-- Logically “anything follows from false” becomes the following program:

data False : Set where

magic : {Anything-you-want : Set} → False → Anything-you-want
magic ()

{-
do
  magic x = ?
then case split on x
yields the program

------------------------------------------------------------------------------
-- isTrue pattern : passing around explicit proof objects
-- when not possible/easy to capture a desired precondition in types
-}

-- An empty record has only one value: record {} -}
record True : Set where

isTrue : Bool → Set
isTrue true  = True
isTrue false = False

_<0_ : ℕ → ℕ → Bool
_     <0  zero = false
zero  <0 suc y = true
suc x <0 suc y = x <0 y

find : {A : Set} (xs : List A) (i : ℕ) → isTrue (i <0 length xs) → A
find       []       i  ()
find (x ∷ xs)   zero  pf = x
find (x ∷ xs) (suc i) pf = find xs i pf

head’ : {A : Set} (xs : List A) → isTrue (0 <0 length xs) → A
head’       []  ()
head’ (x ∷ xs) _ = x

-- Unlike the _!!_ definition
-- rather than there being no index into the empty list
-- there is no proof that a natural number i is smaller than 0

{-
------------------------------------------------------------------------------
Mechanically Moving from Bool to Set —Avoiding “Boolean Blindness”

proposition represented as type whose elements denote proofs

Why use?
- awkward to request an index be “in bounds” in the find method,
- easier to encode this using Fin
- likewise, head’ is more elegant type when the non-empty precondition
  is part of the datatype definition, as in head.

recipe : from Boolean functions to inductive datatype families

1. Write the Boolean function.
2. Throw away all the cases with right side false.
3. Every case that has right side true corresponds to a new nullary constructor.
4. Every case that has n recursive calls corresponds to an n-ary constructor.

following these steps for _<0_:
-}

data _<1_ : ℕ → ℕ → Set where
  z< :   {y : ℕ} →          zero  <1     y
  s< : {x y : ℕ} → x <1 y → suc x <1 suc y

{-
then prove
- soundness    : constructed values correspond to Boolean-true statements
                 ensured by the second step in recipe
- completeness : true things correspond to terms formed from constructors.
-}

completeness : {x y : ℕ} → isTrue (x <0 y) → x <1 y
completeness     {x}  {zero} ()
completeness  {zero} {suc y} p = z<
completeness {suc x} {suc y} p = s< (completeness p)

{-
begin with
  completeness {x} {y} p = ?
then want case on p
but that requires evaluating x <0 y
which requires we know the shapes of x and y.

shape of proofs usually mimics the shape of definitions they use

------------------------------------------------------------------------------
record Payload (A : Set) : Set
  field
   pPayload  : [a]

record BlockType (A : Set) : Set
  = Proposal (Payload a) Author
  | NilBlock
  | Genesis

record LastVoteInfo
  fields
    lviLiDigest  :: HashValue
    lviRound     :: Round
    lviIsTimeout :: Bool
-}
