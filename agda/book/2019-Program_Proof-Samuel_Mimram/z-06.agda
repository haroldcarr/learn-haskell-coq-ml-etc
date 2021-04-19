module z-06 where

import Relation.Binary.PropositionalEquality.Core as PE
open import Data.Nat            using (ℕ; zero; suc; _+_)
open import Relation.Binary.PropositionalEquality using (_≡_; refl)

{-
------------------------------------------------------------------------------
-- 267 p 6.2.2 Shortcuts.

C-c C-l       typecheck and highlight the current file
C-c C-,       get information about the hole under the cursor
C-c C-.       show inferred type for proposed term for a hole
C-c C-space   give a solution
C-c C-c       case analysis on a variable
C-c C-r       refine the hole
C-c C-a       automatic fill
C-c C-n       which normalizes a term (useful to test computations)
middle click  definition of the term

SYMBOLS

∧ \and
⊤ \top
→ \to
∀ \all
Π \Pi
λ \Gl
∨ \or
⊥ \bot
¬ \neg
∃ \ex
Σ \Sigma
≡ \equiv
ℕ \bN
× \times
≤ \le
∈ \in
⊎ \uplus
∷ \::
∎ \qed
x₁ \_1
x¹ \^1

------------------------------------------------------------------------------
-- p 268 6.2.3 The standard library.

default path /usr/share/agda-stdlib


Data.Empty    empty type (⊥)
Data.Unit     unit type (⊤)
Data.Bool     booleans
Data.Nat      natural numbers (ℕ)
Data.List     lists
Data.Vec      vectors (lists of given length)
Data.Fin      types with finite number of elements
Data.Integer  integers
Data.Float    floating point numbers
Data.Bin      binary natural numbers
Data.Rational rational numbers
Data.String   strings
Data.Maybe    option types
Data.AVL      balanced binary search trees

Data.Sum                               sum types (⊎, ∨)
Data.Product                           product types (×, ∧, ∃, Σ)
Relation.Nullary                       negation (¬)
Relation.Binary.PropositionalEquality  equality (≡)

------------------------------------------------------------------------------
-- p 277 6.3.4 Postulates.

for axioms (no proof)
avoided as much as possible
e.g, to work in classical logic, assume law of excluded middle with

postulate lem : (A : Set) → ¬ A ⊎ A

postulates do not compute:
- applying 'lem' to type A, will not reduce to ¬ A or A (as expected for a coproduct)
see section 6.5.6

------------------------------------------------------------------------------
-- p 277 6.3.5 Records.
-}

-- implementation of pairs using records
record Pair (A B : Set) : Set where
  field
    fst : A
    snd : B

make-pair : {A B : Set} → A → B → Pair A B
make-pair a b = record { fst = a ; snd = b }

proj1 : {A B : Set} → Pair A B → A
proj1 p = Pair.fst p

------------------------------------------------------------------------------
-- p 278 6.4 Inductive types: data
-- p 278 6.4.1 Natural numbers

{-
data ℕ : Set where
  zero :     ℕ -- base case
  suc  : ℕ → ℕ -- inductive case
-}


pred : ℕ → ℕ
pred  zero   = zero
pred (suc n) = n

_+'_ : ℕ → ℕ → ℕ
zero    +' n =          n
(suc m) +' n = suc (m +' n)
infixl 6  _+'_

_∸'_ : ℕ → ℕ → ℕ
zero ∸' n = zero
suc m ∸' zero  = suc m
suc m ∸' suc n = m ∸' n

_*'_ : ℕ → ℕ → ℕ
zero  *' n = zero
suc m *' n = (m *' n) + n

{-
_mod'_ :  ℕ → ℕ → ℕ
m mod' n with m <? n
m mod' n | yes _ = m
m mod' n | no _ = (m ∸' n) mod' n
-}

{-
------------------------------------------------------------------------------
-- p 280 Empty pattern matching.

use on types with no elements, e.g.,
-}
data ⊥'  : Set where

{-
there is no case to pattern match on elements of this type

uses pattern : () -- means that no such pattern can happen
-}

-- given an element of type ⊥ then "produce" anything
⊥'-elim : {A : Set} → ⊥' → A
⊥'-elim ()

{-
since A is arbitrary, no way, in proof, to exhibit one.
Do not have to.
'()'  states no cases to handle, so done

Useful in negation and orther less obvious ways of constructing empty inductive types.
E.g., the type zero ≡ suc zero of equalities between 0 and 1 is also an empty inductive type.
-}

{-
------------------------------------------------------------------------------
-- p 281 Anonymous pattern matching.
-}
-- curly brackets before args, cases separated by semicolons:
-- e.g.,

pred' : ℕ → ℕ
pred' = λ { zero → zero ; (suc n) → n }

{-
------------------------------------------------------------------------------
-- p 281  6.4.3 The induction principle.

pattern matching corresponds to the presence of a recurrence or induction principle.

e.g.,
f : → A
f  zero   = t
f (suc n) = u' -- u' might 'n' or result of recursive call f n

recurrence principle expresses this as
-}

rec : {A : Set} → A → (ℕ → A → A) → ℕ → A
rec t u  zero   = t
rec t u (suc n) = u n (rec t u n)

{-
Same as "recursor" for including nats to simply typed λ-calculus in section 4.3.6.

Any function of type ℕ → A defined using pattern matching can be re defined using this function.
This recurrence function encapsulates the expressive power of pattern matching.
e.g.,
-}

pred'' : ℕ → ℕ
pred'' = rec zero (λ n _ → n)

{-
logical : recurrence principle corresponds to elimination rule, so aka "eliminator"

Pattern matching in Agda is more powerful
- can be used to define functions whose return type depends on argument.

means must consider functions of the form

f : (n : ℕ) -> P n    -- P : ℕ → Set
f  zero   = t         --   : P zero
f (suc n) = u n (f n) --   : P (suc n)

corresponding dependent variant of trecurrence principle is called the induction principle:
-}

rec' : (P : ℕ → Set)
    → P zero
    → ((n : ℕ) → P n → P (suc n))
    → (n : ℕ)
    → P n
rec' P Pz Ps  zero   = Pz
rec' P Pz Ps (suc n) = Ps n (rec' P Pz Ps n)

{-
reading type as a logical formula, it says the recurrence principle over natural numbers:

    P (0) ⇒ (∀n ∈ ℕ.P (n) ⇒ P (n + 1)) ⇒ ∀n ∈ ℕ.P (n)
-}

-- proof by recurrence
+-zero' : (n : ℕ) → n + zero ≡ n
+-zero'  zero   = refl
+-zero' (suc n) = PE.cong suc (+-zero' n)

-- expressed using dependent induction principle
+-zero'' : (n : ℕ) → n + zero ≡ n
+-zero'' = rec' (λ n → n + zero ≡ n) refl (λ n p → PE.cong suc p)

------------------------------------------------------------------------------
-- p 282 Booleans

data Bool : Set where
  false : Bool
  true  : Bool

-- induction principle for booleans
Bool-rec : (P : Bool → Set) → P false → P true → (b : Bool) → P b
Bool-rec P Pf Pt false = Pf
Bool-rec P Pf Pt true  = Pt

------------------------------------------------------------------------------
-- p 283 Lists Data.List

data List (A : Set) : Set where
  []  :              List A
  _∷_ : A → List A → List A

length : {A : Set} → List A → ℕ
length      []  = 0
length (_ ∷ xs) = 1 + length xs

_++_ : {A : Set} → List A → List A → List A
[]       ++ l =            l
(x ∷ xs) ++ l = x ∷ (xs ++ l)

List-rec
  : {A : Set}
  → (P : List A → Set) → P [] → ((x : A) → (xs : List A) → P xs → P (x ∷ xs)) → (xs : List A)
  → P xs
List-rec P Pe Pc      []  = Pe
List-rec P Pe Pc (x ∷ xs) = Pc x xs (List-rec P Pe Pc xs)

------------------------------------------------------------------------------
-- p 284 6.4.7 Vectors.

data Vec (A : Set) : ℕ → Set where
  []  :                         Vec A  zero
  _∷_ : {n : ℕ} → A → Vec A n → Vec A (suc n)

{-
------------------------------------------------------------------------------
-- p 284 Dependent types.

type Vec A n depends on term n : a defining feature of dependent types.
-}

-- also, functions whose result type depends on argument, e.g.,
replicate : {A : Set} → A → (n : ℕ) → Vec A n
replicate x  zero   = []
replicate x (suc n) = x ∷ replicate x n

------------------------------------------------------------------------------
-- p 285 Dependent pattern matching.

-- since input is type Vec A (suc n), infers never applied to empty
head : {n : ℕ} {A : Set} → Vec A (suc n) → A
head (x ∷ xs) = x

------------------------------------------------------------------------------
-- p 285 Convertibility.

-- Agda is able to compare types up to β-reduction on terms (zero + n reduces to n):
-- - never distinguishes between two β-convertible terms

_++'_ : {m n : ℕ} {A : Set}
     → Vec A m → Vec A n → Vec A (m + n)
[]       ++' l = l                        -- Vec A (zero + n) ≡ Vec A n
(x ∷ xs) ++' l = x ∷ (xs ++' l)

------------------------------------------------------------------------------
-- p 285 Induction principle for Vec-rec

-- induction principle for vectors
Vec-rec : {A : Set}
        → (P : {n : ℕ} → Vec A n → Set)
        → P []
        → ({n : ℕ} → (x : A) → (xs : Vec A n) → P xs → P (x ∷ xs))
        → {n : ℕ}
        → (xs : Vec A n)
        → P xs
Vec-rec P Pe Pc      []  = Pe
Vec-rec P Pe Pc (x ∷ xs) = Pc x xs (Vec-rec P Pe Pc xs)

------------------------------------------------------------------------------
-- p 285 Indices instead of parameters.

-- general : can always encode a parameter as an index
-- recommended using parameters whenever possible (Agda handles them more efficiently)

-- use an index for type A (instead of a parameter)
data VecI : Set → ℕ → Set where
  []  : {A : Set} → VecI A zero
  _∷_ : {A : Set} {n : ℕ} (x : A) (xs : VecI A n) → VecI A (suc n)

-- induction principle for index VecI
VecI-rec : (P : {A : Set} {n : ℕ} → VecI A n → Set)
         → ({A : Set} → P {A} [])
         → ({A : Set} {n : ℕ} (x : A) (xs : VecI A n) → P xs → P (x ∷ xs))
         → {A : Set}
         → {n : ℕ}
         → (xs : VecI A n)
         → P xs
VecI-rec P Pe Pc      []  = Pe
VecI-rec P Pe Pc (x ∷ xs) = Pc x xs (VecI-rec P Pe Pc xs)

------------------------------------------------------------------------------
-- p 286 6.4.8 Finite sets.

-- has n elements
data Fin' : ℕ → Set where
  fzero' : {n : ℕ} →          Fin' (suc n)
  fsuc'  : {n : ℕ} → Fin' n → Fin' (suc n)

{-
Fin n is collection of ℕ restricted to Fin n = {0,...,n − 1}

above type corresponds to inductive set-theoretic definition:

    Fin      0  = ∅
    Fin (n + 1) = {0} ∪ {i + 1 | i ∈ Fin n}
-}

to : {n : ℕ} → Fin' n → ℕ
to  fzero'   = zero
to (fsuc' i) = suc (to i)

------------------------------------------------------------------------------
-- p 286 Vector lookup using Fin

-- Fin n typically used to index.

-- type ensures index in bounds
lookup : {n : ℕ} {A : Set} → Fin' n → Vec A n → A
lookup  fzero'   (x ∷ xs) = x
lookup (fsuc' i) (x ∷ xs) = lookup i xs

-- where index can be out of bounds
open import Data.Maybe
lookup' : ℕ → {A : Set} {n : ℕ} → Vec A n → Maybe A
lookup'  zero       []  = nothing
lookup'  zero   (x ∷ l) = just x
lookup' (suc i)     []  = nothing
lookup' (suc i) (x ∷ l) = lookup' i l

-- another option : add proof of i < n
{-
lookupP : {i n : ℕ} {A : Set}
        → i < n
        → Vec A n
        → A
lookupP     {i}       {.0}  ()     []
lookupP  {zero} {.(suc _)} i<n (x ∷ l) = x
lookupP {suc i} {.(suc _)} i<n (x ∷ l) = lookupP (≤-pred i<n) l
-}

{-
------------------------------------------------------------------------------
-- p 287 6.5 Inductive types: logic

Use inductive types to implement types used as logical formulas,
though the Curry-Howard correspondence.

What follows is a dictionary between the two.

------------------------------------------------------------------------------
-- 6.5.1 Implication : corresponds to arrow (→) in types
-}

-- classical formula   A ⇒ B ⇒ A  proved by
K : {A B : Set} → A → B → A
K x y = x

-- classical formula     (A ⇒ B ⇒ C) ⇒ (A ⇒ B) ⇒ A ⇒ C  proved by
S : {A B C : Set} → (A → B → C) → (A → B) → A → C
S g f x = g x (f x)

{-
------------------------------------------------------------------------------
-- 6.5.2 Product : corresponds to conjunction
defined in Data.Product
-}

data _×_ (A B : Set) : Set where
  _,_ : A → B → A × B

-- aka fst
proj₁ : {A B : Set} → A × B → A
proj₁ (a , b) = a

-- aka snd
proj₂ : {A B : Set} → A × B → B
proj₂ (a , b) = b

-- proof of A ∧ B ⇒ B ∧ A (commutativity of conjunction)
×-comm : {A B : Set} → A × B → B × A
×-comm (a , b) = (b , a)

-- proof of curryfication
×-→ : {A B C : Set} → (A × B → C) → (A → B → C)
×-→ f x y = f (x , y)
-- and
→-× : {A B C : Set} → (A → B → C) → (A × B → C)
→-× f (x , y) = f x y

{-
introduction rule for conjunction:
    Γ ⊢ A    Γ ⊢ B
    -------------- (∧I)
    Γ ⊢ A      ∧ B

general : when logical connectives are defined with inductive types,
- CONSTRUCTORS CORRESPOND TO INTRODUCTION RULES

------------------------------------------------------------------------------
Induction principle : ELIMINATION RULE CORRESPONDS TO THE ASSOCIATED INDUCTION PRINCIPLE
-}

-- for case where P does not depend on its arg,
-- below dependent induction principle (x-ind) implies simpler principle:
×-rec : {A B : Set}
  → (P : Set)
  → (A → B → P)
  → A × B → P
×-rec P Pp (x , y) = Pp x y
{-
corresponds to elimination rule for conjunction:
    Γ, A,B ⊢ P      Γ ⊢ A ∧ B
    ------------------------- (∧E)
               Γ ⊢ P
if the premises are true then the conclusion is also true
-}
-- dependent induction principle
-- corresponds to the elimination rule in dependent types
-- see 8.3.3
×-ind : {A B : Set}
  → (P : A × B → Set)
  → ((x : A) → (y : B) → P (x , y))
  → (p : A × B) → P p
×-ind P Pp (x , y) = Pp x y

{-
------------------------------------------------------------------------------
-- p 289 6.5.3 Unit type : corresponds to truth
Data.Unit
-}

data ⊤ : Set where
  tt : ⊤ -- constructor is introduction rule

{-
    ----- (>I)
    Γ ⊢>

know from logic : there is no introduction rule associated to truth.
-}

-- induction principle
⊤-rec
  : (P : ⊤ → Set)
  → P tt
  → (t : ⊤)
  → P t
⊤-rec P Ptt tt = Ptt

{-
    Γ ⊢ P    Γ ⊢> ⊤
    --------------- (>E)
         Γ ⊢ P

not interesting from a logical point of view:
if P holds and > holds
then can deduce that P holds, which was already known

------------------------------------------------------------------------------
-- 6.5.4 Empty type : corresponds to falsity
Data.Empty
-}

data ⊥ : Set where
  -- no constructor, thus no introduction rule

-- dependent induction principle
⊥-d-elim : (P : ⊥ → Set) → (x : ⊥) → P x
⊥-d-elim P ()

-- non-dependent variant of this principle
⊥-elim : (P : Set) → ⊥ → P
⊥-elim P () -- () is the empty pattern in Agda
            -- indicates there are no cases to handle when matching on a value of type

{-
corresponds to explosion principle, which is the associated elimination rule

    Γ ⊢ ⊥
    ---- (⊥E)
    Γ ⊢ P

------------------------------------------------------------------------------
6.5.5 Negation
Relation.Nullary
-}

¬ : Set → Set
¬ A = A → ⊥

-- e.g., A ⇒ ¬¬A  proved:
nni : {A : Set} → A → ¬ (¬ A)
nni x f = f x

{-
------------------------------------------------------------------------------
p 290 6.5.6 Coproduct : corresponds to disjunction
Data.Sum
-}
data _⊎_ (A : Set) (B : Set) : Set where
  inj₁ : A → A ⊎ B -- called injection of A into A ⊎ B
  inj₂ : B → A ⊎ B -- ditto               B

{-
The two constructors correspond to the two introduction rules
    Γ ⊢ A
    --------- (∨ᴸI)
    Γ ⊢ A ∨ B

    Γ ⊢     B
    --------- (∨ᴿI)
    Γ ⊢ A ∨ B
-}

-- e.g., commutativity of disjunction
⊎-comm : (A B : Set) → A ⊎ B → B ⊎ A
⊎-comm A B (inj₁ x) = inj₂ x
⊎-comm A B (inj₂ y) = inj₁ y

{-
-- e.g., proof of (A ∨ ¬A) ⇒ ¬¬A ⇒ A
lem-raa : {A : Set}
  → A ⊎ ¬ A
  → ¬ (¬ A)
  → A
lem-raa (inj₁ a)  k = a
lem-raa (inj₂ a') k = ⊥-elim (k a')

-- induction principle
-}
⊎-rec : {A B : Set}
  → (P : A ⊎ B → Set)
  → ((x : A) → P (inj₁ x))
  → ((y : B) → P (inj₂ y))
  → (u : A ⊎ B)
  → P u
⊎-rec P P₁ P₂ (inj₁ x) = P₁ x
⊎-rec P P₁ P₂ (inj₂ y) = P₂ y
{-
non-dependent induction principle corresponds to the elimination rule
    Γ, A ⊢ P    Γ, B ⊢ P    Γ ⊢ A ∨ B
    ----------------------------- (∨E)
                   Γ ⊢ P

------------------------------------------------------------------------------
Decidable types : A type A is decidable when it is known whether it is inhabited or not
i.e. a proof of A ∨ ¬A

could define predicate: Dec A is a proof that A is decidable
-}
Dec' : Set → Set
Dec' A = A ⊎ ¬ A -- by definition of the disjunction
{-
Agda convention
  write yes/no instead of inj₁/inj₂
because it answers the question: is A provable?

defined in Relation.Nullary as

-- p 291
A type A is decidable when
-}
data Dec (A : Set) : Set where
  yes :   A → Dec A
  no  : ¬ A → Dec A

{-
logic of Agda is intuitionistic
therefore A ∨ ¬A not provable for any type A, and not every type is decidable

it can be proved that no type is not decidable (see 2.3.5 and 6.6.8)
-}

nndec : (A : Set) → ¬ (¬ (Dec A))
nndec A n = n (no (λ a → n (yes a)))

{-
------------------------------------------------------------------------------
-- p 291 6.5.7 Π-types : corresponds to universal quantification

dependent types : types that depend on terms

some connectives support dependent generalizations

e.g., generalization of function types
  A → B
to dependent function types
  (x : A) → B
where x might occur in B.
the type B of the returned value depends on arg x

e.g.,

replicate : {A : Set} → A → (n : ℕ) → Vec A n

Dependent function types are also called Π-types

often written

    Π(x : A).B


can be define as (note: there is builtin notation in Agda)
-}
data Π {A : Set} (A : Set) (B : A → Set) : Set where
  Λ : ((a : A) → B a) → Π A B

{-
an element of Π A B is a dependent function

    (x : A) → B x

bound universal quantification bounded (the type A over which the variable ranges)

   corresponds to

    ∀x ∈ A.B(x)

proof of that formula corresponds to a function which
- to every x in A
- associates a proof of B(x)

why Agda allows the notation
    ∀ x → B x
for the above type (leaves A implicit)

Exercise 6.5.7.1.
Show that the type
     Π Bool (λ { false → A ; true → B })
is isomorphic to
     A × B

------------------------------------------------------------------------------
-- p 292 6.5.8 Σ-types : corresponds to bounded existential quantification
Data.Product
-}

-- dependent variant of product types : a : A , b : B a
data Σ (A : Set) (B : A → Set) : Set where
  _,_ : (a : A) → B a → Σ A B

-- actual Agda def is done using a record

dproj₁ : {A : Set} {B : A → Set} → Σ A B → A
dproj₁ (a , b) = a

dproj₂ : {A : Set} {B : A → Set} → (s : Σ A B) → B (dproj₁ s)
dproj₂ (a , b) = b

{-
Logical interpretation

type

    Σ A B

is bounded existential quantification

    ∃x ∈ A.B(x)

set theoretic interpretation corresponds to constructing sets by comprehension

    {x ∈ A | B(x)}

the set of elements x of A such that B(x) is satisfied

e.g., in set theory
- given a function f : A → B
- its image Im(f) is the subset of B consisting of elements in the image of f.

formally defined

     Im(f) = {y ∈ B | ∃x ∈ A.f (x) = y}

translated to with two Σ types
- one for the comprehension
- one for the universal quantification
Im : {A B : Set} (f : A → B) → Set
Im {A} {B} f = Σ B (λ y → Σ A (λ x → f x ≡ y))

e.g., can show that every function f : A → B has a right inverse (or section)
g : Im(f) → A

sec : {A B : Set} (f : A → B) → Im f → A
sec f (y , x , p) = x

------------------------------------------------------------------------------
-- p 293 : the axiom of choice

for every relation R ⊆ A × B satisfying ∀x ∈ A.∃y ∈ B.(x, y) ∈ R
there is a function f : A → B such that ∀x ∈ A.(x, f (x)) ∈ R

section 6.5.9 defined type Rel A B
corresponding to relations between types A and B

using Rel, can prov axiom of choice
AC : {A B : Set} (R : Rel A B)
  → ((x : A) → Σ B (λ y → R x y))
  → Σ (A → B) (λ f → ∀ x → R x (f x))
AC R f = (λ x → proj₁ (f x)) , (λ x → proj₂ (f x))

the arg that corresponds to the proof of (6.1), is constructive
a function which to every element x of type A
associates a pair of an element y of B
and a proof that (x, y) belongs to the relation.

By projecting it on the first component,
necessary function f is obtained (associates an element of B to each element of A)

use the second component to prove that it satisfies the required property

the "classical" variant of the axiom of choice
does not have access to the proof of (6.1) -- it only knows its existence.

another description is thus
where the double negation has killed the contents of the proof, see section 2.5.9

postulate CAC : {A B : Set} (R : Rel A B)
  → ¬ ¬ ((x : A) → Σ B (λ y → R x y))
  → ¬ ¬ Σ (A → B) (λ f → ∀ x → R x (f x))
see 9.3.4.

------------------------------------------------------------------------------
-- p 293 : 6.5.9 Predicates

In classical logic, the set B of booleans is the set of truth values:
- a predicate on a set A can either be false or true
- modeled as a function A → B

In Agda/intuitionistic logic
- not so much interested in truth value of predicate
- but rather in its proofs
- so the role of truth values is now played by Set

predicate P on a type A is term of type

  A → Set

which to every element x of A associates the type of proofs of P x.

------------------------------------------------------------------------------
-- p 294 Relations
Relation.Binary

In classical mathematics, a relation R on a set A is a subset of A × A (see A.1)

x of A is in relation with an element y when (x, y) ∈ R

relation on A can also be encoded as a function : A × A → 𝔹
or, curryfication,                              : A → A → 𝔹

In this representation, x is in relation with y when R(x, y) = 1

In Agda/intuitionistic
- relations between types A and B as type Rel A
- obtained by replacing the set 𝔹 of truth values with Set in the above description:

    Rel : Set → Set₁
    Rel A = A → A → Set

e.g.
- _≤_ : type Rel ℕ
- _≡_ : type Rel A

------------------------------------------------------------------------------
Inductive predicates (predicates defined by induction)
--e.g.,
-}

data isEven : ℕ → Set where
  even-z : isEven zero                               -- 0 is event
  even-s : {n : ℕ} → isEven n → isEven (suc (suc n)) -- if n is even then n + 2 is even
{-
corresponds to def of set E ⊆ N of even numbers
as the smallest set of numbers such that
    0 ∈ E
and
    n ∈ E ⇒ n+2 ∈ E
-}
data _≤_ : ℕ → ℕ → Set where
  z≤n : {n : ℕ}                 →  zero ≤     n
  s≤s : {m n : ℕ} (m≤n : m ≤ n) → suc m ≤ suc n
{-
smallest relation on ℕ such that
     0 ≤ 0
and
     m ≤ n implies m+1 ≤ n+1

inductive predicate definitions enable reasoning by induction over those predicates/relations
-}

≤-refl : {n : ℕ} → (n ≤ n)
≤-refl  {zero} = z≤n
≤-refl {suc n} = s≤s ≤-refl

-- p 295

≤-trans : {m n p : ℕ} → (m ≤ n) → (n ≤ p) → (m ≤ p)
≤-trans      z≤n       n≤p  = z≤n
≤-trans (s≤s m≤n) (s≤s n≤p) = s≤s (≤-trans m≤n n≤p)

{-
inductive defs good because of Agda's support for
reasoning by induction (and dependent pattern matching)
leading to simpler proofs

other defs possible

-- base on classical equivalence, for m, n ∈ N, m ≤ n ⇔ ∃m' ∈ N.m + m' = n
_≤'_ : ℕ → ℕ → Set
m ≤' n = Σ (λ m' → m + m' ≡eq n)

another:

le : ℕ → ℕ → Bool
le   zero       n  = true
le (suc m)   zero  = false
le (suc m) (suc n) = le m n

_≤'_ : ℕ → ℕ → Set
m ≤' n = le m n ≡ true

EXERCISE : show reflexivity and transitivity with the alternate formalizations

involved example
implicational fragment of intuitionistic natural deduction is formalized in section 7.2
the relation Γ ⊢ A
between a context Γ and a type A
which is true when the sequent is provable
is defined inductively

------------------------------------------------------------------------------
-- p 295 6.6 Equality
Relation.Binary.PropositionalEquality

-- typed equality : compare elements of same type
data _≡_ {A : Set} (x : A) : A → Set where
  refl : x ≡ x -- only way to be equal is to be the same

------------------------------------------------------------------------------
6.6.1 Equality and pattern matching

example proof with equality : successor function on natural numbers is injective
- for every natural numbers m and n
- m + 1 = n + 1 ⇒ m = n
-}

-- p 296

suc-injective : {m n : ℕ} → suc m ≡ suc n → m ≡ n
-- suc-injective {m} {n} p  -- Goal: m ≡ n;   p : suc m ≡ suc n
                            -- case split on p : it can ONLY be refl
                            -- therefore m is equal to n
                            -- so agda provide .m - not really an arg - something equal to m
suc-injective {m} {.m} refl --       m ≡ m
  = refl

{-
------------------------------------------------------------------------------
-- p 296 6.6.2 Main properties of equality: reflexive, congruence, symmetric, transitive
-}

sym : {A : Set} {x y : A}
  → x ≡ y
  → y ≡ x
sym refl = refl

trans : {A : Set} {x y z : A}
  → x ≡ y
  → y ≡ z
  → x ≡ z
trans refl refl = refl

cong : {A B : Set} (f : A → B) {x y : A}
  →   x ≡   y
  → f x ≡ f y
cong f refl = refl

-- substitutivity : enables transporting the elements of a type along an equality
subst : {A : Set} (P : A → Set) → {x y : A}
  →   x ≡ y
  → P x
  → P     y
subst P refl p = p

-- coercion : enables converting an element of type to another equal type
{- DOES NOT COMPILE
coe : {A B : Set}
  → A ≡ B
  → A
  → B
coe p x = subst (λ A → A) p x
-}

-- see 9.1

{-
------------------------------------------------------------------------------
-- p 296 6.6.3 Half of even numbers : example every even number has a half
using proof strategy from 2.3

traditional notation : show  ∀n ∈ N. isEven(n) ⇒ ∃m ∈ ℕ.m + m = n
-}

+-suc : ∀ (m n : ℕ)
      →      m + suc n
      ≡ suc (m +     n)
+-suc  zero   n = refl
+-suc (suc m) n = cong suc (+-suc m n)

{- DOES NOT COMPILE
even-half : {n : ℕ}
  → isEven n
  → Σ (λ m → m + m ≡ n)
even-half  even-z = zero , refl
even-half (even-s e) with even-half e
even-half (even-s e) | m , p =
  suc m , cong suc (trans (+-suc m m) (cong suc p))

second case : by induction have m such that m + m = n
need to construct a half for n + 2: m + 1
show that it is a half via

    (m + 1) + (m + 1) =  (m + (m  + 1)) + 1   by definition of addition
                      = ((m +  m) + 1)  + 1   by +-suc
                      =       (n  + 1)  + 1   since m + m = n

implemented using transitivity of equality and
fact that it is a congruence (m + 1 = n + 1 from m = n)
also use auxiliary lemma m + (n + 1) = (m + n) + 1
-}

{-
------------------------------------------------------------------------------
-- p 297 6.6.4 Reasoning
-}

--import      Relation.Binary.PropositionalEquality as Eq
--open        Eq.≡-Reasoning using (begin_; _≡⟨⟩_; step-≡; _∎)

infix  3 _∎
infixr 2 _≡⟨⟩_ _≡⟨_⟩_
infix  1 begin_

begin_ : ∀ {ℓ} {A : Set ℓ} {x y : A} → x ≡ y → x ≡ y
begin_ x≡y = x≡y

_≡⟨⟩_ : ∀ {ℓ} {A : Set ℓ} (x {y} : A) → x ≡ y → x ≡ y
_ ≡⟨⟩ x≡y = x≡y

_≡⟨_⟩_ : {A : Set} (x {y z} : A) → x ≡ y → y ≡ z → x ≡ z
_ ≡⟨ x≡y ⟩ y≡z = trans x≡y y≡z

_∎ : ∀ {ℓ} {A : Set ℓ} (x : A) → x ≡ x
_∎ _ = refl

+-zero : ∀ (n : ℕ) → n + zero ≡ n
+-zero   zero  = refl
+-zero (suc n) rewrite +-zero n = refl

+-comm : (m n : ℕ) → m + n ≡ n + m
+-comm m zero    = +-zero m
+-comm m (suc n) =
  begin
    (m + suc n) ≡⟨ +-suc m n ⟩             -- m + (n + 1) = (m + n) + 1 -- by +-suc
    suc (m + n) ≡⟨ cong suc (+-comm m n) ⟩ -- = (n + m) + 1 -- by induction hypothesis
    suc (n + m)
  ∎

-- p 298
-- another proof using properties of equality
+-comm' : (m n : ℕ) → m + n ≡ n + m
+-comm' m   zero  = +-zero m
+-comm' m (suc n) = trans (+-suc m n) (cong suc (+-comm m n))

{-
------------------------------------------------------------------------------
-- p 298 6.6.5 Definitional equality

two terms which are convertible (i.e. re duce to a common term) are considered to be EQUAL.

not ≡, but equality internal to Agda, referred to as definitional equality
- cannot distinguish between two definitionally equal terms
- e.g., zero + n is definitionally equal to n (because addition defined that way)
- definitional equality implies equality by refl:
-}

+-zero''' : (n : ℕ) → zero + n ≡ n
+-zero''' n = refl

-- n + zero and n are not definitionally equal (because def of addition)
-- but can be proved
+-0 : (n : ℕ) → n + zero ≡ n
+-0  zero   = refl
+-0 (suc n) = cong suc (+-zero n)

-- implies structure of definitions is important (and an art form)

------------------------------------------------------------------------------
-- p 299 6.6.6 More properties with equality

-- zero is NOT the successor of any NAT
zero-suc : {n : ℕ} → zero ≡ suc n → ⊥
zero-suc ()

+-assoc : (m n o : ℕ) → (m + n) + o ≡ m + (n + o)
+-assoc  zero   n o = refl
+-assoc (suc m) n o = cong suc (+-assoc m n o)

-- *-+-dist-r TODO
-- *-assoc    TODO

------------------------------------------------------------------------------
-- p 299 Lists

++-empty' : {A : Set}
          → (l : List A)
          → [] ++ l ≡ l
++-empty' l = refl

empty-++ : {A : Set}
          → (l : List A)
          → l ++ [] ≡ l
empty-++     []  = refl
empty-++ (x ∷ l) = cong (x ∷_) (empty-++ l)

++-assoc : {A : Set}
         → (l1 l2 l3 : List A)
         → ((l1 ++ l2) ++ l3) ≡ (l1 ++ (l2 ++ l3))
++-assoc      []  l2 l3 = refl
++-assoc (x ∷ l1) l2 l3 = cong (x ∷_) (++-assoc l1 l2 l3)

{- DOES NOT COMPILE: Set₁ != Set
++-not-comm : ¬ ({A : Set} → (l1 l2 : List A) → (l1 ++ l2) ≡ (l2 ++ l1))
++-not-comm f with f (1 ∷ []) (2 ∷ [])
...  | ()
-}

++-length : {A : Set}
          → (l1 l2 : List A)
          → length (l1 ++ l2) ≡ length l1 + length l2
++-length      []  l2 = refl
++-length (x ∷ l1) l2 = cong (1 +_) (++-length l1 l2)

-- adds element to end of list
snoc : {A : Set} → List A → A → List A
snoc     []  x = x ∷ []
snoc (y ∷ l) x = y ∷ (snoc l x)

-- reverse a list
rev : {A : Set} → List A → List A
rev     []  = []
rev (x ∷ l) = snoc (rev l) x

-- reversing a list with x as the last element will prodice a list with x as the first element
rev-snoc : {A : Set}
         → (l : List A) → (x : A)
         → rev (snoc l x) ≡ x ∷ (rev l)
rev-snoc     []  x = refl
rev-snoc (y ∷ l) x = cong (λ l → snoc l y) (rev-snoc l x)

-- reverse twice
rev-rev : {A : Set}
        → (l : List A)
        → rev (rev l) ≡ l
rev-rev     []  = refl
rev-rev (x ∷ l) = trans (rev-snoc (rev l) x)
                        (cong (x ∷_) (rev-rev l))


--------------------------------------------------
-- 6.6.7 The J rule.

-- equality
data _≡'_ {A : Set} : A → A → Set where
  refl' : {x : A} → x ≡' x

-- has associated induction principle :  J rule:
-- to prove P depending on equality proof p,
-- - prove it when this proof is refl
J : {A : Set} {x y : A}
    (p : x ≡' y)
    (P : (x y : A) → x ≡' y → Set)
    (r : (x : A)   → P x x refl')
  → P x y p
J {A} {x} {.x} refl' P r = r x

{-
section 6.6 shows that definition usually taken in Agda is different
(it uses a parameter instead of an index for the first argument of type A),
so that the resulting induction principle is a variant:
-}

J' : {A : Set}
    (x : A)
    (P : (y : A) → x ≡' y → Set)
    (r : P x refl')
    (y : A)
    (p : x ≡' y)
  → P y p
J' x P r .x refl' = r

{-
--------------------------------------------------
6.6.8 Decidable equality.

A type A is decidable when either A or ¬A is provable.

Write Dec A for the type of proofs of decidability of A
- yes p, where p is a proof of  A, or
- no  q, where q is a proof of ¬A

A relation on a type A is decidable when
- the type R x y is decidable
- for every elements x and y of type A.
-}

-- in Relation.Binary
-- term of type Decidable R is proof that relation R is decidable
Decidable : {A : Set} (R : A → A → Set) → Set
Decidable {A} R = (x y : A) → Dec (R x y)

{-
A type A has decidable equality when equality relation _≡_ on A is decidable.
- means there is a function/algorithm able to determine,
  given two elements of A,
  whether they are equal or not
- return a PROOF (not a boolean)
-}

_≟_ : Decidable {A = Bool} _≡_
false ≟ false = yes refl
true  ≟ true  = yes refl
false ≟ true  = no (λ ())
true  ≟ false = no (λ ())

_≟ℕ_ : Decidable {A = ℕ} _≡_
zero  ≟ℕ zero  = yes refl
zero  ≟ℕ suc n = no(λ())
suc m ≟ℕ zero  = no(λ())
suc m ≟ℕ suc n with m ≟ℕ n
... | yes refl = yes refl
... | no  ¬p   = no (λ p → ¬p (suc-injective p))

{-
--------------------------------------------------
p 302 6.6.9 Heterogeneous equality : enables comparing (seemingly) distinct types

due to McBride [McB00]

e.g., show concatenation of vectors is associative

    (l1 ++ l2) ++ l3 ≡ l1 ++ (l2 ++ l3) , where lengths are m, n and o

equality ≡ only used on terms of same type
- but types above are
- left   Vec A ((m + n) + o)
- right  Vec A (m + (n + o))

the two types are propositionally equal, can prove

   Vec A ((m + n) + o) ≡ Vec A (m + (n + o))

by

  cong (Vec A) (+-assoc m n o)

but the two types are not definitionally equal (required to compare terms with ≡)

PROOF WITH STANDARD EQUALITY.

To compare vecs, use above propositional equality
and'coe' to cast one of the members to the same type as other.
   term
      coe (cong (Vec A) (+-assoc m n o))
has type
      Vec A ((m + n) + o) → Vec A (m + (n + o))
-}

-- lemma : if l and l’ are propositional equal vectors,
-- up to propositional equality of their types as above,
-- then x : l and x : l’ are also propositionally equal:
{- DEPENDS ON 'coe' WHICH DOES NOT COMPILE
∷-cong : {A : Set} → {m n : ℕ} {l1 : Vec A m} {l2 : Vec A n}
       → (x : A)
       → (p : m ≡ n)
       → coe (cong (Vec A) p) l1 ≡ l2
       → coe (cong (Vec A) (cong suc p)) (x ∷ l1) ≡ x ∷ l2
∷-cong x refl refl = refl

++-assoc : {A : Set} {m n o : ℕ}
         → (l1 : Vec A m)
         → (l2 : Vec A n)
         → (l3 : Vec A o)
         → coe (cong (Vec A) (+-assoc m n o)))
               ((l1 ++ l2) ++ l3) ≡ l1 ++ (l2 ++ l3)
++-assoc                          []  l2 l3 = refl
++-assoc {_} {suc m} {n} {o} (x : l1) l2 l3 = ∷-cong x (+-assoc m n o) (++-assoc l1 l2 l3)
-}
