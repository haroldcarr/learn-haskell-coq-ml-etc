{-# OPTIONS --type-in-type   #-} -- NOT SOUND!

open import Data.Empty      using (⊥)
open import Data.Nat        renaming (ℕ to Nat)
open import Data.Nat.DivMod
open import Data.Product    hiding (map)
open import Data.Unit       using (⊤)
------------------------------------------------------------------------------
open import Relation.Binary.PropositionalEquality using (_≡_; refl)

module x where

{-
https://webspace.science.uu.nl/~swier004/publications/2019-icfp-submission-a.pdf

A predicate transformer semantics for effects

WOUTER SWIERSTRA and TIM BAANEN, Universiteit Utrecht

abstract

reasoning about effectful code harder than pure code

predicate transformer (PT) semantics gives a refinement relation that can be used to
- relate a program to its specification, or
- calculate effectful programs that are correct by construction

INTRODUCTION

key techniques
- syntax of effectful computations represented as free monads
  - assigning meaning to these monads gives meaning to the syntactic ops each effect provides
- paper shows how to assign PT semantics to computations arising from Kleisli arrows on free monads
  - enables computing the weakest precondition associated with a given postcondition
- using weakest precondition semantics
  - define refinement on computations
  - show how to
    - use this refinement relation to show a program satisfies its specification, or
    - calculate a program from its specification.
- show how programs and specifications may be mixed,
  enabling verified programs to be calculated from their specification one step at a time

2 BACKGROUND

Free monads
-}

-- C          : type of commands
-- Free C R   : returns an 'a' or issues command c : C
-- For each c : C, there is a set of responses R c
-- 2nd arg of Step is continuation : how to proceed after receiving response R c
data Free (C : Set) (R : C → Set) (a : Set) : Set where
  Pure :                           a  → Free C R a
  Step : (c : C) → (R c → Free C R a) → Free C R a

-- show that 'Free' is a monad:

map : ∀ {a b C : Set} {R : C → Set} → (a → b) → Free C R a → Free C R b
map f (Pure x)   = Pure (f x)
map f (Step c k) = Step c (λ r → map f (k r))

return : ∀ {a C : Set} {R : C → Set} → a → Free C R a
return = Pure

_>>=_ : ∀ {a b C : Set} {R : C → Set} → Free C R a → (a → Free C R b) → Free C R b
Pure   x >>= f = f x
Step c x >>= f = Step c (λ r → x r >>= f)

{-
different effects choose C and R differently, depending on their ops

Weakest precondition semantics

idea of associating weakest precondition semantics with imperative programs
dates to Dijkstra’s Guarded Command Language [1975]

ways to specify behaviour of function f : a → b
- reference implementation
- define a relation R : a → b → Set
- write contracts and test cases
- PT semantics

call values of type a → Set : predicate on type a

PTs are functions between predicates
e.g., weakest precondition:
-}

-- "maps"
-- function f : a → b and
-- desired postcondition on the function’s output, b → Set
-- to weakest precondition a → Set on function’s input that ensures postcondition satisfied
--
-- note: definition is just reverse function composition
-- wp0 : ∀ {a b : Set} → (f : a → b) → (b → Set) → (a → Set)
wp0 : ∀ {a : Set} {b :     Set} (f :      a  → b)   →           (b   → Set) → (a → Set)
wp0 f P = λ x → P   (f x)
{-
above wp semantics is sometimes too restrictive
- no way to specify that output is related to input
- fix via making f dependent:
-}
wp  : ∀ {a : Set} {b : a → Set} (f : (x : a) → b x) → ((x : a) → b x → Set) → (a → Set)
wp  f P = λ x → P x (f x)

-- shorthand for working with predicates and predicates transformers
_⊆_ : ∀ {a : Set} → (a → Set) → (a → Set) → Set
P ⊆ Q = ∀ x → P x → Q x

-- refinement relation defined between PTs
_⊑_ : ∀ {a : Set} {b : a → Set} → (pt1 pt2 : ((x : a) → b x → Set) → (a → Set)) → Set₁
pt1 ⊑ pt2 = ∀ P → pt1 P ⊆ pt2 P

{-
use refinement relation
- to relate PT semantics between programs and specifications
- to show a program satisfies its specification; or
- to show that one program is somehow ‘better’ than another,
  where ‘better’ is defined by choice of PT semantics

in pure setting, this refinement relation is not interesting:
the refinement relation corresponds to extensional equality between functions:

lemma follows from the ‘Leibniz rule’ for equality in intensional type theory:
refinement : ∀ (f g : a → b) → (wp f ⊑ wp g) ↔ (∀ x → f x ≡ g x)

this paper defines PT semantics for Kleisli arrows of form

    a → Free C R b

could use 'wp' to assign semantics to these computations directly,
but typically not interested in syntactic equality between free monads

rather want to study semantics of effectful programs they represent

to define a PT semantics for effects
define a function with form:

  -- how to lift a predicate on 'a' over effectful computation returning 'a'
  pt : (a → Set) → Free C R a → Set

'pt' def depends on semantics desired for a particulr free monad

Crucially,
choice of pt and weakest precondition semantics, wp, together
give a way to assign weakest precondition semantics to Kleisli arrows
representing effectful computations

3 PARTIALITY

Partial computations : i.e., 'Maybe'

make choices for commands C and responses R
-}

data C : Set where
  Abort : C -- no continuation

R : C → Set
R Abort = ⊥ -- since C has no continuation, valid responses is empty

Partial : Set → Set
Partial = Free C R

-- smart constructor for failure:
abort : ∀ {a : Set} → Partial a
abort = Step Abort (λ ())

{-
computation of type Partial a will either
- return a value of type a or
- fail, issuing abort command

Note that responses to Abort command are empty;
abort smart constructor abort uses this to discharge the continuation
in the second argument of the Step constructor

Example: division

expression language, closed under division and natural numbers:
-}

data Expr : Set where
  Val : Nat  → Expr
  Div : Expr → Expr → Expr

exv : Expr
exv = Val 3
exd : Expr
exd = Div (Val 3) (Val 3)

-- semantics specified using inductively defined RELATION:
-- def rules out erroneous results by requiring the divisor evaluates to non-zero
data _⇓_ : Expr → Nat → Set where
  Base : ∀ {x : Nat}
       → Val x ⇓ x
  Step : ∀ {l r : Expr} {v1 v2 : Nat}
       →     l   ⇓  v1
       →       r ⇓         (suc v2)
       → Div l r ⇓ (v1 div (suc v2))

exb : Val 3 ⇓ 3
exb = Base

exs : Div (Val 3) (Val 3) ⇓ 1
exs = Step Base Base

-- Alternatively
-- evaluate Expr via monadic INTERPRETER, using Partial to handle division-by-zero

-- op used by ⟦_⟧ interpreter
_÷_ : Nat → Nat → Partial Nat
n ÷ zero    = abort
n ÷ (suc k) = return (n div (suc k))

⟦_⟧ : Expr → Partial Nat
⟦ Val x ⟧     = return x
⟦ Div e1 e2 ⟧ = ⟦ e1 ⟧ >>= λ v1 → ⟦ e2 ⟧ >>= λ v2 → v1 ÷ v2

evv   : Free C R Nat
evv   = ⟦ Val 3 ⟧
evv'  : evv ≡ Pure 3
evv'  = refl

evd   : Free C R Nat
evd   = ⟦ Div (Val 3) (Val 3) ⟧
evd'  : evd ≡ Pure 1
evd'  = refl

evd0  : Free C R Nat
evd0  = ⟦ Div (Val 3) (Val 0) ⟧
evd0' : evd0 ≡ Step Abort (λ ())
evd0' = refl

{-
How to relate two definitions:
- std lib 'div' requires implicit proof that divisor is non-zero
  - ⇓ relation generates via pattern matching
  - _÷_ does explicit check
- interpreter uses _÷_
  - fails explicitly with abort when divisor is zero

Assign a weakest precondition semantics to Kleisli arrows of the form

    a → Partial b
-}

wpPartial : ∀ {a : Set} {b : a → Set}
          → (f : (x : a) → Partial (b x))
          → (P : (x : a) →          b x → Set)
          → (a → Set)
wpPartial f P = wp f (mustPT P)
 where
  mustPT : ∀ {a : Set} {b : a → Set}
         → (P : (x : a) → b x → Set)
         →      (x : a)
         → Partial       (b x) → Set
  mustPT P _ (Pure y)       = P _ y
  mustPT P _ (Step Abort _) = ⊥
{-
To call 'wp', must show how to transform
-      predicate                  P :         b → Set
- to a predicate on partial results : Partial b → Set
Done via proposition 'mustPT P c'
- holds when computation c of type Partial b successfully returns a 'b' that satisfies P

particular PT semantics of partial computations determined by def mustPT
here: rule out failure entirely
- so case Abort returns empty type

Given this PT semantics for Kleisli arrows in general,
can now study semantics of above monadic interpreter
via passing
- interpreter: ⟦_⟧
- desired postcondition : _⇓_
as arguments to wpPartial:

  wpPartial ⟦_⟧ _⇓_ : Expr → Set

resulting in a predicate on expressions

for all expressions satisfying this predicate,
the monadic interpreter and the relational specification, _⇓_,
must agree on the result of evaluation

What does this say about correctness of interpreter?
To understand the predicate better, consider defining this predicate on expressions:
-}

SafeDiv : Expr → Set
SafeDiv (Val x)     = ⊤
SafeDiv (Div e1 e2) = (e2 ⇓ zero → ⊥) {-∧-} × SafeDiv e1 {-∧-} × SafeDiv e2

{-
Expect : any expr e for which SafeDiv e holds
can be evaluated without division-by-zero

can prove SafeDiv is sufficient condition for two notions of evaluation to coincide:

-- lemma relates the two semantics
-- expressed as a relation and an evaluator
-- for those expressions that satisfy the SafeDiv property
correct : SafeDiv ⊆ wpPartial ⟦_⟧ _⇓_

Instead of manually defining SafeDiv, define more general predicate
characterising the domain of a partial function:
-}

dom : ∀ {a : Set} {b : a → Set}
    → ((x : a)
    → Partial (b x))
    → (a → Set)
dom f = wpPartial f (λ _ _ → ⊤)

{-
can show that the two semantics agree precisely on the domain of the interpreter:

sound    : dom       ⟦_⟧ ⊆ wpPartial ⟦_⟧ _⇓_
complete : wpPartial ⟦_⟧ _⇓_ ⊆ dom ⟦_⟧

both proofs proceed by induction on the argument expression

Refinement

weakest precondition semantics on partial computations give rise
to a refinement relation on Kleisli arrows of the form a → Partial b

can characterise this relation by proving:

refinement : (f g : a → Maybe b)
           → (wpPartial f ⊑ wpPartial g) ↔ (∀ x → (f x ≡ g x) ∨ (f x ≡ Nothing))

use refinement to relate Kleisli morphisms,
and to relate a program to a specification given by a pre- and postcondition


Example: Add (interpreter for stack machine)

add top two elements; can fail fail if stack has too few elements

below shows how to prove the definition meets its specification

Define specification in terms of a pre/post condition.
The specification of a function of type (x : a) → b x consists of
-}
record Spec (a : Set) (b : a → Set) : Set where
  constructor [_,_]
  field
    pre  : a → Set             -- a precondition on a, and
    post : (x : a) → b x → Set -- a postcondition relating inputs that satisfy this precondition
                               -- and the corresponding outputs
