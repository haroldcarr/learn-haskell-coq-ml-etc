import Relation.Binary.PropositionalEquality as PE

module x01 where

{-
Dependent Types at Work
Ana Bove and Peter Dybjer
Chalmers University of Technology, Göteborg, Sweden

intro to functional programming with dependent types

- simply typed functional programming in the style of Haskell and ML
- discuss differences between Agda’s type system and the Hindley-Milner type system of Haskell/ML
- how to use dependent types for programming
- explain ideas behind type-checking dependent types.
- explain Curry-Howard identification of propositions and types
- show a method to encode partial/general recursive functions as total functions using dependent types


Dependent types are types that depend on elements of other types (e.g., Vec of length n)
Invariants can also be expressed with dependent type (e.g., sorted list)

Agda's dependent type system is an extension of Martin-Löf type theory [20,21,22,25].

Parametrised types
- e.g.,  [A]
- usually not called dependent types
- they are are families of types indexed by other types
  - not families of types indexed by ELEMENTS of another type
- however, in dependent type theories, there is a type of small types (a universe),
  so that we have a type [A] of lists of elements of a given small type A.
- FORTRAN could define arrays of a given dimension, so a form a dependent type on small type
- simply typed lambda calculus and the Hindley-Milner type system,
  (e.g., Haskell/ML) do not include dependent types, only parametrised types
- polymorphic lambda calculus System F [15] has types like
  ∀X.A where X ranges over all types,
  but no quantification over ELEMENTS of other types

The modern development of dependently typed programming languages has
its origins in the Curry-Howard isomorphism between propositions and types.

In the 1930’s Curry noticed the similarity between the axioms of implicational logic
   P ⊃ Q ⊃ P                                (P ⊃ Q ⊃ R) ⊃ (P ⊃ Q) ⊃ P ⊃ R
and types of the combinators
   K              and                       S
   A → B → A                                (A → B → C) → (A → B) → A → C

K can be viewed as a witness (i.e., proof object) of the truth of P ⊃ Q ⊃ P
S can be viewed as a withness of the truth of (P ⊃ Q ⊃ R) ⊃ (P ⊃ Q) ⊃ P ⊃ R

typing rule for application
corresponds to the inference rule modus ponens: from P ⊃ Q and P conclude Q
- f : A → B
- a : A
- f a → B

product types correspond to conjunctions
 sum types (disjoint unions) to disjunctions

To extend this correspondence to predicate logic
Howard and de Bruijn introduced dependent types A(x) corresponding to predicates P(x).
They formed
indexed products Π x : D.A(x) : corresponding to universal   quantifications ∀x : D.P (x)
indexed sums     Σ x : D.A(x) :                  existential quantifications ∃x : D.P (x)

This obtains a Curry-Howard interpretation of intuitionistic predicate logic.

one-to-one correspondence between propositions and types in a type system with
dependent types.

one-to-one correspondence between proofs of a certain proposition in constructive predicate logic
and terms of the corresponding type.

to accommodate equality in predicate logic
introduce the type a == b of proofs that a and b are equal.
Get a Curry-Howard interpretation of predicate logic with equality.

Can go further and add natural numbers with addition and multiplication
to obtain a Curry-Howard version of Heyting (intuitionistic) arithmetic.

Curry-Howard interpretation was basis for Martin-Löf’s intuition-istic type theory [20,21,22].
- propositions and types are actually identified
- intended to be a foundational system for constructive mathematics
- but can also be used as a programming language [21]
- from 1980’s and onwards, systems implementing variants of Martin-Löf type theory
  - NuPRL [6] : implementing an extensional version of the theory
  - Coq [33]  : implementing an intensional impredicative version
  - Agda      : implements an intensional predicative extension

About these Notes

Agda's type-checking algorithm
- Section 5
- ideas first presented by Coquand [7]
- more information
  - Norell’s thesis [26]
  - Abel, Coquand, and Dybjer [1,2]
  - Coquand, Kinoshita, Nordström, and Takeyama [8].

more complete understanding of dependent type theory
- read one of the books about Martin-Löf type theory and related systems
- Martin-Löf’s “Intuitionistic Type Theory” [22] is a classic, although the
  reader should be aware that it describes an extensional version of the theory.
- Nordström, Petersson, and Smith [25] contains a description of the later
  intensional theory on which Agda is based.
- variants of dependent type theory are
  - Thompson [34]
  - by Constable et al [6] on the NuPRL-system,
  - Bertot and Casteran [4] on the Coq-system
- lecture notes (available from the Agda wiki [3]) by Norell complement the present notes
  - collection of advanced examples of how to use Agda for dependently typed programming.
- Geuvers’ lecture notes provide an introduction to type theory including
  Barendregt’s pure type systems and their most important meta-theoretic prop-
  erties.
- Bertot’s notes describe how dependent types (in Coq) can be used for
  implementing a number of concepts occurring in a course in programming lan-
  guage theory with the focus on abstract interpretation.
- Barthe, Grégoire, and Riba’s notes present a method for making more powerful
  termination-checkers.

Section 2 : ordinary functional programming
Section 3 : basic dependent types and shows how to use them
Section 4 : Curry-Howard isomorphism
Section 5 : aspects of type-checking and pattern matching with dependent types
Section 6 : use Agda as a programming logic
Section 7 : how to represent general recursion and partial functions as total functions in Agda

------------------------------------------------------------------------------
2 Simply Typed Functional Programming in Agda

2.1 Truth Values
-}
data Bool : Set where
  true  : Bool
  false : Bool
{-
states
- Bool is a data type with the two (no-arg) constructors
- “:” denotes type membership
- Bool is a member of the type Set
- Set
  - the type of sets (using a terminology introduced by Martin-Löf [22])
    or small types (mentioned in the introduction).
- Bool is a small type, but Set itself is not, it is a large type.
- If we added that Set : Set, the system would actually become
  inconsistent and hence, we would be able to prove any property.
-}
not : Bool -> Bool
not true  = false
not false = true
{-
Agda cannot generally infer types.
Because Agda type-checking done via normalisation (simplification)
- without the normality restriction it may not terminate
-}
equiv : Bool -> Bool -> Bool
equiv false false = true
equiv  true false = false
equiv false  true = false
equiv  true  true = true

_||_ : Bool -> Bool -> Bool
true  || _     = true
false || true  = true
false || false = false
infixl 60 _||_

-- EXERCISE: Define some more truth functions, such as conjunction and implication.

_&&_ : Bool -> Bool -> Bool
false && _     = false
true  && false = false
true  && true  = true
-- infixl ?? _&&_

-- 2.2 Natural Numbers

-- such data types known as recursive in Haskell
-- in constructive type theory, referred to as inductive types or inductively defined types

data Nat : Set where
  zero : Nat
  succ : Nat -> Nat
{-# BUILTIN NATURAL Nat #-}

pred : Nat -> Nat
pred  zero    = zero
pred (succ n) = n

_+_ : Nat -> Nat -> Nat
zero + m = m
succ n + m = succ (n + m)
{-# BUILTIN NATPLUS _+_ #-}

_*_ : Nat -> Nat -> Nat
zero   * n = zero
succ n * m = (n * m) + m
{-# BUILTIN NATTIMES _*_ #-}

_ : (2 * 3) + 10 PE.≡ 16
_ = PE.refl
{-
+ and * are defined by PRIMITIVE RECURSION in FIRST ARG.
- base case for zero
- step case for non zero defined recursively

Given a first order data type
- distinguish between
  - canonical
    - built by constructors only
    - e.g., true, false
form. Moreover, zero, succ zero, succ (succ zero), . . . , are canonical forms,
  - non-canonical forms
    - whereas non-canonical elements might contain defined functions
    - e.g., not true, zero + zero, zero * zero, succ (zero + zero)

Martin-Löf  [21] instead considers lazy canonical forms
- a term begins with a constructor to be considered a canonical form
- e.g., succ (zero + zero) is a lazy canonical form, but not a “full” canonical form
- Lazy canonical forms are appropriate for lazy functional programming languages (e.g., Haskell)

-- EXERCISE: Write the cut-off subtraction function
-- the function on natural numbers, which returns 0
-- if the second argument is greater than or equal to the first.
-}
_∸_ : Nat -> Nat -> Nat
zero   ∸ succ _ = zero
zero   ∸ zero   = zero
succ n ∸ zero   = succ n
succ n ∸ succ m = n ∸ m

_ :  0 ∸  0 PE.≡  0
_ = PE.refl
_ :  0 ∸ 10 PE.≡  0
_ = PE.refl
_ : 10 ∸  0 PE.≡ 10
_ = PE.refl
_ : 10 ∸ 10 PE.≡  0
_ = PE.refl
_ : 10 ∸  9 PE.≡  1
_ = PE.refl

-- Also write some more numerical functions like < or <=

_<_ : Nat -> Nat -> Bool
zero   < zero   = false
zero   < succ _ = true
succ _ < zero   = false
succ n < succ m = n < m

_ :  9 < 10 PE.≡ true
_ = PE.refl
_ : 10 < 10 PE.≡ false
_ = PE.refl

{-
2.3 Lambda Notation and Polymorphism

Agda is based on the typed lambda calculus. We have already seen that applica-
tion is written by juxtaposition. 

Lambda abstraction is either written Curry-style without a type label on the argument x
   \x -> e
or Church-style with a type label
  \(x : A) -> e

a family of identity functions, one for each small type:
-}
-- idE(xplicit)
idE :  (A : Set) ->       A  -> A
idE = \(A : Set) -> \(x : A) -> x
{-
apply this “generic” identity function id to a type argument A
to obtain the identity function from A to A

1st use of dependent types: type A -> A depends on variable A : Set
- ranging over the small types

Agda’s notation for dependent function types
- if A is a type and B[x] is a type which depends on (is indexed by) (x : A)
  then (x : A) -> B[x] is the type of functions f mapping arguments (x : A) to values f x : B[x].

If the type-checker can figure out value of an arg, can use a wild card character:
id _ x : A

K and S combinators in Agda (uses telescopic notation (A B : Set)
-}
K : (A B : Set) -> A -> B -> A
K _ _ x _     = x

S : (A B C : Set) -> (A -> B -> C) -> (A -> B) -> A -> C
S _ _ _ f g x = f x (g x)
{-
2.4 Implicit Arguments

declared by enclosing typings in curly brackets
-}

id : {A : Set} -> A -> A
id x = x

-- implicit arguments are omitted in applications
_ : id       zero PE.≡ zero
_ = PE.refl
-- can explicitly write an implicit argument by using curly brackets if needed
_ : id {Nat} zero PE.≡ zero
_ = PE.refl
_ : id {_}   zero PE.≡ zero
_ = PE.refl
{-
2.5 Gödel System T is a subsystem of Agda.

System of primitive recursive functionals [16]
- important in logic and a precursor to Martin-Löf type theory.

Recursion restricted to primitive recursion : to make sure programs terminate.

Gödel System T
- based on simply typed lambda calculus
- with two base types : truth values and natural numbers
  - (Some formulations code truth values as 0 and 1.)
- constants for constructors true, false, zero
- conditional and primitive recursion combinators.
-}

-- conditional
if_then_else_ : {C : Set} -> Bool -> C -> C -> C
if true then x else y = x
if false then x else y = y

-- primitive recursion combinator for natural numbers
-- natrec is a higher-order function defined by primitive recursion with args
-- - (implicit) return type
-- - p : element returned in base case
-- - h : step function
-- - the Nat on which to perform the recursion
natrec : {C : Set} -> C -> (Nat -> C -> C) -> Nat -> C
natrec p h  zero    = p
natrec p h (succ n) = h n (natrec p h n)

-- natrec definitions of addition and multiplication
_plus_ : Nat -> Nat -> Nat
_plus_ n m = natrec m    (\x y -> succ y)   n

_mult_ : Nat -> Nat -> Nat
_mult_ n m = natrec zero (\x y -> y plus m) n

-- compare plus/mult to _+_/_*_

_ : succ (succ  zero) + 2   PE.≡
    succ (succ  zero  + 2)
_ = PE.refl
_ : succ (succ  zero  + 2)  PE.≡
    succ (succ (zero  + 2))
_ = PE.refl
_ : succ (succ (zero  + 2)) PE.≡
    succ (succ          2 )
_ = PE.refl

_ : succ (succ  zero) plus 2   PE.≡
    natrec 2 (\x y -> succ y) (succ (succ zero))
_ = PE.refl
_ : natrec 2 (\x y -> succ y) (succ (succ zero)) PE.≡
    (\x y -> succ y) (succ zero) (natrec 2 (\x y -> succ y) (succ zero))
_ = PE.refl
_ : (\x y -> succ y) (succ zero) (natrec 2 (\x y -> succ y) (succ zero)) PE.≡
    (\  y -> succ y)             (natrec 2 (\x y -> succ y) (succ zero))
_ = PE.refl
_ : (\  y -> succ y)             (natrec 2 (\x y -> succ y) (succ zero)) PE.≡
             succ                (natrec 2 (\x y -> succ y) (succ zero))
_ = PE.refl
_ :          succ                (natrec 2 (\x y -> succ y) (succ zero)) PE.≡
             succ                ((\x y -> succ y) zero (natrec 2 (\x y -> succ y) zero))
_ = PE.refl
_ :          succ                ((\x y -> succ y) zero (natrec 2 (\x y -> succ y) zero)) PE.≡
             succ                ((\  y -> succ y)      (natrec 2 (\x y -> succ y) zero))
_ = PE.refl
_ :          succ                ((\  y -> succ y)      (natrec 2 (\x y -> succ y) zero)) PE.≡
             succ                (         succ         (natrec 2 (\x y -> succ y) zero))
_ = PE.refl
_ :          succ                (         succ         (natrec 2 (\x y -> succ y) zero)) PE.≡
             succ                (         succ                 2                       )
_ = PE.refl

{-
To stay entirely in Gödel system T
- use terms built up by variables, application, lambda abstraction,
- and the constants true, false, zero, succ, if_then_else_, natrec

Gödel system T has unusual property (for a programming language)
- all typable programs terminate

not only do terms in base types Bool and Nat terminate whatever reduction is chosen,
but also terms of function type terminate;

the reduction rules are β-reduction,
and the defining equations for if then else and natrec.

Reductions can be performed anywhere in a term,
so in fact there may be several ways to reduce a term.

Therefore, Gödel system T is strongly normalising
- any typable term reaches a normal form whatever reduction strategy is chosen.

Even in a this restrictive Gödel system T, possible to define many numerical functions.
Can define all primitive recursive functions
(in the usual sense without higher-order functions).

Can also define functions which are not primitive recursive, such as the Ackermann function.

Gödel system T is important in the history of ideas that led to the Curry-Howard
isomorphism and Martin-Löf type theory.

Gödel system T is the simply typed kernel of Martin-Löf’s constructive type theory,
Martin-Löf type theory is the foundational system from which Agda grew.

relationship between Agda and Martin-Löf type theory is much like the
relationship between Haskell and the simply typed lambda calculus.
Or compare it with the relationship between Haskell and Plotkin’s PCF [30].

Like Gödel system T,
PCF is based on the simply typed lambda calculus with truth values and natural numbers.
But PCF includes a fixed point combinator
- used for encoding arbitrary general recursive definitions
  - therefore can define non-terminating functions in PCF.
-}

-- Exercise: Define all functions previously given in the text in Gödel System T.

{-
2.6
Parametrised Types
As already mentioned, in Haskell we have parametrised types such as the type
[a] of lists with elements of type a. In Agda the analogous definition is as follows:
-}

