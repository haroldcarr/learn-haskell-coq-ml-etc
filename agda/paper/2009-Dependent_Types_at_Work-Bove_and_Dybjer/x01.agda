import Relation.Binary.PropositionalEquality as PE

module x01 where

import Data.Empty.Polymorphic as DEP
open import Data.Unit.Polymorphic
import Data.Maybe as Maybe


{-
Dependent Types at Work
Ana Bove and Peter Dybjer
Chalmers University of Technology, Göteborg, Sweden

intro to functional programming with dependent types

- simply typed functional programming in style of Haskell and ML
- discuss differences between Agda’s type system and Hindley-Milner type system of Haskell/ML
- how to use dependent types for programming
- explain ideas behind type-checking dependent types
- explain Curry-Howard identification of propositions and types
- show a method to encode partial/general recursive functions
  as total functions using dependent types

Dependent types are types that depend on elements of other types (e.g., Vec of length n)
Invariants can be expressed with dependent type (e.g., sorted list)

Agda's dependent type system is an extension of Martin-Löf type theory [20,21,22,25].

Parametrised types
- e.g., [A]
- usually not called dependent types
- they are are families of types indexed by other types
  - NOT families of types indexed by ELEMENTS of another type
- however, in dependent type theories, there is a type of small types (a universe),
  so that we have a type [A] of lists of elements of a given small type A.
- FORTRAN could define arrays of a given dimension, so a form a dependent type on small type
- simply typed lambda calculus and the Hindley-Milner type system,
  (e.g., Haskell/ML) do not include dependent types, only parametrised types
- polymorphic lambda calculus System F [15] has types like
  ∀X.A where X ranges over all types,
  but no quantification over ELEMENTS of other types

development of dependently typed programming languages has
origins in Curry-Howard isomorphism between propositions and types

1930’s : Curry noticed similarity between axioms of implicational logic
   P ⊃ Q ⊃ P                                (P ⊃ Q ⊃ R) ⊃ (P ⊃ Q) ⊃ P ⊃ R
and types of combinators
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

to extend this correspondence to predicate logic
Howard and de Bruijn introduced dependent types A(x) corresponding to predicates P(x).
They formed
indexed products Π x : D.A(x) : corresponding to universal   quantifications ∀x : D.P (x)
indexed sums     Σ x : D.A(x) :                  existential quantifications ∃x : D.P (x)

this gives a Curry-Howard interpretation of intuitionistic predicate logic

one-to-one correspondence between propositions and types
in a type system with dependent types

one-to-one correspondence between
- proofs of a certain proposition in constructive predicate logic, and
- terms of the corresponding type

To accommodate equality in predicate logic
introduce the type a == b of proofs that a and b are equal.
Gives a Curry-Howard interpretation of predicate logic with equality.

Can go further and add natural numbers with addition and multiplication
to obtain a Curry-Howard version of Heyting (intuitionistic) arithmetic.

Curry-Howard interpretation was basis for Martin-Löf’s intuitionistic type theory [20,21,22].
- propositions and types are identified
- intended to be a foundational system for constructive mathematics
- can also be used as a programming language [21]
- from 1980’s and onwards, systems implementing variants of Martin-Löf type theory
  - NuPRL [6] : implementing an extensional version of the theory
  - Coq [33]  : implementing an intensional impredicative version
  - Agda      : implements   an intensional predicative extension

About these Notes

Agda's type-checking algorithm
- Section 5
- ideas first presented by Coquand [7]
- more information
  - Norell’s thesis [26]
  - Abel, Coquand, and Dybjer [1,2]
  - Coquand, Kinoshita, Nordström, and Takeyama [8].

more complete understanding of dependent type theory
- read a book about Martin-Löf type theory and related systems
- Martin-Löf’s “Intuitionistic Type Theory” [22] is a classic
  - be aware that it describes an extensional version of the theory
- Nordström, Petersson, and Smith [25]
  description of the later intensional theory on which Agda is based
- variants of dependent type theory are
  - Thompson [34]
  - Constable et al     [6] : NuPRL
  - Bertot and Casteran [4] : Coq
- lecture notes (Agda wiki [3]) by Norell
  - collection of advanced examples of how to use Agda for dependently typed programming
- Geuvers’ lecture notes : introduction to type theory
  - including Barendregt’s pure type systems and important meta-theoretic properties
- Bertot’s notes : how dependent types (in Coq) can be used for
  implementing a number of concepts of programming language theory
  - focus on abstract interpretation
- Barthe, Grégoire, and Riba’s notes
  - method for making termination-checkers

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
- allowing Set : Set, would make the system inconsistent
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

-- EXERCISE: Define some more truth functions, such as conjunction and implication/TODO.

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

infixl 6  _+_  _∸_
infixl 7  _*_

_ : (2 * 3) + 10 PE.≡ 16
_ = PE.refl
{-
+ and * are defined by PRIMITIVE RECURSION in FIRST ARG.
  - PRIMITIVE RECURSIION :
    - upper bound of number of iterations of every loop can be determined before entering loop
    - time complexity of PR functions bounded by the input size
- base case for zero
- step case for non zero defined recursively

Given a first order data type
- distinguish between
  - canonical form
    - built by constructors only
    - e.g., true, false,  zero, succ zero, succ (succ zero), ...
  - non-canonical forms
    - whereas non-canonical elements might contain defined functions
    - e.g., not true, zero + zero, zero * zero, succ (zero + zero)

Martin-Löf [21] instead considers lazy canonical forms
- a term begins with a constructor to be considered a canonical form
- e.g., succ (zero + zero) is a lazy canonical form, but not a “full” canonical form
- Lazy canonical forms are appropriate for lazy functional programming languages (e.g., Haskell)

-- EXERCISE: Write the cut-off subtraction function
-- the function on natural numbers, which returns 0
-- if the second argument is greater than or equal to the first.
-}
_∸_ : Nat -> Nat -> Nat
zero   ∸    _   = zero
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

-- EXERCISE: Also write some more numerical functions like < or <=

_<_ : Nat -> Nat -> Bool
zero   < zero   = false
zero   < succ _ = true
succ _ < zero   = false
succ n < succ m = n < m

_ :  9 < 10 PE.≡ true
_ = PE.refl
_ : 10 < 10 PE.≡ false
_ = PE.refl

_<=_ : Nat -> Nat -> Bool
zero   <= zero   = true
zero   <= succ _ = true
succ _ <= zero   = false
succ n <= succ m = n <= m

_ :  9 <= 10 PE.≡ true
_ = PE.refl
_ : 10 <= 10 PE.≡ true
_ = PE.refl

_>_ : Nat -> Nat -> Bool
zero   > zero   = false
zero   > succ _ = false
succ _ > zero   = true
succ n > succ m = n > m

{-
2.3 Lambda Notation and Polymorphism

Agda is based on the typed lambda calculus.

APPLICATION is juxtaposition

lambda ABSTRACTION written either
- Curry-style (without a type label)
   \x -> e
- Church-style (with a type label)
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
2.4 Implicit Arguments : declared by enclosing typings in curly brackets
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
- important in logic and a precursor to Martin-Löf type theory

recursion restricted to primitive recursion : to make sure programs terminate

Gödel System T
- based on simply typed lambda calculus
- with two base types : truth values and natural numbers
  - (some formulations code truth values as 0 and 1)
- constants for constructors true, false, zero
- conditional and primitive recursion combinators
-}

-- conditional
if_then_else_ : {C : Set} -> Bool -> C -> C -> C
if true  then x else _ = x
if false then _ else y = y

-- primitive recursion combinator for natural numbers
-- natrec is a higher-order function defined by primitive recursion with args
-- - (implicit) return type
-- - c     : element returned in base case
-- - 𝕟→c→c : step function
-- - the Nat on which to perform the recursion
natrec : {C : Set} -> C -> (Nat -> C -> C) -> Nat -> C
natrec c 𝕟→c→c  zero    = c
natrec c 𝕟→c→c (succ n) = 𝕟→c→c n (natrec c 𝕟→c→c n)

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

_ :                                                      succ (succ  zero) plus 2                     PE.≡
                                                         natrec 2 (\x y -> succ y) (succ (succ zero))
_ = PE.refl
_ :                                                      natrec 2 (\x y -> succ y) (succ (succ zero)) PE.≡
    (\x y -> succ y) (succ zero)                        (natrec 2 (\x y -> succ y)       (succ zero))
_ = PE.refl
_ : (\x y -> succ y) (succ zero)                        (natrec 2 (\x y -> succ y)       (succ zero)) PE.≡
    (\  y -> succ y)                                    (natrec 2 (\x y -> succ y)       (succ zero))
_ = PE.refl
_ : (\  y -> succ y)                                    (natrec 2 (\x y -> succ y)       (succ zero)) PE.≡
             succ                                       (natrec 2 (\x y -> succ y)       (succ zero))
_ = PE.refl
_ :          succ                                       (natrec 2 (\x y -> succ y)       (succ zero)) PE.≡
             succ                ((\x y -> succ y) zero (natrec 2 (\x y -> succ y)             zero))
_ = PE.refl
_ :          succ                ((\x y -> succ y) zero (natrec 2 (\x y -> succ y)             zero)) PE.≡
             succ                ((\  y -> succ y)      (natrec 2 (\x y -> succ y)             zero))
_ = PE.refl
_ :          succ                ((\  y -> succ y)      (natrec 2 (\x y -> succ y)             zero)) PE.≡
             succ                (         succ         (natrec 2 (\x y -> succ y)             zero))
_ = PE.refl
_ :          succ                (         succ         (natrec 2 (\x y -> succ y)             zero)) PE.≡
             succ                (         succ                 2                                   )
_ = PE.refl

{-
to stay entirely in Gödel system T
- use terms built up by variables, application, lambda abstraction,
- and constants true, false, zero, succ, if_then_else_, natrec

Gödel system T has unusual property (for a programming language)
- all typable programs terminate

not only do terms in base types Bool and Nat terminate whatever reduction is chosen,
but also terms of function type terminate

reduction rules
- β-reduction
- defining equations for if_then_else and natrec

reductions can be performed anywhere in a term,
so there may be several ways to reduce a term

therefore, Gödel system T is strongly normalising
- any typable term reaches a normal form whatever reduction strategy is chosen

Even in this restrictive Gödel system T, possible to define many numerical functions.
Can define all primitive recursive functions
(in the usual sense without higher-order functions).

Can also define functions which are not primitive recursive, such as the Ackermann function.

Gödel system T is important in the history of ideas that led to
the Curry-Howard isomorphism and Martin-Löf type theory.

Gödel system T
- is the simply typed kernel of Martin-Löf’s constructive type theory,
Martin-Löf type theory
- is the foundational system from which Agda grew

relationship between Agda and Martin-Löf type theory is much like the
relationship between Haskell and the simply typed lambda calculus.
Or compare it with the relationship between Haskell and Plotkin’s PCF [30].

Like Gödel system T,
PCF is based on the simply typed lambda calculus with truth values and natural numbers.
But PCF includes a fixed point combinator
- used for encoding arbitrary general recursive definitions
  - therefore can define non-terminating functions in PCF.
-}

-- Exercise/TODO: Define all functions previously given in the text in Gödel System T.

-- induction principle for booleans (from Mimram)
Bool-rec : (P : Bool → Set) → P false → P true → (b : Bool) → P b
Bool-rec P Pf Pt false = Pf
Bool-rec P Pf Pt true  = Pt

not' : Bool -> Bool
not' b = Bool-rec (λ bool → Bool) true false b

_ : not' false PE.≡ not false
_ = PE.refl
_ : not' true  PE.≡ not true
_ = PE.refl

equiv' : Bool -> Bool -> Bool
equiv' b false = Bool-rec (λ bool → Bool) true false b
equiv' b true  = Bool-rec (λ bool → Bool) false true b

_ : equiv' false false PE.≡ equiv false false
_ = PE.refl
_ : equiv'  true false PE.≡ equiv  true false
_ = PE.refl
_ : equiv' false  true PE.≡ equiv false  true
_ = PE.refl
_ : equiv'  true  true PE.≡ equiv  true  true
_ = PE.refl

_||'_ : Bool -> Bool -> Bool
b ||' false = Bool-rec (λ bool → Bool)      b  b b
b ||'  true = Bool-rec (λ bool → Bool) (not b) b b

_ : false ||' false PE.≡ false || false
_ = PE.refl
_ : false ||'  true PE.≡ false ||  true
_ = PE.refl
_ :  true ||' false PE.≡  true || false
_ = PE.refl
_ :  true ||'  true PE.≡  true ||  true
_ = PE.refl

_&&'_ : Bool -> Bool -> Bool
b &&' false = Bool-rec (λ bool → Bool) false false b
b &&'  true = Bool-rec (λ bool → Bool) b     b     b

_ : false &&' false PE.≡ false && false
_ = PE.refl
_ : false &&'  true PE.≡ false &&  true
_ = PE.refl
_ :  true &&' false PE.≡  true && false
_ = PE.refl
_ :  true &&'  true PE.≡  true &&  true
_ = PE.refl

pred' : Nat -> Nat
pred'  zero    = natrec zero (\x y -> zero) zero
pred' (succ n) = natrec zero (\_ _ -> n)    n
-- test after List def

{-
2.6 Parametrised Types

Haskell parametrised types : [a]
-}
data List (A : Set) : Set where
  []   :                List A
  _::_ : A -> List A -> List A
infixr 5 _::_
{-# BUILTIN LIST List #-}
{-
(A : Set) to the left of the colon
- tells Agda that A is a parameter
- and it becomes an implicit argument to the constructors:
  []   : {A : Set} -> List A
  _::_ : {A : Set} -> A -> List A -> List A

def only allows defining lists with elements in arbitrary SMALL types
- cannot define lists of sets using this definition, since sets form a large type
-}

-- test for pred'
_ : (pred' 0 :: pred' 1 :: pred' 2 :: pred' 101 :: []) PE.≡ (pred 0 :: pred 1 :: pred 2 :: pred 101 :: [])
_ = PE.refl


mapL : {A B : Set} -> (A -> B) -> List A -> List B
mapL f       []  = []
mapL f (x :: xs) = f x :: mapL f xs

_ : mapL succ (1 :: 2 :: []) PE.≡ (2 :: 3 :: [])
_ = PE.refl

-- Exercise: Define some more list combinators like for example foldl or filter.
filter : {A : Set} -> (A -> Bool) -> List A -> List A
filter f       []  = []
filter f (x :: xs) = let tail = filter f xs in if f x then x :: tail else tail

_ : filter (_< 4) (5 :: 4 :: 3 :: 2 :: 1 :: []) PE.≡ (3 :: 2 :: 1 :: [])
_ = PE.refl

foldl : {A B : Set} -> (B -> A -> B) -> B -> List A -> B
foldl _ b       []  = b
foldl f b (a :: as) = foldl f (f b a) as

foldr : {A B : Set} -> (A -> B -> B) -> B -> List A -> B
foldr f b       []  = b
foldr f b (a :: as) = f a (foldr f b as)

_ : foldl _+_ 0 (3 :: 2 :: 1 :: []) PE.≡ 6
_ = PE.refl

_ : foldl _∸_ 0 (3 :: 1 :: []) PE.≡ 0
_ = PE.refl

_ : foldr _∸_ 0 (3 :: 1 :: []) PE.≡ 2
_ = PE.refl

-- Exercise: define list recursion combinator listrec (like natrec, but for lists)
-- from Mimram
List-rec
  : {A : Set}
  → (P : List A → Set)
  → P []
  → ((x : A) → (xs : List A) → P xs → P (x :: xs))
  → (xs : List A)
  → P xs
List-rec P Pe Pc       []  = Pe
List-rec P Pe Pc (x :: xs) = Pc x xs (List-rec P Pe Pc xs)

mapL' : {A B : Set} -> (A -> B) -> List A -> List B
mapL' {_} {B} f xs = List-rec (λ _ → List B) [] (λ a _ bs → f a :: bs) xs

_ : mapL' succ (1 :: 2 :: []) PE.≡ mapL succ (1 :: 2 :: [])
_ = PE.refl

filter' : {A : Set} -> (A -> Bool) -> List A -> List A
filter' {A} f xs = List-rec (λ _ → List A) [] (λ a _ as' → if f a then a :: as' else as') xs

_ : filter' (_< 4) (5 :: 4 :: 3 :: 2 :: 1 :: []) PE.≡ filter (_< 4) (5 :: 4 :: 3 :: 2 :: 1 :: [])
_ = PE.refl

foldl' : {A B : Set} -> (B -> A -> B) -> B -> List A -> B
foldl' {_} {B} f b as = List-rec (λ _ → B) b (λ a _ b → f b a) as

_ : foldl' _+_ 0 (3 :: 2 :: 1 :: []) PE.≡ foldl _+_ 0 (3 :: 2 :: 1 :: [])
_ = PE.refl

foldr' : {A B : Set} -> (A -> B -> B) -> B -> List A -> B
foldr' {A} {B} f b as = List-rec (λ _ → B) b (λ a as b → f a b) as

_ : foldr' _∸_ 0 (3 :: 1 :: []) PE.≡ foldr _∸_ 0 (3 :: 1 :: [])
_ = PE.refl

-- binary Cartesian product AKA pair
data _X_ (A B : Set) : Set where
  <_,_> : A -> B -> A X B

-- prime because these are never used but are redeinfed later
fst' : {A B : Set} -> A X B -> A
fst' < a , b > = a

snd' : {A B : Set} -> A X B -> B
snd' < a , b > = b

-- Usually want to zip lists of equal length.
-- Can fix that with runtime Maybe or compiletime dependent types (later).
zipL : {A B : Set} -> List A -> List B -> List (A X B)
zipL       []         _  = []
zipL (_ ::  _)       []  = []
zipL (x :: xs) (y :: ys) = < x , y > :: zipL xs ys

-- Exercise: Define the sum A + B of two small types with constructors: inl, inr.
data Either (A B : Set) : Set where
  inl : A -> Either A B
  inr : B -> Either A B

-- Exercise: Define a function from A + B to a small type C by cases.
toC : {A B C : Set} → (A -> C) -> (B -> C) -> (Either A B) -> C
toC ac  _ (inl a) = ac a
toC  _ bc (inr b) = bc b

{-
2.7 Termination-checking

General recursion allowed in most languages.
Therefore possible to define "partial" functions (e.g., divide by 0)

How to ensure functions terminate?
- one solution : restrict recursion to primitive recursion (like in Gödel system T)
  - the approach taken in Martin-Löf type theory
    where primitive recursion as a kind of structural recursion on well-founded data types
    (see Martin-Löf’s book [22] and Dybjer’s schema for inductive definitions [10])
  - but structural recursion (in one argument at a time) is often inconvenient

Agda’s termination-checker
- generalisation of primitive recursion which is practically useful
- enables doing pattern matching on several arguments simultaneously
- and having recursive calls to structurally smaller arguments

But repeated subtraction is rejected by Agda termination-checker although it is terminating.
Because Agda does not recognise the recursive call to (m - n) as structurally smaller.
Because subtraction is not a constructor for natural numbers.
Further reasoning required to deduce the recursive call is on a smaller argument.

Section 7 : describes how partial and general recursive functions can be represented in Agda.
Idea is to replace a partial function by a total function with an extra argument:
a proof that the function terminates on its arguments.

------------------------------------------------------------------------------
3 Dependent Types

3.1 Vectors of a Given Length

2 ways to define
- Recursive family
  - by primitive recursion/induction on n
- Inductive family
  - as a family of data types by declaring its constructors and the types of constructors
  - like def of List, except length info included in types

paper mostly uses inductive families

--------------------------------------------------
Vectors as a Recursive Family

In math : define vectors by induction on n:
  A   0 = 1
  A n+1 = A × An
In Agda (and Martin-Löf type theory):
-}
-- type with only one element
data Unit : Set where
  <> : Unit

VecR : Set -> Nat -> Set
VecR A  zero    = Unit
VecR A (succ n) = A X VecR A n

{-
Up till now, only used primitive recursion for defining functions
where the range is in a given set (in a given small type).
Here, primitive recursion used for defining a family of sets
- a family of elements in a given large type.
-}

zipR : {A B : Set} {n : Nat} -> VecR A n -> VecR B n -> VecR (A X B) n
zipR {n =  zero}          _          _   = <>
zipR {n = succ n'} < a , as > < b , bs > = < < a , b > , zipR {n = n'} as bs >

{-
base case : type-correct since the right hand side has type
            VecR (A X B) zero     = <>
step case : type-correct since the right hand side has type
            VecR (A X B) (succ n) = (A X B) X (VecR (A X B) n)
-}

-- Exercise. Write the functions head, tail, and map for the recursive vectors.

headR : {A : Set} {n : Nat} -> VecR A (succ n) ->      A
headR < x , _ > = x

tailR : {A : Set} {n : Nat} -> VecR A (succ n) -> VecR A n
tailR < _ ,  t > = t

mapR : {A B : Set} {n : Nat} -> (A -> B) -> VecR A n -> VecR B n
mapR {n = zero}    _        _   = <>
mapR {n = succ n'} f < a , as > = < f a , mapR {n = n'} f as >

_ : mapR succ < 1 , < 2 , < 0 , <> > > > PE.≡ < 2 , < 3 , < 1 , <> > > >
_ = PE.refl

--------------------------------------------------
-- Vectors as an Inductive Family

data Vec (A : Set) : Nat -> Set where
  []   :                              Vec A  zero
  _::_ : {n : Nat} -> A -> Vec A n -> Vec A (succ n)

{-
this time no induction on length
instead : constructors generate vectors of different lengths
[] : length 0
:: : length (n + 1)

Definition style called an inductive family, or an inductively defined family of sets.

Terminology comes from constructive type theory,
where data types such as Nat and (List A) are called inductive types.

Remark: Beware of terminological confusion:
- In programming languages one talks about recursive types for such data
  types defined by declaring the constructors with their types.
- May be confusing since the word recursive family was used for a different notion.
- Reason for terminological distinction between data types in ordinary functional languages,
  and data types in languages where all programs terminate.
- In terminating languages, there will not be any non-terminating numbers or non-terminating lists.
- The set-theoretic meaning of such types is therefore simple:
  - build the set inductively generated by the constructors, see [9] for details.
- In non-terminating language, the semantic domains are more complex.
  - One typically considers various kinds of Scott domains which are complete partially orders.

(Vec A n) has two args:
- small type A of the elements
  - A is a parameter in the sense that it remains the same throughout the definition:
    for a given A, define the family Vec A : Nat -> Set
- length n of type Nat.
  - n is not a parameter since it varies in the types of the constructors

Non-parameters are often called INDICES
- say that Vec A is an inductive family indexed by natural numbers

In Agda data type definitions
- parameters are placed to the left of the colon
  and become implicit arguments to the constructors
- indices are placed to the right
-}

zip : {A B : Set} {n : Nat} -> Vec A n -> Vec B n -> Vec (A X B) n
zip {n = zero}          []        []  = []
zip {n = succ n'} (x :: xs) (y :: ys) = < x , y > :: zip {n = n'} xs ys

head : {A : Set} {n : Nat} -> Vec A (succ n) -> A
head (x :: _) = x

tail : {A : Set} {n : Nat} -> Vec A (succ n) -> Vec A n
tail (_ :: xs) = xs

map : {A B : Set} {n : Nat} -> (A -> B) -> Vec A n -> Vec B n
map f [] = []
map f (x :: xs) = f x :: map f xs

--------------------------------------------------
-- 3.2 Finite Sets

data Fin : Nat -> Set where
  fzero : {n : Nat} ->          Fin (succ n)
  fsucc : {n : Nat} -> Fin n -> Fin (succ n)

{-
for each n, Fin n contains n elements
e.g., Fin 3 contains fzero, fsucc fzero and fsucc (fsucc fzero)

useful to access element in a vector
- for Vec A n
- position given by (Fin n)
- ensures accessing an element inside the vector

For empty vector n is zero.
So would use (Fin 0) --- which has no elements.
There is no such case.
Expressed in Agda via "absurd" pattern '()'
-}

_!_ : {A : Set} {n : Nat} -> Vec A n -> Fin n -> A
[]        ! ()
(x ::  _) ! fzero   = x
(_ :: xs) ! fsucc i = xs ! i

-- Exercise: Rewrite the function !! so that it has the following type:
-- This eliminates the empty vector case, but other cases are needed.

_!!_  : {A : Set} {n : Nat} -> Vec A (succ n) -> Fin (succ n) -> A
_!!_  {_}   {zero} (a ::  f_)  fzero    = a
_!!_  {_} {succ n} (a ::  _)  fzero    = a
_!!_  {_} {succ n} (_ :: as) (fsucc f) = _!!_  {n = n} as f

_!!'_ : {A : Set} {n : Nat} -> Vec A (succ n) -> Fin (succ n) -> A
_!!'_              (a ::  _)  fzero    = a
_!!'_ {_} {succ n} (_ :: as) (fsucc f) = _!!'_ {n = n} as f

_ : (0 :: 1 :: 2 :: []) !   fsucc fzero PE.≡ 1
_ = PE.refl
_ : (0 :: 1 :: 2 :: []) !!  fsucc fzero PE.≡ 1
_ = PE.refl
_ : (0 :: 1 :: 2 :: []) !!' fsucc fzero PE.≡ 1
_ = PE.refl

-- Exercise: Give an alternative definition of Fin as a recursive family.

-- from Conal Eliot https://raw.githubusercontent.com/conal/agda-play/main/DependentTypesAtWork/RecVec.agda
FinR : Nat -> Set
FinR  zero    = DEP.⊥
FinR (succ n) = Maybe.Maybe (FinR n)

f5 : FinR 5
f5 = Maybe.nothing
f5' : FinR 5 PE.≡ Maybe.Maybe (Maybe.Maybe (Maybe.Maybe (Maybe.Maybe (Maybe.Maybe DEP.⊥))))
f5' = PE.refl
f4 : FinR 4
f4 = Maybe.nothing
f3 : FinR 3
f3 = Maybe.nothing
f2 : FinR 2
f2 = Maybe.nothing
f1 : FinR 1
f1 = Maybe.nothing
f1' : FinR 1 PE.≡ Maybe.Maybe DEP.⊥
f1' = PE.refl
--f0 : FinR 0
--f0 = {!!} -- no solution found
f0' : FinR 0 PE.≡ DEP.⊥
f0' = PE.refl

_!'_ : {A : Set} {n : Nat} → VecR A n → FinR n → A
_!'_  {n = succ m} < a ,  _ >  Maybe.nothing    = a
_!'_  {n = succ m} < _ , as > (Maybe.just finm) = as !' finm

_    : < 0 , < 1 , < 2 , <> > > >  !'                                    Maybe.nothing   PE.≡ 0
_    = PE.refl
_    : < 0 , < 1 , < 2 , <> > > >  !'                         Maybe.just Maybe.nothing   PE.≡ 1
_    = PE.refl
_    : < 0 , < 1 , < 2 , <> > > >  !'             Maybe.just (Maybe.just Maybe.nothing)  PE.≡ 2
_    = PE.refl
-- (Maybe.Maybe _A_645) !=< DEP.⊥
-- _ : < 0 , < 1 , < 2 , <> > > >  !' Maybe.just (Maybe.just (Maybe.just Maybe.nothing)) PE.≡ 100
-- _ = PE.refl

--------------------------------------------------
-- 3.3 More Inductive Families

-- binary trees of a certain height
-- any given (t : DBTree A n) is a perfectly balanced tree
-- with 2n elements and information in the leaves
data DBTree (A : Set) : Nat -> Set where
  leaf :                     A                 -> DBTree A zero
  node : {n : Nat} -> DBTree A n -> DBTree A n -> DBTree A (succ n)

l1       : DBTree Nat 0
l1       = leaf 1
l2       : DBTree Nat 0
l2       = leaf 2
n1-2     : DBTree Nat 1
n1-2     = node l1 l2
l3       : DBTree Nat 0
l3       = leaf 3
l4       : DBTree Nat 0
l4       = leaf 4
n3-4     : DBTree Nat 1
n3-4     = node l3 l4
n1-2-3-4 : DBTree Nat 2
n1-2-3-4 = node n1-2 n3-4

-- Exercise/TODO: Modify DBTree def to define the height balanced binary trees
-- i.e., binary trees where difference between heights of left and right subtree is at most one.

-- Exercise/TODO: Define lambda terms as an inductive family
-- indexed by the maximal number of free variables allowed in the term.

-- Exercise/TODO: define typed lambda terms as an inductive family indexed by the type of the term.

{-
------------------------------------------------------------------------------
Curry, 1930’s : one-to-one correspondence between propositions in propositional logic and types.
1960’s de Bruijn and Howard introduced dependent types to extend Curry’s correspondence to predicate logic.
Scott [31] and Martin-Löf [20], correspondence became building block of foundation for constructive math:
- Martin-Löf’s intuitionistic type theory.

Will now show how intuitionistic predicate logic with equality is a subsystem of
Martin-Löf type theory by realising it as a theory in Agda.

4.1 Propositional Logic

isomorphism : each proposition is interpreted as the set of its proofs.
To emphazie PROOFS here are FIRST-CLASS mathematical object, say PROOF OBJECTS.
aka : CONSTRUCTIONS

proposition
- is true IFF its set of proofs is inhabited
- it is false iff its set of proofs is empty
-}

-- CONJUNCTION (aka AND)
-- A and B are two propositions represented by their sets of proofs.
-- A & B is also a set of proofs, representing the conjunction of A and B.
-- Note : conjunction is same as Cartesian product of two sets.
-- applying & constructor is &-introduction
data _&_ (A B : Set) : Set where
  <_,_> : A -> B -> A & B

-- alternate def
_&'_ : Set -> Set -> Set
A &' B = A X B

-- two rules of &-elimination
-- rules state that if A & B is true then A and B are also true
fst : {A B : Set} -> A & B -> A
fst < a , _ > = a

snd : {A B : Set} -> A & B -> B
snd < _ , b > = b

sndP : {A B : Set} -> A &' B -> B
sndP < _ , b > = b

-- DISJUNCTION (aka OR)
-- disjunction corresponds to disjoint union
data _\/_ (A B : Set) : Set where
  inl : A -> A \/ B
  inr : B -> A \/ B

-- alternate def
_\/'_ : Set -> Set -> Set
A \/' B = Either A B

-- rule of \/-elimination is case analysis for a disjoint union
case : {A B C : Set} -> A \/ B -> (A -> C) -> (B -> C) -> C
case (inl a) d e = d a
case (inr b) d e = e b

-- proposition which is always true
-- corresponds to the unit set (see page 14) according to Curry-Howard:
data True : Set where
  <> : True

-- proposition False is the proposition that is false by definition
-- corresponds to the the empty set according to Curry-Howard.
-- This is the set which is defined by stating that it has no canonical elements.
-- sometimes referred as the “absurdity” set and denoted by ⊥
data False : Set where

-- rule of ⊥-elimination states that if False has been proved, then can prove any proposition A.
-- Can only happen by starting with contradictory assumptions.
nocase : {A : Set} -> False -> A
nocase () -- tells agda no cases to consider
{-
Justification of this rule is same as justification of existence of
an empty function from the empty set into an arbitrary set.
Since the empty set has no elements there is nothing to define; it is definition by no cases.
Recall the explanation on page 16 when we used the notation ().
-}

-- In constructive logic
-- - proving negation of a proposition is the same as
-- - proving the proposition leads to absurdity:
Not : Set -> Set
Not A = A -> False

{-
BHK-interpretation says an implication is proved by providing a method for transforming
a proof of A into a proof of B.

When Brouwer pioneered this 100 years ago, there were no computers and no models of computation.
In contemporary constructive mathematics in general, and in Martin-Löf type theory in particular,
a “method” is usually understood as a computable function (or computer program) which transforms proofs.
Implication is defined as function space.
Emphasized via new notation:
-}

_==>_ : (A B : Set) -> Set
A ==> B = A -> B

_ : False ==> False -- T
_ = λ ()
_ : False ==> True  -- T
_ = λ ()
_ : True  ==> False -- F
_ = λ _ → {!!}
_ : True  ==> True  -- T
_ = λ _ → <>

{-
Above def not accepted in Martin-Löf’s version of propositions-as-sets.
Because each proposition should be defined by stating what its canonical proofs are.
A canonical proof should always begin with a constructor.
A function in A -> B does not, unless one considers the lambda-sign as a constructor.

Instead, Martin-Löf defines implication as a set with one constructor:
Using this, a canonical proof of A ==>' B always begins with the constructor fun.
-}
data _==>'_ (A B : Set) : Set where
  fun : (A -> B) -> A ==>' B

-- rule of ==>-elimination (modus ponens)
apply : {A B : Set} -> A ==>' B -> A -> B
apply (fun f) a = f a

-- equivalence of propositions
_<==>_ : Set -> Set -> Set
A <==> B = (A ==> B) & (B ==> A)

_ : False <==> False
_ = < ( λ () ) , ( λ () ) >
_ : False <==> True
_ = < (λ () ) , (λ { <> → {!!}}) >
_ : True  <==> False
_ = < (λ { <> → {!!}}) , (λ () ) >
_ : True  <==> True
_ = < (λ _ → <>) , (λ _ → <>) >

{-
Exercise/TODO: Prove your favourite tautology from propositional logic.
Beware that you will not be able to prove the law of the excluded middle A \/ Not A.
This is a consequence of the definition of disjunction, can you explain why?.
The law of the excluded middle is not available in intuitonistic logic, only in classical logic.
-}

le : ∀ {P Q : Set} → (P ==> Q) <==> (Not P \/ Q)
le {P} {Q} = < (λ {P==>Q → {!!} }) , (λ { (inl NotP) p → {!!} ; (inr q) p → q}) >
