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
- e.g., [A]
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

Agda is based on the typed lambda calculus.

APPLICATION is juxtaposition

lambda ABSTRACTION is either written Curry-style without a type label on the argument x
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

-- Exercise/TODO: Define all functions previously given in the text in Gödel System T.

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
- cannot define lists of sets using this definition, since sets form a large type.
-}

mapL : {A B : Set} -> (A -> B) -> List A -> List B
mapL f       []  = []
mapL f (x :: xs) = f x :: mapL f xs

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

-- Exercise/TODO: define list recursion combinator listrec (like natrec, but for lists)

-- binary Cartesian product AKA pair
data _X_ (A B : Set) : Set where
  <_,_> : A -> B -> A X B

fst : {A B : Set} -> A X B -> A
fst < a , b > = a

snd : {A B : Set} -> A X B -> B
snd < a , b > = b

-- Usually want to zip lists of equal length.
-- Can fix that with runtime Maybe or compiletime dependent types (later).
zipL : {A B : Set} -> List A -> List B -> List (A X B)
zipL       []         _  = []
zipL (_ ::  _)       []  = []
zipL (x :: xs) (y :: ys) = < x , y > :: zipL xs ys

{-
Exercise: Define the sum A + B of two small types with constructors: inl, inr.
Exercise: Define a function from A + B to a small type C by cases.
-}

data Either (A B : Set) : Set where
  inl : A -> Either A B
  inr : B -> Either A B

toC : {A B C : Set} → (A -> C) -> (B -> C) -> (Either A B) -> C
toC ac  _ (inl a) = ac a
toC  _ bc (inr b) = bc b

{-
2.7 Termination-checking

General recursion is allowed in most languages.
Therefore it is possible to define "partial" functions (e.g., divide by 0)

How to ensure functions terminate?
- one solution : restrict recursion to primitive recursion (like in Gödel system T)
  - the approach taken in Martin-Löf type theory
    where primitive recursion as a kind of structural recursion on well-founded data types
    (see Martin-Löf’s book [22] and Dybjer’s schema for inductive definitions [10])
  - structural recursion (in one argument at a time) is often inconvenient

Agda’s termination-checker
- generalisation of primitive recursion which is practically useful
- enables doing pattern matching on several arguments simultaneously
- and having recursive calls to structurally smaller arguments

But Agda Repeated subtraction is rejected by the termination-checker
although it is actually terminating.  The reason it does not recognise
the recursive call to (m - n) as structurally smaller.  Because
subtraction is not a constructor for natural numbers.  Further
reasoning is required to deduce that the recursive call is actually on
a smaller argument.

Section 7 : describes how partial and general recursive functions can be represented in Agda.
Idea is to replace a partial function by a total function with an extra argument:
a proof that the function terminates on its arguments.

------------------------------------------------------------------------------
3 Dependent Types

3.1 Vectors of a Given Length

2 ways to define
- Recursive family: by induction on n (i.e., by primitive recursion on n)
- Inductive family: as a family of data types by declaring its constructors and their types
  - like def of List, except length info included in types

Paper mostly uses inductive families.

Vectors as a Recursive Family.

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

zipR : {A B : Set} -> (n : Nat) -> VecR A n -> VecR B n -> VecR (A X B) n
zipR  zero          v         w   = <>
zipR (succ n) < a , v > < b , w > = < < a , b > , zipR n v w >

{-
base case : type-correct since the right hand side has type VecR (A X B) zero
            which is defined as the type Unit
step case : type-correct since the right hand side has type
            VecR (A X B) (succ n) = (A X B) X (VecR (A X B) n),
-}

-- Exercise. Write the functions head, tail, and map for the recursive vectors.

headR : {A : Set} {n : Nat} -> VecR A (succ n) -> A
headR < x , _ > = x

mapR : {A B : Set} -> (n : Nat) -> (A -> B) -> VecR A n -> VecR B n
mapR  zero    _        _   = <>
mapR (succ n) f < a , as > = < f a , mapR n f as >

-- Vectors as an Inductive Family.

data Vec (A : Set) : Nat -> Set where
  [] : Vec A zero
  _::_ : {n : Nat} -> A -> Vec A n -> Vec A (succ n)

{-
this time no induction on length
instead give constructors which generate vectors of different lengths
[] : length 0
:: : length (n + 1)

definition style called an inductive family, or an inductively defined family of sets.
Terminology comes from constructive type theory,
where data types such as Nat and (List A) are called inductive types.

Remark: Beware of terminological confusion. As we have mentioned before, in
programming languages one instead talks about recursive types for such data
types defined by declaring the constructors with their types. This may be a bit
confusing since we used the word recursive family for a different notion. There is
a reason for the terminological distinction between data types in ordinary func-
tional languages, and data types in languages where all programs terminate. In
the latter, we will not have any non-terminating numbers or non-terminating
lists. The set-theoretic meaning of such types is therefore simple: just build the
set inductively generated by the constructors, see [9] for details. In a language
with non-terminating programs, however, the semantic domains are more com-
plex. One typically considers various kinds of Scott domains which are complete
partially orders.

(Vec A n) has two args:
- small type A of the elements
  - A is a parameter in the sense that it remains the same throughout the definition:
    for a given A we define the family Vec A : Nat -> Set.
- length n of type Nat.
  - n is not a parameter since it varies in the types of the constructors.

Non-parameters are often called indices and we can say that Vec A is an inductive family
indexed by the natural numbers.

In Agda data type definitions
- parameters are placed to the left of the colon
  and become implicit arguments to the constructors
- indices are placed to the right
-}

zip : {A B : Set} -> (n : Nat) -> Vec A n -> Vec B n -> Vec (A X B) n
zip  zero          []        []  = []
zip (succ n) (x :: xs) (y :: ys) = < x , y > :: zip n xs ys

head : {A : Set} {n : Nat} -> Vec A (succ n) -> A
head (x :: _) = x

tail : {A : Set} {n : Nat} -> Vec A (succ n) -> Vec A n
tail (_ :: xs) = xs

map : {A B : Set} {n : Nat} -> (A -> B) -> Vec A n -> Vec B n
map f [] = []
map f (x :: xs) = f x :: map f xs

-- 3.2 Finite Sets

data Fin : Nat -> Set where
  fzero : {n : Nat} ->          Fin (succ n)
  fsucc : {n : Nat} -> Fin n -> Fin (succ n)

{-
for each n, Fin n contains n elements
e.g., Fin 3 contains fzero, fsucc fzero and fsucc (fsucc fzero

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

-- Exercise/TODO: Rewrite the function !! so that it has the following type:
-- This eliminates the empty vector case, but other cases are needed.

_!!_ : {A : Set} {n : Nat} -> Vec A (succ n) -> Fin (succ n) -> A
_!!_ {_}   {zero} (a ::  _) fzero = a
_!!_ {_} {succ n} (a ::  _) fzero = a
_!!_ {_} {succ n} (_ :: as) (fsucc f) = _!!_ {n = n} as f

_!!'_ : {A : Set} {n : Nat} -> Vec A (succ n) -> Fin (succ n) -> A
_!!'_              (a ::  _)  fzero    = a
_!!'_ {_} {succ n} (_ :: as) (fsucc f) = _!!'_ {n = n} as f

-- Exercise/TODO: Give an alternative definition of Fin as a recursive family.

-- 3.3 More Inductive Families

-- binary trees of a certain height:
data DBTree (A : Set) : Nat -> Set where
  dlf :                     A                 -> DBTree A zero
  dnd : {n : Nat} -> DBTree A n -> DBTree A n -> DBTree A (succ n)
-- With this definition, any given (t : DBTree A n) is a perfectly balanced tree
-- with 2n elements and information in the leaves.

