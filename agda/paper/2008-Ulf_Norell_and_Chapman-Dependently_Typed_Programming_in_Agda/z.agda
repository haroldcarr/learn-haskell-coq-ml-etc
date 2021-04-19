module z where

-- http://www.cse.chalmers.se/~ulfn/papers/afp08/tutorial.pdf
{-
Dependently Typed Programming in Agda
Ulf Norell1   : Chalmers University, Gothenburg   : ulfn@chalmers.se
James Chapman : Institute of Cybernetics, Tallinn : james@cs.ioc.ee

------------------------------------------------------------------------------
1 Introduction

In Hindley-Milner languages (Haskell, ML) : separation between types and values.

Dependently typed : types can contain (depend on) arbitrary values and
                    appear as arguments and results of ordinary functions

Dependent types enables types to talk about values,
so can encode properties of values as types
whose elements are proofs that the property is true.

So dependently typed programming language can be used as a logic.

To be consistent programs must be total.

------------------------------------------------------------------------------
2 BASICS

based on intuitionistic type theory[4]

--------------------------------------------------
2.1 DATATYPES AND PATTERN MATCHING
-}

data Bool : Set where
  true  : Bool
  false : Bool

{-
type of Bool is
- Set, the type of small types
- There is hierarchy of increasingly large types
- type of Set is Set1, whose type is Set2, ...
-}

not : Bool → Bool
not true  = false
not false = true

data Nat : Set where
 zero :       Nat
 suc  : Nat → Nat
{-# BUILTIN NATURAL Nat #-}

_+_ : Nat → Nat → Nat
zero  + m =          m
suc n + m = suc (n + m)
infixl 40 _+_

_*_ : Nat → Nat → Nat
zero  * m = zero
suc n * m = m + n * m
infixl 60 _*_

{-
functions must not crash and must terminate

recursive calls have to be made on structurally smaller arguments

mixfix via  _ : args where the underscores are
-}

_or_ : Bool → Bool → Bool
false or x = x
true  or _ = true -- uses wildcard pattern on "don't care" args
infixr 20 _or_

-- polymorphic function over type A
if_then_else_ : {A : Set} → Bool → A → A → A
if  true then x else y = x
if false then x else y = y
infix 5 if_then_else_

-- types parameterize by other types
data List (A : Set) : Set where
  []   :                List A
  _::_ : A → List A → List A
infixr 40 _::_

-- liberal naming policy enables naming above
data _⋆ (α : Set) : Set where
  ε : α ⋆
  _C_ : α → α ⋆ → α ⋆

{-
whitespace important

--------------------------------------------------
2.2 DEPENDENT FUNCTIONS

result type depends on VALUE of an arg
  (x : A) → B for the
type of functions
- given   : arg x of type A
- returns : result of type B
  - where x may appear in B

special case is when x itself is a type, e.g.,
-}

identity : (A : Set) → A → A
identity A x = x

zero’ : Nat
zero’ = identity Nat zero

{-
identity is a dependent function
- given
  - a type argument A and
  - an element of A
- returns
  - the element

This is how polymorphic functions are encoded in Agda.

example : function which takes a dependent function and applies it to an argument:
-}

apply : (A : Set) (B : A → Set) → ((x : A) → B x) → (a : A) → B a
apply A B f a = f a

{-
shorthand for dependent function types:

        (x : A)   (y : B) → C
for     (x : A) → (y : B) → C

        (x y : A)       → B
for     (x : A) (y : A) → B


elements of dependent function types are lambda terms which may carry explicit type info

alternative ways to identity
identity function above are:
-}

identity2 : (A : Set) → A → A
identity2 = \A x → x

identity3 : (A : Set) → A → A
identity3 = \(A : Set) (x : A) → x

identity4 : (A : Set) → A → A
identity4 = \(A : Set) x → x

{-
--------------------------------------------------
2.3 IMPLICIT ARGUMENTS

Previous section showed how dependent functions taking types as
arguments used to model polymorphic types.

But it is not necessary to say at which point it is applied - that can be inferred.

The identity functions above explicitly provide the type argument.

Avoid explicit type arg via implicit arguments using {}
-}

id : {A : Set} → A → A
id x = x

true’ : Bool
true’ = id true

{-
No restrictions on what arguments can be made implicit.
Not guarantees that an implicit argument can be inferred.

E.g., make 2nd arg of identity function implicit as well:
-}

silly : {A : Set} {x : A} → A -- but type checker cannot figure out 2nd arg
silly {_} {x} = x

false’ : Bool
false’ = silly {x = false} -- must provide via named implicit application syntax f {v}
                           -- must use name if not in order
-- type checker fills in term
-- will not search, only looks at typing constraints and performs unification
one : Nat
one = identity _ (suc zero)

-- example of inferring the typing constraints
_◦_ : {A : Set}{B : A → Set}{C : (x : A) → B x → Set}
      (f : {x : A}(y : B x) → C x y)(g : (x : A) → B x)
      (x : A) → C x (g x)
(f ◦ g) x = f (g x)

plus-two = suc ◦ suc

-- other functions

map : {A B : Set} → (A → B) → List A → List B
map f       []  = []
map f (x :: xs) = f x :: map f xs

_++_ : {A : Set} → List A → List A → List A
[]        ++ ys =             ys
(x :: xs) ++ ys = x :: (xs ++ ys)

{-
--------------------------------------------------
-- 2.4 DATATYPE FAMILIES

So far, only used dependent types is to represent polymorphism
-}

data Vec (A : Set) : Nat → Set where
  []   :                           Vec A  zero
  _::_ : {n : Nat} → A → Vec A n → Vec A (suc n)

{-
type of Vec A is Nat → Set
- means that Vec A is a family of types indexed by natural numbers
- for each Nat n, Vec A n is a type
- []   constructs an element in Vec A zero
- _::_ constructs an element in Vec A (suc n) for some n

distinction between parameters and indices of a datatype
- parameterised by type A
- indexed over Nat

type of _::_ is a dependent function type
- 1st arg is an implicit Nat n
- returns type that depends on n 'suc n'

same constructor names for Vec as for List
-  constructor names are not required to be distinct between different datatypes
-}

-- safe head
head : {A : Set} {n : Nat} → Vec A (suc n) →     A
head (x :: xs) = x

tail : {A : Set} {n : Nat} → Vec A (suc n) → Vec A n
tail (x :: xs) = xs

{-
rule for when to include a particular case is:
  if it is type correct you have to include it
-}

--------------------------------------------------
-- DOT PATTERNS : indicate value of an arg deduced by type checking, not pattern matching

vmap : {A B : Set} {n : Nat} → (A → B) → Vec A n → Vec B n
vmap f       []  = []
vmap f (x :: xs) = f x :: vmap f xs

{-
map on Vec is exactly the same map on List
only change is the type

behind the scenes, what happens with the length argument when pattern matching

to see, define new versions of Vec and vmap with fewer implicit arguments:
-}

data Vec2 (A : Set) : Nat → Set where
  nil  :                            Vec2 A  zero
  cons : (n : Nat) → A → Vec2 A n → Vec2 A (suc n)

vmap2 : {A B : Set} (n : Nat) → (A → B) → Vec2 A n → Vec2 B n
vmap2 .zero    f  nil          = nil
vmap2 .(suc n) f (cons n x xs) = cons n (f x) (vmap2 n f xs)

{-
pattern matching on list argument reveals its length
- if nil  then zero
- if cons then suc n

to indicate value of an arg deduced by type checking,
rather than observed by pattern matching,
it is prefixed by a dot (.)

could choose to define vmap by first pattern matching on the length rather than on the list
-}

vmap3 : {A B : Set} (n : Nat) → (A → B) → Vec2 A n → Vec2 B n
vmap3  zero   f  nil           = nil
vmap3 (suc n) f (cons .n x xs) = cons n (f x) (vmap3 n f xs)

{-
rule for when arg should be dotted:
- if there is a unique type correct value for the argument it should be dotted

above, the terms under the dots were valid patterns
- in general they can be arbitrary terms
e.g.,
-}

data Image_el_ {A B : Set}(f : A → B) : B → Set where
  im : (x : A) → Image f el f x

{-
only way to construct an element in the image of f
is to pick an argument x and apply f to x

knowing  that a particular y is in the image of f enables computing the inverse of f on y:
-}

inv : {A B : Set} (f : A → B) (y : B) → Image f el y → A
inv f .(f x) (im x) = x

{-
--------------------------------------------------
ABSURD PATTERNS
-}

-- family of numbers smaller than a given natural number
data Fin : Nat → Set where
  fzero : {n : Nat} →         Fin (suc n) -- smaller than suc n for any n
  fsuc  : {n : Nat} → Fin n → Fin (suc n) -- if i smaller than n then fsuc i is smaller than suc n

-- Now way to construct a Fin smaller than zero.
-- When no possible constructor patterns for a given argument
-- then pattern match with absurd pattern ()

magic : {A : Set} → Fin zero → A
magic ()

{-
Using absurd pattern means no need to give a right hand side,
since no way anyting could given as an arg that would match.

Seems the clause not needed, that checker could tell.
But a case can only be omitted if no type correct way of writing it.

'magic a' has a type correct left hand side

absurd pattern can only be used if there are no valid constructor patterns for the arg.
It is not enough that there are no closed inhabitants of the type.
E.g.,
-}

data Empty : Set where
  empty : Fin zero → Empty

{-
Args of type Empty cannot be matched with absurd pattern,
since there is a valid constructor pattern: 'empty x'
So must write:
-}

magic’ : {A : Set} → Empty → A
magic’ (empty ()) -- magic’ () -- not accepted

-- Extract ith element of the list (starting from 0):
-- Turns a list into a function from indices to elements.
_!_ : {n : Nat} {A : Set} → Vec A n → Fin n → A
[]        ! ()
(x :: xs) !  fzero   = x
(x :: xs) ! (fsuc i) = xs ! i

{-
types ensure no danger of indexing outside the list

reflected in use of absurd in empty case : no possible values for the index
-}

-- constructing a list given a function from indices to elements
tabulate : {n : Nat} {A : Set} → (Fin n → A) → Vec A n
tabulate  {zero} f = []
tabulate {suc n} f = f fzero :: tabulate (f ◦ fsuc) -- recursive call implicitly gets 'n'

{-
tabulate is defined by recursion over the length of the result list,
even though it is an implicit argument.

In general, no correspondance between implicit data and computationally irrelevant data.

------------------------------------------------------------------------------
-- 2.5 PROGRAMS AS PROOFS

Type system can represent propositions as types whose elements are proofs of the proposition.
-}

data   False : Set where -- datatype with no constructors
record True  : Set where -- record type with no fields
                         -- has a single element : the empty record

{-
Could have defined True as datatype with a single element.
But using record def lets checker know there is a unique element of True
and will fill in any implicit arguments of type True with this element.

exploited in trivial:
-}

trivial : True
trivial = _

trivial' : True
trivial' = record {}

{-
where right hand is underscore,
instead of explicitly writing 'record {}'

The 'False' and 'True' propositions enable working with decidable propositions.
Can model decidable propositions as booleans and define
-}

-- takes a VALUE of type Bool
-- returns a Set (i.e., a TYPE)
isTrue : Bool → Set
isTrue true  = True
isTrue false = False

isFalse : Bool → Set
isFalse x = isTrue (not x)

{-
isTrue b is the type of proofs that b equals true.

This technique enables defining safe list lookup function in a different way,
working on simply typed lists and numbers.
-}

-- takes   Nat  VALUES
-- returns Bool VALUES
_<_ : Nat → Nat → Bool
_     < zero  = false
zero  < suc n = true
suc m < suc n = m < n

length : {A : Set} → List A → Nat
length        [] = zero
length (x :: xs) = suc (length xs)

--                                             |
--                                             v
lookup : {A : Set} (xs : List A) (n : Nat) → isTrue (n < length xs) → A
lookup       []       n ()
lookup (x :: xs)  zero   p = x
lookup (x :: xs) (suc n) p = lookup xs n p

{-
Rather than there being no index into the empty list,
there is no proof that a number n is smaller than zero.

In this example, using indexed types to capture the precondition,
as done in '_!_' above, is a little bit nicer,
since do not have to pass an explicit proof object
as done in 'lookup' above.

When properties cannot be easily captured by indexed types, this is a useful alternative.

--------------------------------------------------
DEFINE PROPOSITIONS USING DATATYPE FAMILIES
-}

-- family of proofs of “being equal to x”
-- specify the type by giving it two args
-- type can only be constructed when the two args reduce to the same thing
data _≡_ {A : Set} (x : A) : A → Set where
  refl : x ≡ x -- only inhabited at index x where the single proof is refl

-- another exampla
-- compare to def of '_<_' above
data _≤_ : Nat → Nat → Set where
  leq-zero :   {n : Nat} →         zero  ≤     n
  leq-suc  : {m n : Nat} → m ≤ n → suc m ≤ suc n

{-
advantage of this approach
- PATTERN MATCH ON PROOF OBJECT
- makes proving properties of _≤_ easier
- e.g.,
-}

leq-trans : {l m n : Nat} → l ≤ m → m ≤ n → l ≤ n
leq-trans  leq-zero            _  = leq-zero
leq-trans (leq-suc p) (leq-suc q) = leq-suc (leq-trans p q)

{-
------------------------------------------------------------------------------
-- 2.6 WITH : PATTERN MATCH ON RESULT OF INTERMEDIATE COMPUTATION

Haskell : done on right hand side using case

when matching expression in dependently typed language, learn
- shape of expression
- things about other expressions
  - e.g., matching 'Vec A n' reveals info about n
- not captured by usual case expression

to pattern match on an expression e in the def of fun f
- abstract f over value of e
- effectively adding another argument to f
- can then be matched on in usual way
-}

min : Nat → Nat → Nat
min x y with x < y
min x y | true  = x
min x y | false = y

{-
Can abstract over multiple expressions at same time, separated by vertical bars.

Can nest with abstractions.

When matching on arg using WITH, no need to match on args again:
- use ...
-}

filter : {A : Set} → (A → Bool) → List A → List A
filter p [] = []
filter p (x :: xs) with p x
... | true  = x :: filter p xs
... | false =      filter p xs

{-
Example of revealing info via matching.

Compare numbers for equality.
Instead of returning boolean,
return proof of equality or
explanation of why they are not equal.
-}

data _≠_ : Nat → Nat → Set where
  z≠s :   {n : Nat} →          zero ≠ suc n -- different if one is zero and other suc
  s≠z :   {n : Nat} →         suc n ≠ zero  -- vice versa
  s≠s : {m n : Nat} → m ≠ n → suc m ≠ suc n -- both suc but their predecessors are different

data Equal? (n m : Nat) : Set where
  eq  : n ≡ m → Equal? n m
  neq : n ≠ m → Equal? n m

-- now function that returns the explanation

equal? : (n m : Nat) → Equal? n m
--                                  Equal? zero zero
--                                      zero ≡ zero
equal?  zero    zero             = eq  refl
equal?  zero   (suc _)           = neq z≠s
equal? (suc _)  zero             = neq s≠z
equal? (suc n) (suc m) with equal? n m -- matching on proof reveals if predecessors are equal
equal? (suc _) (suc _) | eq refl = eq refl
--                                 Equal? (suc n) (suc m)
--                                      suc n ≠ suc m
--                                          n ≠     m
equal? (suc _) (suc _) | neq p   = neq (s≠s p)

{-
using WITH : expression is abstracted from entire context

means that if expression occurs in type of arg to function or in result type,
occurrence is replaced by the with-argument on the left hand side.

e.g., prove 'filter' only removes some elements
-}

-- to make a sublist, each element can either be dropped or kept
infix 20 _⊆_
data _⊆_ {A : Set} : List A → List A → Set where
  stop :                                   [] ⊆      []
  drop : forall {xs y ys} → xs ⊆ ys →      xs ⊆ y :: ys
  keep : forall {x xs ys} → xs ⊆ ys → x :: xs ⊆ x :: ys

{-
When checker can infer type of an arg in fun type, then can use forall:
– forall {x y} a b → A    shorthand for   {x : _} {y : _} (a : _) (b : _) → A
-}

-- proof that filter computes a sublist of its argument:
lem-filter : {A : Set} (p : A → Bool) (xs : List A)
           → filter p xs ⊆ xs
lem-filter p       [] = stop
lem-filter p (x :: xs) with p x
... | true            = keep (lem-filter p xs)
... | false           = drop (lem-filter p xs)

{-
to prove     lem-filter p (x :: xs)

need to prove : (filter p (x :: xs) | p x) ⊆ x :: xs

when abstracting over p x it will be abstracted from goal type, giving
             lem-filter p (x :: xs) with p x
             ... | px = ?

where p x has been replaced by px in the goal type
                (filter p (x :: xs) | px) ⊆ x :: xs

match px reduce call to filter

             lem-filter p (x :: xs) with p x
             ... | true  = ? {- x :: filter p xs ⊆ x :: xs -}
             ... | false = ? {-      filter p xs ⊆ x :: xs -}

Sometimes WITH useful to abstract over expr that will not be matched,
e.g., expect expr to be instantiated by matching on something else
-}

lem-plus-zero : (n : Nat) → n + zero ≡ n
lem-plus-zero zero = refl
lem-plus-zero (suc n) with n + zero | lem-plus-zero n
... | _ | refl = refl -- suc n ≡ suc n

{-
In suc ("step" case) : match on induction hypothesis : n + zero ≡ n
to prove suc n + zero ≡ suc n

but n + zero does not unify with n

so abstract over n + zero, calling it _,
left with the induction hypothesis m ≡ n and goal suc m ≡ suc n
Now match on induction hypothesis, instantiating _ to n

--------------------------------------------------
-- 2.7 Modules TODO

--------------------------------------------------
-- 2.8 Records
-}

record Point : Set where
  field
    x : Nat
    y : Nat

mkPoint : Nat → Nat → Point
mkPoint a b = record { x = a; y = b }

{-
field projections via module of the same name.
- parameterised by element of record type
- contains projection functions for the fields
- i.e.,
   module Point (p : Point) where
     x : Nat
     y : Nat
- can be used as is or instantiated to a particular record
-}

getX : Point → Nat
getX = Point.x

abs2 : Point → Nat
abs2 p = let open Point p in x * x + y * y

-- TODO pattern match on records

-- can add functions to the module of a record

record Monad (M : Set → Set) : Set1 where
  field
    return :   {A : Set} →   A →             M A
    _>>=_  : {A B : Set} → M A → (A → M B) → M B

  mapM : {A B : Set} → (A → M B) → List A → M (List B)
  mapM f [] = return []
  mapM f (x :: xs) = f x >>= \y →
              mapM f xs >>= \ys →
              return (y :: ys)

mapM’ : {M : Set → Set}
      → Monad M
      → {A B : Set}
      → (A → M B)
      → List A
      → M (List B)
mapM’ Mon f xs = Monad.mapM Mon f xs

--------------------------------------------------
-- 2.9 Exercises

-------------------------
-- Exercise 2.1. Matrix transposition
-- inner vectors are rows
Matrix : Set → Nat → Nat → Set
Matrix A n m = Vec (Vec A n) m

-- (a) function to compute vector containing n copies of element x

vec : {n : Nat} {A : Set} → A → Vec A n
vec  {zero} x = []
vec {suc n} x = x :: vec {n} x

vecTest : Vec Nat 3
vecTest = vec zero

-- (b) point-wise application of vector of functions to vector of arguments

infixl 90 _$_
_$_ : {n : Nat} {A B : Set} → Vec (A → B) n → Vec A n → Vec B n
_$_       []        []  = []
_$_ (f :: fs) (x :: xs) = f x :: fs $ xs

$TestInputFs : Vec (Nat → Nat) 2
$TestInputFs = (_+ 1) :: (_* 2) :: []

$TestInputXs : Vec Nat 2
$TestInputXs =     0 ::      2  :: []

$TestOutput  : Vec Nat 2
$TestOutput  =     1 ::      4  :: []

$Test : $TestInputFs $ $TestInputXs ≡ $TestOutput
$Test = refl

-- (c) matrix transposition in terms of 'vec' and _$_

transpose : forall {A n m} → Matrix A n m → Matrix A m n
transpose         []  = vec []
transpose (xs :: xss) = (vmap _::_ xs) $ (transpose xss)

transposeTestInput : Matrix Nat 2 3
transposeTestInput = r1 :: r2 :: r3 :: []
 where
  r1 = 1 :: 2 :: []
  r2 = 3 :: 4 :: []
  r3 = 5 :: 6 :: []

transposeTestOuput : Matrix Nat 3 2
transposeTestOuput = r1 :: r2 :: []
 where
  r1 = 1 ::  3 :: 5 :: []
  r2 = 2  :: 4 :: 6 :: []

transposeTest : transpose transposeTestInput ≡ transposeTestOuput
transposeTest = refl

-------------------------
-- Exercise 2.2. Vector lookup

-- function composition
_∘_  : ∀ {A B C : Set} → (B → C) → (A → B) → (A → C)
(g ∘ f) x = g (f x)

-- prove 'tabulate' and '!' are each other’s inverses

-- (a) relatively easy
lem-!-tab : ∀ {A n}
          → (f : Fin n → A) → (i : Fin n)
          → ((tabulate f) ! i) ≡ f i
lem-!-tab f  fzero   = refl                   -- (tabulate f ! fzero)  ≡ f  fzero
lem-!-tab f (fsuc i) = lem-!-tab (f ∘ fsuc) i -- (tabulate f ! fsuc i) ≡ f (fsuc i)

-- (b) trickier
lem-tab-! : forall {A n}
          → (xs : Vec A n)
          → tabulate (xs !_) ≡ xs
lem-tab-!       [] = refl
lem-tab-! (x :: xs) -- tabulate (_!_ (x :: xs)) ≡ (x :: xs)
  with tabulate (xs !_) | lem-tab-! xs
... | _y     | refl = refl -- (x :: _y) ≡ (x :: xs)
--    ^        ^
-- Vec A n     _y ≡ xs

-------------------------
-- Exercise 2.3. Sublists (see def above)

-- (a) prove reflexivity and transitivity of ⊆

-- need to name implicits since the interesting one does not come first
⊆-refl : {A : Set} {xs : List A}
       → xs ⊆ xs
⊆-refl {xs =       []} = stop
⊆-refl {xs = x :: xss} = keep (⊆-refl { xs = xss })

{-
   []    []    []
   []    []   [z]
   []   [z]   [z]
  [z]   [z]   [z]
  [z]   [z] [z,z]
  [z] [z,z] [z,z]
[z,z] [z,z] [z,z]
-}

⊆-trans : {A : Set} {xs ys zs : List A}
        → xs ⊆ ys
        →      ys ⊆ zs
        → xs ⊆      zs
--       []   []    []
⊆-trans  stop      stop     = stop

--       []   []   [z]
--      [z]  [z] [z,z]
⊆-trans       xy  (drop yz) = drop (⊆-trans xy yz)

--      []   [z]   [z]
--     [z] [z,z] [z,z]
⊆-trans (drop xy) (keep yz) = drop (⊆-trans xy yz)

--     [z]   [z]   [z]
⊆-trans (keep xy) (keep yz) = keep (⊆-trans xy yz)

⊆-trans' : {A : Set} {xs ys zs : List A}
        → xs ⊆ ys
        →      ys ⊆ zs
        → xs ⊆      zs
⊆-trans'  stop        stop     = stop
⊆-trans'  stop       (drop yz) = drop (⊆-trans'  stop     yz)
⊆-trans' (drop xy)   (drop yz) = drop (⊆-trans' (drop xy) yz)
⊆-trans' (drop xy)   (keep yz) = drop (⊆-trans'       xy  yz)
⊆-trans' (keep xy)   (drop yz) = drop (⊆-trans' (keep xy) yz)
⊆-trans' (keep xy)   (keep yz) = keep (⊆-trans'       xy  yz)

-- sublist TYPE of a specific list (compare to existing sublist RELATION above)
infixr 30 _:::_
data SubList {A : Set} : List A → Set where
  []    : SubList []
  _:::_ : forall x {xs} → SubList xs → SubList (x :: xs)
  skip  : forall {x xs} → SubList xs → SubList (x :: xs)

-- (b) extract list corresponding to a sublist
forget : {A : Set} {xs : List A}
       → SubList xs
       → List A
forget       []  = []
forget (x ::: s) = x :: forget s
forget  (skip s) =      forget s

-- (c) prove SubList is a sublist in the sense of ⊆
lem-forget : {A : Set} {xs : List A}
           → (zs : SubList xs)
           → forget zs ⊆ xs
lem-forget        []  = stop
lem-forget (x ::: zs) = keep (lem-forget zs)
lem-forget  (skip zs) = drop (lem-forget zs)

-- (d) alternative def of filter : satisfies sublist property by construction
filter' : {A : Set}
        → (A → Bool) → (xs : List A)
        → SubList xs
filter' p       []  = []
filter' p (x :: xs) with p x
... | true  = x ::: filter' p xs
... | false = skip (filter' p xs)

-- (e) complement of a sublist
complement : {A : Set} {xs : List A}
           → SubList xs
           → SubList xs
complement              []  = []
complement       (x ::: xs) = skip (complement xs)
complement (skip {x}    xs) = x ::: complement xs

module ComplementTest where
  ll : List Nat
  ll = 1 :: 2 :: 3 :: 4 :: []

  p1 : Nat → Bool
  p1 2 = true
  p1 3 = true
  p1 4 = true
  p1 _ = false

  p2 : Nat → Bool
  p2 2 = true
  p2 4 = true
  p2 _ = false

  sl1 : SubList ll
  sl1 = filter' p1 ll

  sl2 : SubList ll
  sl2 = filter' p2 ll

  sl1Test : sl1 ≡ skip (2 ::: 3 ::: 4 ::: [])
  sl1Test = refl

  sl2Test : sl2 ≡ skip (2 ::: skip (4 ::: []))
  sl2Test = refl

  c1 : SubList ll
  c1 = complement sl1

  c2 : SubList ll
  c2 = complement sl2

  cTest1 : c1 ≡ 1 ::: skip (skip (skip []))
  cTest1 = refl

  cTest2 : c2 ≡ 1 ::: skip (3 ::: skip [])
  cTest2 = refl

-- https://medium.com/@angerman/powersets-in-haskell-1df9684db52a
-- (f) compute all sublists of a given list
sublists : {A : Set}
         → (xs : List A)
         → List (SubList xs)
sublists        [] = [] :: []
sublists (x :: xs) = map (x :::_) (sublists xs) ++ (map skip (sublists xs))

-- 2^3 elements - expected output MUST be in same element ORDER as function result
sublistsTest : sublists (1 :: 2 :: 3 :: []) ≡
  (1 ::: 2 ::: 3 ::: [])  ::
  (1 ::: 2 ::: skip  [])  ::
  (1 ::: skip (3 ::: [])) ::
  (1 ::: skip (skip  [])) ::
  skip  (2 ::: 3 ::: [])  ::
  skip  (2 ::: skip  [])  ::
  skip  (skip (3 ::: [])) ::
  skip  (skip (skip  [])) ::
  []
sublistsTest = refl

{-
------------------------------------------------------------------------------
3 Programming Techniques : VIEWS and UNIVERSE constructions

--------------------------------------------------
3.1 VIEWS

matching can reveal info about term being matched AND terms INSIDE the type matched term

VIEW[5]: datatypes whose purpose is to reveal info about its indices

to use a view, define a view function
- computes an element of the view for arbitrary indices
-}

-- view datatype expressing
-- any Nat can be expressed as 2k or 2k + 1 for some k
-- element of Parity n says if n is even or odd and what k is
data Parity : Nat → Set where
  even : (k : Nat) → Parity     (k * 2)
  odd  : (k : Nat) → Parity (1 + k * 2)

parity : (n : Nat) → Parity n
parity  zero = even zero
parity (suc n) with parity n
...  | even k = odd k
...  | odd  k = even (suc k)
{-
parity (suc     .(k * 2)) | even k = odd k
parity (suc .(1 + k * 2)) | odd  k = even (suc k)
-}

half : Nat → Nat
half n with parity n
...  | even k = k
...  | odd  k = k
{-
-- Note that k is bound in the pattern for the view,
-- not in the dotted pattern for the natural number.
half     .(k * 2) | even k = k
half .(1 + k * 2) | odd  k = k
-}

-------------------------
-- FINDING AN ELEMENT IN A LIST

-- given predicate and list
-- returns if P holds for all elements
-- A proof of All P xs is a list of proofs of P x for each element x of xs.
-- P does not have to be a decidable predicate.
infixr 30 _:all:_
data All {A : Set} (P : A → Set) : List A → Set where
  all[]   :                                  All P       []
  _:all:_ : forall {x xs} → P x → All P xs → All P (x :: xs)

-- to turn a decidable predicate into a general predicate, define:
satisfies : {A : Set} → (A → Bool) → A → Set
satisfies p x = isTrue (p x)

-------------------------
-- exercise : use All to prove 2nd part of correctness of filter
-- - all elements of result satisfies the predicate
--    All (satisfies p) (filter p xs)

-- https://www.javaer101.com/en/article/18631037.html
{-
open import Relation.Binary.PropositionalEquality

filter-lem-b : {A : Set} → (p : A → Bool) → (xs : List A) → All (satisfies p) (filter p xs)
filter-lem-b p []        = vacuo
filter-lem-b p (x :: xs) with p x | inspect p x
... | true  | [ eq ] = holds _ _ (subst isTrue (sym eq) _) (filter-lem-b p xs)
... | false | [ eq ] = filter-lem-b p xs
-}

-- https://stackoverflow.com/questions/38572464/agda-type-isnt-simplified-in-with-block
lem-all-filter : {A : Set}
               → (p : A → Bool) → (xs : List A)
               → All (satisfies p) (filter p xs)
lem-all-filter p [] = all[]
--                                           isTrue   (p x)
lem-all-filter p (x :: xs) with p x | λ (y : satisfies p x) → y :all: lem-all-filter p xs
-- onTrue : (y : True) → All (λ x₁ → isTrue (p x₁)) (x :: filter p xs)
... | true  | onTrue = onTrue _
... | false |      _ = lem-all-filter p xs

-------------------------
-- VIEWS ON LISTS

-- given : decidable predicate on elements of list
-- find element in list that satisfies  predicate,
--  or else all elements satifies negation of the predicate

data Find {A : Set} (p : A → Bool) : List A → Set where
  -- does NOT specify which element to use as a witness in the found case.
  -- (If the view was always to return first (or last) matching element,
  -- force elements of xs (or ys) to satisfy the negation of p.)
  found     : (xs : List A) → (y : A) → satisfies p y → (ys : List A)
            → Find p (xs ++ y :: ys)
  not-found : forall {xs} → All (satisfies (not ◦ p)) xs
            → Find p  xs

-- view function computing an element of Find p xs for any p and xs

-- 1st attempt
{-
find1 : {A : Set} (p : A → Bool) (xs : List A) → Find p xs
find1 p [] = not-found all[]
find1 p (x :: xs) with p x
-- Need to return found on first match.
-- ({ }) is isTrue (p x), even though already matched on p x and found out that it was true.
-- Problem : when abstracting over p x did not know that needed to use the found constructor,
--    so there were no p x to abstract over.
-- WITH does not remember connection between the with-term and the pattern.
... | true  = found [] x {! !} xs
... | false = {! !}
-}

{-
A solution : make this connection explicit with a proof object.
Do NOT abstract over the term itself
- instead rather over arbitrary term of same type
  AND a proof that it is equal to the original term
-}

-- type of elements of type A AND proofs they are equal to some given x in A
data Inspect {A : Set} (x : A) : Set where
  it : (y : A) → x ≡ y → Inspect x

-- construct an element of Inspect x by picking x as thing which is equal to x.
inspect : {A : Set} → (x : A) → Inspect x
inspect x = it x refl

-- lemmas
trueIsTrue   : {x : Bool} → x ≡ true  → isTrue  x
falseIsFalse : {x : Bool} → x ≡ false → isFalse x
trueIsTrue   refl = _
falseIsFalse refl = _

{-
now define find by abstracting over inspect (p x) rather than p x
provide either a proof of p x == true or a proof of p x == false
- can be use in args to 'found' and 'not-found
-}

find : {A : Set}
     → (p : A → Bool) → (xs : List A)
     → Find p xs
find p       [] = not-found all[]
find p (x :: xs) with inspect (p x)
-- When p x is true, inspect (p x) matches 'it true prf' where prf : p x == true.
-- Use lemma to turn into proof of isTrue (p x) needed by 3rd arg of 'found'
... | it true  prf = found [] x (trueIsTrue prf) xs
... | it false prf with find p xs
find p (x :: ._) | it false _   | found xs y py ys = found (x :: xs) y py ys
-- p x is false : use lemma
find p (x :: xs) | it false prf | not-found npxs   = not-found (falseIsFalse prf :all: npxs)

-------------------------
-- INDEXING INTO A LIST

{-
Previously showed two ways of safely indexing into a list.
Both cases used type system to guarantee the index didn’t point outside the list.

In situations where there is no control over value of index (i.e., it might be outside)
a solution is to wrap result of lookup in MAYBE, but MAYBE provides no info.
-}

-- type of proofs that an element x is in a list xs.
data _∈_ {A : Set} (x : A) : List A → Set where
  hd : forall   {xs} →          x ∈ x :: xs -- 1st el is a member
  tl : forall {y xs} → x ∈ xs → x ∈ y :: xs -- any el in tail is member


-- Given proof of x ∈ xs, compute index where x occurs.
-- Count number of tls in proof.
index : forall {A} {x : A} {xs} → x ∈ xs → Nat
index  hd    = zero
index (tl p) = suc (index p)

-- view on Nat with respect to list
data Lookup {A : Set} (xs : List A) : Nat → Set where
  inside  : (x : A) → (p : x ∈ xs) → Lookup xs (index p)
  outside : (m : Nat)              → Lookup xs (length xs + m)

{-
When n is valid, get element at that position and guarantee that element is returned.
No way for 'lookup' to cheat.

When n is outside, get out-of-bounds proof showing by how much.
-}

-- now, guaranteed 'lookup' function
_!'_ : {A : Set}
     → (xs : List A) → (n : Nat)
     → Lookup xs n
[] !' n           = outside n
(x :: xs) !' zero = inside x hd
(x :: xs) !' suc n with xs !' n
...                               | inside y p = inside y (tl p)
(x :: xs) !' suc .(length xs + n) | outside n  = outside n
{-
(x :: xs) !' suc .(index p)       | inside y p = inside y (tl p)
(x :: xs) !' suc .(length xs + n) | outside n  = outside n
-}

--------------------------------------------------
-- TYPE CHECKER FOR λ-CALCULUS

