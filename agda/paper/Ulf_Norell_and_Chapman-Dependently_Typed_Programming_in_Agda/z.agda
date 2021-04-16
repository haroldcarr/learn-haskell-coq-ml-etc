module z where

-- http://www.cse.chalmers.se/~ulfn/papers/afp08/tutorial.pdf
{-
Dependently Typed Programming in Agda
Ulf Norell1, Chalmers University, Gothenburg ulfn@chalmers.se
2
James Chapman2, Institute of Cybernetics, Tallinn james@cs.ioc.ee

------------------------------------------------------------------------------
1 Introduction

In Hindley-Milner languages (Haskell, ML) : separation between types and values.

Dependently typed : types can contain (depend on) arbitrary values and
                    appear as arguments and results of ordinary functions.

Dependent types enables types to talk about values,
so can encode properties of values as types whose elements
are proofs that the property is true.

So dependently typed programming language can be used as a logic.

To be consistent programs must be total.

------------------------------------------------------------------------------
2 Agda Basics

based on intuitionistic type theory[4]

--------------------------------------------------
2.1 Datatypes and pattern matching
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
 zero : Nat
 suc  : Nat → Nat
{-# BUILTIN NATURAL Nat #-}

_+_ : Nat → Nat → Nat
zero  + m = m
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
2.2 Dependent functions

result type depends on VALUE of an arg
  (x : A) → B for the
type of functions
- taking an arg x of type A
- returning a result of type B
  - where x may appear in B

special case is when x itself is a type, e.g.,
-}

identity : (A : Set) → A → A
identity A x = x

zero’ : Nat
zero’ = identity Nat zero

{-
identity is a dependent function
- taking
  - a type argument A and
  - an element of A
- returns the element

This is how polymorphic functions are encoded in Agda.

example : function which takes a dependent function and applies it to an argument:
-}

apply : (A : Set) (B : A → Set) → ((x : A) → B x) → (a : A) → B a
apply A B f a = f a

{-
short hands for dependent function types:

  (x : A) (y : B) → C      for     (x : A) → (y : B) → C

  (x y : A) → B            for     (x : A) (y : A) → B


The elements of dependent function types are lambda terms which may carry explicit type info.

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
2.3 Implicit arguments

Previous section showed how dependent functions taking types as
arguments used to model polymorphic types.

But it is not necessary to say at which it is applied - that can be inferred.

The identity functions above explicitly provide the type argument.

Avoid type arg via implicit arguments using {}
-}

id : {A : Set} → A → A
id x = x

true’ : Bool
true’ = id true

{-
No restrictions on what arguments can be made implicit.
Not guarantees that an implicit argument can be inferred.

by the type checker. For instance, we could be silly and make the second
argument of the identity function implicit as well:
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
-- 2.4 Datatype families

So far, only used dependent types is to represent polymorphism
-}

data Vec (A : Set) : Nat → Set where
  []   :                           Vec A  zero
  _::_ : {n : Nat} → A → Vec A n → Vec A (suc n)

{-
type of Vec A is Nat → Set
- means that Vec A is a family of types indexed by natural numbers
- for each natural number n, Vec A n is a type

constructors are free to construct elements in an arbitrary type of the family.
- []   constructs an element in Vec A zero
- _::_ constructs an element in Vec A (suc n) for some n

distinction between parameters and indices of a datatype.
- parameterised by a type A
- indexed over natural numbers

type of _::_ is a dependent function type.
- 1st arg is an implicit natural number n

Same constructor names for Vec as for List.
-  Constructor names are not required to be distinct between different datatypes.
-}

-- safe head
head : {A : Set} {n : Nat} → Vec A (suc n) → A
head (x :: xs) = x

tail : {A : Set} {n : Nat} → Vec A (suc n) → Vec A n
tail (x :: xs) = xs

{-
rule for when to include a particular case is:
  if it is type correct you have to include it.
-}

-- DOT PATTERNS
vmap : {A B : Set} {n : Nat} → (A → B) → Vec A n → Vec B n
vmap f [] = []
vmap f (x :: xs) = f x :: vmap f xs

{-
map on Vec is exactly the same map on List. only change is the type

behind the scenes, what happens with the length argument pattern matching on the list?

to see this, define new versions of Vec and vmap with fewer implicit arguments:
-}

data Vec2 (A : Set) : Nat → Set where
  nil  :                               Vec2 A  zero
  cons : (n : Nat) → A → Vec2 A n → Vec2 A (suc n)

vmap2 : {A B : Set} (n : Nat) → (A → B) → Vec2 A n → Vec2 B n
vmap2 .zero    f  nil          = nil
vmap2 .(suc n) f (cons n x xs) = cons n (f x) (vmap2 n f xs)

{-
pattern matching on list argument reveals its length
- if nil then zero
- if cons then suc n

to indicate the value of an argument has been deduced by type checking,
rather than observed by pattern matching, it is prefixed by a dot (.).

could choose to define vmap by first pattern matching on the length rather than on the list
-}

vmap3 : {A B : Set}(n : Nat) → (A → B) → Vec2 A n → Vec2 B n
vmap3  zero   f  nil           = nil
vmap3 (suc n) f (cons .n x xs) = cons n (f x) (vmap3 n f xs)

{-
rule for when arg should be dotted:
- if there is a unique type correct value for the argument it should be dotted.

above, the terms under the dots were valid patterns,
but in general they can be arbitrary terms.
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
Absurd patterns
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

reflected in case of empty list : no possible values for the index
-}

-- Constructing a list given a function from indices to elements
tabulate : {n : Nat} {A : Set} → (Fin n → A) → Vec A n
tabulate  {zero} f = []
tabulate {suc n} f = f fzero :: tabulate (f ◦ fsuc)

{-
tabulate is defined by recursion over the length of the result list,
even though it is an implicit argument.

In general, no correspondance between implicit data and computationally irrelevant data.

------------------------------------------------------------------------------
-- 2.5 Programs as proofs

Type system can represent propositions as types whose elements are proofs of the proposition.
-}

data   False : Set where -- datatype with no constructors
record True  : Set where -- record type with no fields
                         -- has a single element : the empty record

trivial : True
trivial = _

{-
Could have defined True as datatype with a single element.
But using record def lets checker know there is a unique element of True
and will fill in any implicit arguments of type True with this element.

Exploited in trivial where right hand is underscore.

To write the element of True : record {}

These two propositions enable working with decidable propositions.
Can model decidable propositions as booleans and define
-}

isTrue : Bool → Set
isTrue true  = True
isTrue false = False

{-
isTrue b is the type of proofs that b equals true.

This technique enables defining safe list lookup function in a different way,
working on simply typed lists and numbers.
-}

_<_ : Nat → Nat → Bool
_     < zero  = false
zero  < suc n = true
suc m < suc n = m < n

length : {A : Set} → List A → Nat
length        [] = zero
length (x :: xs) = suc (length xs)

lookup : {A : Set} (xs : List A) (n : Nat) → isTrue (n < length xs) → A
lookup       []       n ()
lookup (x :: xs)  zero   p = x
lookup (x :: xs) (suc n) p = lookup xs n p

{-
Rather than there being no index into the empty list,
there is no proof that a number n is smaller than zero.

In this example, using indexed types to capture the precondition is a little bit nicer,
since do not have to pass an explicit proof object.

But some properties cannot be easily captured by indexed types,
in which case this is a nice alternative.

Can also use datatype families to define propositions.
-}

-- family of proofs of “being equal to x”
data _==_ {A : Set} (x : A) : A → Set where
  refl : x == x -- is only inhabited at index x where the single proof is refl

{-
Can be defined as a boolean function, like _<_ above,
but can define it inductively:
-}

data _≤_ : Nat → Nat → Set where
  leq-zero :   {n : Nat} →           zero ≤     n
  leq-suc  : {m n : Nat} → m ≤ n → suc m ≤ suc n

{-
advantage of this approach
- pattern match on proof object
- makes proving properties of _≤_ easier
- e.g.,
-}

leq-trans : {l m n : Nat} → l ≤ m → m ≤ n → l ≤ n
leq-trans  leq-zero            _  = leq-zero
leq-trans (leq-suc p) (leq-suc q) = leq-suc (leq-trans p q)

{-
------------------------------------------------------------------------------
-- 2.6 More on pattern matching

seen pattern matching  on fun args

sometimes need pattern match on result of intermediate computation

Haskell : done on right hand side using case

matching expression in dependently typed language, learn
- shape of the expression
- things about other expressions
  - e.g., matching 'Vec A n' reveals info about n
- not captured by usual case expression
- Agda provides way of matching on intermediate computations on left hand side

-------------------------
WITH

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

In case pattern matching on arg to with (e.g., x < y)
then no need to match on args again.
To avoid repeating the left hand side, use ...
bit tedious.
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
  z≠s :   {n : Nat} →           zero ≠ suc n -- different if one is zero and other suc
  s≠z :   {n : Nat} →          suc n ≠ zero  -- ditto
  s≠s : {m n : Nat} → m ≠ n → suc m ≠ suc n -- both suc but their predecessors are different

data Equal? (n m : Nat) : Set where
  eq  : n == m → Equal? n m
  neq : n ≠  m → Equal? n m

-- now function equal?

equal? : (n m : Nat) → Equal? n m
equal?   zero   zero   = eq refl
equal?   zero  (suc m) = neq z≠s
equal? (suc n)  zero   = neq s≠z
equal? (suc n) (suc m) with equal? n m -- matching on proof reveals if predecessors are equal
equal? (suc n) (suc .n) | eq refl = eq refl
equal? (suc n) (suc m)  | neq p   = neq (s≠s p)

{-
using WITH : expression is abstracted from the entire context

means that if expression occurs in type of arg to the function or in result type,
occurrence is replaced by the with-argument on the left hand side.

e.g., prove something 'filter' : it only may remove some elements
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

-- proof that filter computes a sublist of its argument:
-}

lem-filter : {A : Set} (p : A → Bool) (xs : List A) → filter p xs ⊆ xs
lem-filter p [] = stop
lem-filter p (x :: xs) with p x
... | true      = keep (lem-filter p xs)
... | false     = drop (lem-filter p xs)

{-
to prove     lem-filter p (x :: xs)

need to prove : (filter p (x :: xs) | p x) ⊆ x :: xs

when abstracting over p x it will be abstracted from the goal type, giving
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

lem-plus-zero : (n : Nat) → n + zero == n
lem-plus-zero zero = refl
lem-plus-zero (suc n) with n + zero | lem-plus-zero n
... | .n | refl = refl

{-
In suc ("step" case) : match on induction hypothesis : n + zero == n
to prove suc n + zero == suc n

but n + zero does not unify with n

so abstract over n + zero, calling it m,
left with the induction hypothesis m == n and goal suc m == suc n
Now match on induction hypothesis, instantiating m to n

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

-- (a) function to compute a vector containing n copies of an element x.

vec : {n : Nat} {A : Set} → A → Vec A n
vec  {zero} x = []
vec {suc n} x = x :: vec {n} x

vecTest : Vec Nat (suc (suc (suc zero)))
vecTest = vec zero

-- (b) point-wise application of a vector of functions to a vector of arguments.

infixl 90 _$_
_$_ : {n : Nat} {A B : Set} → Vec (A → B) n → Vec A n → Vec B n
_$_       []        []  = []
_$_ (f :: fs) (x :: xs) = f x :: fs $ xs

$TestInputFs : Vec (Nat → Nat) (suc (suc zero))
$TestInputFs = (_+ suc zero) :: (_* suc (suc zero)) :: []

$TestInputXs : Vec Nat (suc (suc zero))
$TestInputXs =     zero ::           suc (suc zero)   :: []

$TestOutput  : Vec Nat (suc (suc zero))
$TestOutput  = suc zero :: suc (suc (suc (suc zero))) :: []

$Test : $TestInputFs $ $TestInputXs == suc zero :: suc (suc (suc (suc zero))) :: []
$Test = refl

-- (c) matrix transposition in terms of 'vec' and _$_

transpose : forall {A n m} → Matrix A n m → Matrix A m n
transpose         []  = vec []
transpose (xs :: xss) = (vmap _::_ xs) $ (transpose xss)

transposeTestInput : Matrix Nat (suc (suc zero)) (suc (suc (suc zero)))
transposeTestInput = r1 :: r2 :: r3 :: []
 where
  r1 =                     suc zero     ::                     suc (suc zero)     :: []
  r2 =           suc (suc (suc zero))   ::           suc (suc (suc (suc zero)))   :: []
  r3 = suc (suc (suc (suc (suc zero)))) :: suc (suc (suc (suc (suc (suc zero))))) :: []

transposeTestOuput : Matrix Nat (suc (suc (suc zero)))  (suc (suc zero))
transposeTestOuput =
  (     suc zero  ::      suc (suc (suc zero))  ::      suc (suc (suc (suc (suc zero))))  :: [])
  ::
  (suc (suc zero) :: suc (suc (suc (suc zero))) :: suc (suc (suc (suc (suc (suc zero))))) :: [])
  :: []

transposeTest : transpose transposeTestInput == transposeTestOuput
transposeTest = refl

-------------------------
-- Exercise 2.2. Vector lookup

-- prove 'tabulate' and '!' are each other’s inverses

-- (a) relatively easy
lem-!-tab : ∀ {A n} (f : Fin n → A) (i : Fin n)
          → ((tabulate f) ! i) == f i
lem-!-tab f  fzero   = refl
lem-!-tab f (fsuc i) = lem-!-tab (\x → f (fsuc x)) i

-- (b) trickier
lem-tab-! : forall {A n} (xs : Vec A n)
          → tabulate (xs !_) == xs
lem-tab-!       [] = refl
lem-tab-! (x :: xs) with tabulate (xs !_) | lem-tab-! xs
... | .xs | refl = refl

-------------------------
-- Exercise 2.3. Sublists (see def above)

-- (a) prove reflexivity and transitivity of ⊆
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

--      [],         [],                    []
--      []⊆[]       []⊆[]               []⊆[]
⊆-trans  stop        stop             = stop

--      [],         [],                    z::?
--                                           []⊆zs
--      []⊆[]       []⊆z::zs            []⊆z::zs      []⊆[]           []⊆zs
⊆-trans  stop       (drop q)          = drop (⊆-trans  stop           q)

--      xs,         y::xs                  z::y::xs
--                                           xs⊆zs          xs⊆ys
--      xs⊆y::ys    y::ys⊆z::zs         xs⊆z::zs      xs⊆y::ys        y::ys⊆zs
⊆-trans (drop p)    (drop q)          = drop (⊆-trans (drop p)        q)

--      xs,         z::y::xs,              z::z::y::xs
--                                           xs⊆zs
--      xs⊆y::ys    y::y::ys⊆z::zs      xs⊆z::zs            xs⊆ys     y::ys⊆zs
⊆-trans (drop p)    (keep q)          = drop (⊆-trans       p         q)

--      x::xs       x::xs::ys              z::x::xs::ys
--                                                          xs⊆ys
--      x::xs⊆y::ys y::ys⊆z::zs         xs⊆z::zs      x::xs⊆y::ys     ys⊆zs
⊆-trans (keep p)    (drop q)          = drop (⊆-trans (keep p)        q)

--      x::xs       z::y::x::xs,           z::z::y::x::xs
--      x::xs⊆y::ys y::y::ys⊆z::zs                          xs⊆ys
⊆-trans (keep p)    (keep q)          = keep (⊆-trans       p         q)

-- alternative to existing sublist relation
-- sublist type
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

  sl1Test : sl1 == skip (2 ::: 3 ::: 4 ::: [])
  sl1Test = refl

  sl2Test : sl2 == skip (2 ::: skip (4 ::: []))
  sl2Test = refl

  c1 : SubList ll
  c1 = complement sl1

  c2 : SubList ll
  c2 = complement sl2

  cTest1 : c1 == 1 ::: skip (skip (skip []))
  cTest1 = refl

  cTest2 : c2 == 1 ::: skip (3 ::: skip [])
  cTest2 = refl

