module z where

------------------------------------------------------------------------------
-- 1 intro

-- data types

data Nat : Set where
  zero :       Nat
  suc  : Nat → Nat
{-# BUILTIN NATURAL Nat #-}

--------------------------------------------------
-- begin : from later - here to use in tests
data Bool : Set where
  true  : Bool
  false : Bool

data IsTrue : Bool → Set where
  is-true : IsTrue true

_=Nat_ : Nat → Nat → Bool
zero    =Nat  zero   = true
(suc x) =Nat (suc y) = x =Nat y
_       =Nat  _      = false
-- end : from later
--------------------------------------------------

-- pattern matching

_+_ : Nat → Nat → Nat
zero    + y =          y
(suc x) + y = suc (x + y)
infixl 9 _+_
-- {-# BUILTIN NATPLUS _+_ #-}

-- ex 1.1
halve : Nat → Nat
halve       zero    = zero
halve (suc  zero)   = zero
halve (suc (suc n)) = suc (halve n)

halve-0  : IsTrue (halve   0 =Nat 0)
halve-0  = is-true
halve-1  : IsTrue (halve   1 =Nat 0)
halve-1  = is-true
halve-2  : IsTrue (halve   2 =Nat 1)
halve-2  = is-true
halve-3  : IsTrue (halve   3 =Nat 1)
halve-3  = is-true
halve-4  : IsTrue (halve   4 =Nat 2)
halve-4  = is-true
halve-14 : IsTrue (halve  14 =Nat 7)
halve-14 = is-true
halve-15 : IsTrue (halve  15 =Nat 7)
halve-15 = is-true
halve-16 : IsTrue (halve  16 =Nat 8)
halve-16 = is-true

-- ex 1.2
_*_ : Nat → Nat → Nat
zero    * y    = zero
(suc x) * y = y + (x * y)

2*0 : IsTrue ((2 * 0) =Nat  0)
2*0 = is-true
2*1 : IsTrue ((2 * 1) =Nat  2)
2*1 = is-true
2*2 : IsTrue ((2 * 2) =Nat  4)
2*2 = is-true
3*3 : IsTrue ((3 * 3) =Nat  9)
3*3 = is-true
4*4 : IsTrue ((4 * 4) =Nat 16)
4*4 = is-true

{-
-- ex 1.3
data Bool : Set where
  true  : Bool
  false : Bool
-}

not : Bool → Bool
not true  = false
not false = true

_&&_ : Bool → Bool → Bool
_&&_ true  y = y
_&&_ false _ = false

_||_ : Bool → Bool → Bool
_||_ true  _ = true
_||_ false y = y

-- types are 1st class
-- equivalent to Haskell type alias : type MyNat = Nat
MyNat : Set
MyNat = Nat

-- Set used to implement polymorphic functions
idA : (A : Set) → A → A
idA _ x = x

-- implicit args
id : {A : Set} → A → A
id  x = x

-- polymorphic data types
data List (A : Set) : Set where
  []   :              List A
  _::_ : A → List A → List A
infixr 5 _::_

-- ex 1.4
length : {A : Set} → List A → Nat
length       []  = zero
length (_ :: xs) = 1 + length xs

_++_ :  {A : Set} → List A → List A → List A
_++_       []  l2 =            l2
_++_ (x :: xs) l2 = x :: xs ++ l2

map : { A B : Set } → (A → B) → List A → List B
map _       []  = []
map f (x :: xs) = f x :: map f xs

-- ex 1.5
data Maybe (A : Set) : Set where
  just    : A → Maybe A
  nothing :     Maybe A

lookup : {A : Set} → List A → Nat → Maybe A
lookup       []       _  = nothing
lookup (x ::  _)  zero   = just x
lookup (_ :: xs) (suc n) = lookup xs n

_=List-Nat_ : List Nat → List Nat → Bool
[]        =List-Nat       []  = true
(x :: xs) =List-Nat (y :: ys) = (x =Nat y) && (xs =List-Nat ys)
_         =List-Nat        _  = false

ex-xs  : List Nat
ex-xs  = 0 :: 1 :: 2 :: 3 :: []

length++ : IsTrue ((length ex-xs + length ex-xs) =Nat (length (ex-xs ++ ex-xs)))
length++ = is-true

map2* : IsTrue (map (2 *_) ex-xs =List-Nat (0 :: 2 :: 4 :: 6 :: []))
map2* = is-true

_=Maybe-Nat_ : Maybe Nat → Maybe Nat → Bool
nothing   =Maybe-Nat  nothing  = true
(just x)  =Maybe-Nat  (just y) = x =Nat y
_         =Maybe-Nat        _  = false

lookup[]0    : IsTrue (lookup    [] 0 =Maybe-Nat nothing)
lookup[]0    = is-true
lookupex-xs0 : IsTrue (lookup ex-xs 0 =Maybe-Nat (just 0))
lookupex-xs0 = is-true
lookupex-xs1 : IsTrue (lookup ex-xs 1 =Maybe-Nat (just 1))
lookupex-xs1 = is-true
lookupex-xs9 : IsTrue (lookup ex-xs 9 =Maybe-Nat nothing)
lookupex-xs9 = is-true

data _×_ (A B : Set) : Set where
  _,_ : A → B → A × B
infixr 4 _,_

fst : {A B : Set} → A × B → A
fst (a , _) = a

snd : {A B : Set} → A × B → B
snd (_ , b) = b

-- ex 1.6
-- NO.  Regardless of the value of 'Nat', there is NO way to construct an A if given [].

------------------------------------------------------------------------------
-- 2 dependent types

--      parameter         : bound for ENTIRE def
--                  index : varies according to constructor
--          v         v
data Vec (A : Set) : Nat → Set where              -- Vec is dependent TYPE
  []   :                           Vec A 0
  _::_ : {n : Nat} → A → Vec A n → Vec A (suc n)
--infixr 5 _::_

-- dependent FUNCTION : return TYPE depends on inputs
zeroes : (n : Nat) → Vec Nat n
zeroes  zero   = []
zeroes (suc n) = 0 :: zeroes n

-- ex 2.1
downFrom : (n : Nat) → Vec Nat n
downFrom   zero  = []
downFrom (suc n) = n :: downFrom n

_=Vec-Nat_ : {n : Nat} → Vec Nat n → Vec Nat n → Bool
[]        =Vec-Nat       []  = true
(x :: xs) =Vec-Nat (y :: ys) = (x =Nat y) && (xs =Vec-Nat ys)

downFrom0 : IsTrue (downFrom 0 =Vec-Nat                                     [])
downFrom0 = is-true
downFrom1 : IsTrue (downFrom 1 =Vec-Nat                               (0 :: []))
downFrom1 = is-true
downFrom2 : IsTrue (downFrom 2 =Vec-Nat                          (1 :: 0 :: []))
downFrom2 = is-true
downFrom3 : IsTrue (downFrom 3 =Vec-Nat                     (2 :: 1 :: 0 :: []))
downFrom3 = is-true
downFrom7 : IsTrue (downFrom 7 =Vec-Nat (6 :: 5 :: 4 :: 3 :: 2 :: 1 :: 0 :: []))
downFrom7 = is-true

-- ex 2.2
tail : {A : Set} {n : Nat} → Vec A (suc n) → Vec A n
tail (_ :: xs) = xs

-- ex 2.3
dotProduct : {n : Nat} → Vec Nat n → Vec Nat n → Nat
dotProduct       []        []  = 0
dotProduct (x :: xs) (y :: ys) = (x * y) + dotProduct xs ys

dotProduct32 : IsTrue (dotProduct (1 :: 2 :: 3 :: []) (1 :: 5 :: 7 :: []) =Nat 32)
dotProduct32 = is-true

-- finite type family
data Fin : Nat → Set where
  zero : {n : Nat} →         Fin (suc n)
  suc  : {n : Nat} → Fin n → Fin (suc n)

lookupVec : {A : Set} {n : Nat} → Vec A n → Fin n → A
lookupVec       []   ()
lookupVec (x ::  _)  zero   = x
lookupVec (_ :: xs) (suc i) = lookupVec xs i

-- ex 2.4
putVec : {A : Set} {n : Nat} → Fin n → A → Vec A n → Vec A n
putVec  zero   a (_ :: xs) = a ::            xs
putVec (suc i) a (x :: xs) = x :: putVec i a xs

ex-vxs  : Vec Nat 4
ex-vxs  = 0 :: 1 :: 2 :: 3 :: []

lookupVecex-vxs0 : IsTrue (lookupVec ex-vxs                zero    =Nat 0)
lookupVecex-vxs0 = is-true
lookupVecex-vxs3 : IsTrue (lookupVec ex-vxs (suc (suc (suc zero))) =Nat 3)
lookupVecex-vxs3 = is-true

putVecex-vxs3 : IsTrue (putVec (suc (suc zero)) 22 ex-vxs  =Vec-Nat (0 :: 1 :: 22 :: 3 :: []))
putVecex-vxs3 = is-true

{-
dependent pair type - aka Σ type

aka Σ type because can be seen as sum (or disjoint union) of all the types B x
- where 1st component is viewed as a label indicating
  which type of the family the 2nd belong to

generalization of pair type A × B
- where type of 2nd component can be different depending on value of 1st
- e.g.,                   Σ Nat (Vec Bool)
        (or equivalently, Σ Nat (λn → Vec Bool n))
 contains elements
        2 , (true :: false :: [])
 and
        0 , []
 but not
        2 , [] (since [] does not have type Vec Bool 2).
-}

--               function
--                  v
data Σ (A : Set) (B : A → Set) : Set where
  _,_ : (x : A) → B x → Σ A B

-- see Σ as generalization of normal pair : normal pair ignores its input
_×'_ : (A B : Set) → Set
A ×' B = Σ A (λ _ → B)

-- ex 2.5
×-to-×' : {A B : Set} → (A × B) → (A ×' B)
×-to-×' (a , b) =  a , b

×'-to-× : {A B : Set} → (A ×' B) → (A × B)
×'-to-× (a , b) =  a , b

-- Σ USE CASE : hide type info

-- e.g., hide length of vector

List' : (A : Set) → Set
List' A = Σ Nat (Vec A)

-- ex 2.6
list-to-vec : {A : Set} → (l : List A) → Vec A (length l)
list-to-vec       []  = []
list-to-vec (x :: xs) = x :: list-to-vec xs

vec-to-list : {A : Set} {n : Nat} → Vec A n → List A
vec-to-list       []  = []
vec-to-list (x :: xs) = x :: vec-to-list xs

list-to-list' : {A : Set} → List A → List' A
list-to-list' l = length l , list-to-vec l

list'-to-list : {A : Set} → List' A → List A
list'-to-list (_ , v) = vec-to-list v

-- if Nat relations existed at this point this function would be very different
lookup-List' : {A : Set} → List' A -> Nat -> Maybe A
lookup-List' (_ ,       [])       _  =  nothing
lookup-List' (_ , (x ::  _))  zero   = just x
lookup-List' (_ , (_ :: xs)) (suc n) = lookup-List' ((list-to-list' (vec-to-list xs))) n

ex-l'xs  : List' Nat
ex-l'xs  = (4 , 0 :: 1 :: 2 :: 3 :: [])

lookup-List'[]0      : IsTrue (lookup-List' (0 , []) 0 =Maybe-Nat nothing)
lookup-List'[]0      = is-true
lookup-List'ex-l'xs0 : IsTrue (lookup-List'  ex-l'xs 0 =Maybe-Nat (just 0))
lookup-List'ex-l'xs0 = is-true
lookup-List'ex-l'xs1 : IsTrue (lookup-List'  ex-l'xs 1 =Maybe-Nat (just 1))
lookup-List'ex-l'xs1 = is-true
lookup-List'ex-l'xs9 : IsTrue (lookup-List'  ex-l'xs 9 =Maybe-Nat nothing)
lookup-List'ex-l'xs9 = is-true

------------------------------------------------------------------------------
-- 3 The Curry-Howard correspondence
-- can interpret logical propositions — such as “P and Q”, “not P”, “P implies Q”, ...
-- as types whose inhabitants are valid proofs of that proposition.

--------------------------------------------------
-- PROPOSITIONAL LOGIC

-- ex 3.1
data Either (A B : Set) : Set where
  left  : A → Either A B
  right : B → Either A B

cases : {A B C : Set} → Either A B → (A → C) → (B → C) → C
cases (left  a) fac   _ = fac a
cases (right b) _   fbc = fbc b

cases-left  : IsTrue (cases (left  3) (2 +_) (2 *_) =Nat 5)
cases-left  = is-true
cases-right : IsTrue (cases (right 3) (2 +_) (2 *_) =Nat 6)
cases-right = is-true

{-
-------------------------
TRUTH : true : the proposition that always holds no matter what.
Proving it is straightforward: do not need to provide any assumptions.
Assuming true in a proof does not provide any new information.
'true' corresponds to the unit type
-}

data ⊤ : Set where
  tt : ⊤

{-
-------------------------
FALSITY. the proposition that is never true.
There are no ways to prove it.
Represented by empty type : datatype with no constructors
-}

data ⊥ : Set where

{-
given a proof p of “false” (which can't happen because no constructors)
- “ex falso quodlibet” : “from falsity follows anything”)
- can can get a proof of any proposition
-}
absurd : {A : Set} → ⊥ → A
absurd ()

{-
-------------------------
NEGATION : the type P → ⊥
EQUIVALENCE. “P is equivalent to Q” as (P → Q) × (Q → P)

Propositional logic versus boolean logic.

types ⊤ and ⊥ seem similar to booleans true and false
- true and false are VALUES : can manipulate and return
- ⊤    and and ⊥ are TYPES
  - not possible to check whether a given type ⊤ or ⊥
-}

curryCompose : {P Q R : Set} → (P → Q) × (Q → R) -> (P → R)
curryCompose (f , g) = λ x → g (f x)

-- ex 3.2
if-A-then-B-implies-A : {A B : Set} → A → (B → A)
if-A-then-B-implies-A a = λ _ → a

if-A-and-true-then-A-or-false : {A : Set} → (A × ⊤) → Either A ⊥
if-A-and-true-then-A-or-false (a , ⊤) = left a

uncurry : {A B C : Set} → (A → (B → C)) → (A × B) → C
uncurry f = λ a×b → f (fst a×b) (snd a×b)

ex32x : {A B C : Set} → (A × Either B C) → Either (A × B) (A × C)
ex32x (a , left  b) = left  (a , b)
ex32x (a , right c) = right (a , c)

ex32y : {A B C D : Set} → ((A → C) × (B → D)) → (A × B) → (C × D)
ex32y (ac , bd) (a , b) = ac a , bd b

{-
--------------------------------------------------
-- PREDICATE LOGIC

to prove propositions that say something about a given VALUE or FUNCTION

e.g., 6 is even
      length (map f xs) is equal to length xs for all xs
      there exists a number n such that n + n = 12

Curry-Howard : propositions are types

can define new propositions by defining new data types

e.g.,
-}

--           index
--             v
data IsEven : Nat → Set where
  even-zero :                        IsEven  zero
  even-suc2 : {n : Nat} → IsEven n → IsEven (suc (suc n))

6-is-even : IsEven 6
6-is-even = even-suc2 (even-suc2 (even-suc2 even-zero))

7-is-not-even : IsEven 7 → ⊥
7-is-not-even (even-suc2 (even-suc2 (even-suc2 ())))
--                                             ^   -- absurb pattern as arg

{-
-- useful predicate
-- states that a given Bool is true
data IsTrue : Bool → Set where
  is-true : IsTrue true

_=Nat_ : Nat → Nat → Bool
zero    =Nat  zero   = true
(suc x) =Nat (suc y) = x =Nat y
_       =Nat  _      = false
-}

length-is-3 : IsTrue (length (1 :: 2 :: 3 :: []) =Nat 3)
length-is-3 = is-true

{-
Defining properties as functions.

Can define properties as
- data types, or
- functions that return a value of type Set
e..g.,
-}

IsTrue’ : Bool → Set
IsTrue’ true  = ⊤
IsTrue’ false = ⊥

{-
Function approach often results in proofs that are shorter, but less readable.

Also less general, as some types (e.g., identity type in next section)
can only be defined as a data type.

BEST PRACTICE: use data types (not functions)

--------------------------------------------------
UNIVERSAL QUANTIFICATION : “∀ x of type A, P(x)”

to prove : provide proof of P(v) for EVERY concrete value v : A.

i.e., a function λv → P v

in opposite direction
assume a f of “∀ x of type A, P(x)”
and given a concrete v : A
then can the proof to the case of v to get a proof f v of P(v)

under Curry-Howard, universal quantification corresponds to dependent function
  (x : A) → P x
-}

double : Nat → Nat
double  zero   = zero
double (suc n) = suc (suc (double n))

-- ∀ n : Nat, double n is an even number
-- 'double-is-even' is a dependent function : return TYPE depends on input VALUE
-- pattern matching here is "proof by cases"
-- recursion here is "induction on n"
double-is-even : (n : Nat) → IsEven (double n)
double-is-even  zero   = even-zero
double-is-even (suc m) = even-suc2 (double-is-even m)

n-equals-n : (n : Nat) → IsTrue (n =Nat n)
n-equals-n  zero   = is-true
n-equals-n (suc m) = n-equals-n m

{-
--------------------------------------------------
EXISTENTIAL QUANTIFICATION : “∃ a x : A such that P( x )”

to prove : provide a concrete v : A, and a proof that P(v) holds

i.e. a pair (v : A, P v)

in opposite direction
given proof z of “∃ a x : A such that P( x )”
then extract the witness fst z : A
and the proof snd z : P (fst z)

under Curry-Howard, existential quantification corresponds to the dependent pair type
  Σ A ( λ x → P x ).
-}

-- ∃ an n such that n + n = 12
half-a-dozen : Σ Nat (λ n → IsTrue ((n + n) =Nat 12))
half-a-dozen = 6 , is-true

-- any number n is either 0 or the successor of another number m
-- ∀ n, n is 0
-- or ∃ m such that n is suc m
zero-or-suc : (n : Nat) → Either (IsTrue (n =Nat 0))
                                 (Σ Nat (λ m → IsTrue (n =Nat (suc m))))
zero-or-suc  zero   = left is-true
zero-or-suc (suc m) = right (m , n-equals-n m)

{-
--------------------------------------------------

Propositional logic                              Type System
------------------------------------------------------------
proposition                  P                   type
proof of a proposition       p : P               program of a type
conjunction                  P × Q               pair type
disjunction                  Either P Q          either type
implication                  P → Q               function type
truth                        ⊤                   unit type
falsity                      ⊥                   empty type
negation                     P → ⊥               function to ⊥
equivalence                  (P → Q) × (Q → P)   pair of two functions
------------------------------------------------------------
Predicate log
universal quantification     (x : A) → P x       dependent function type
existential quantification   Σ A (λ x → P x )    dependent pair type


--------------------------------------------------
-- IDENTITY TYPE : EQUALITY at any type (to avoid =Nat, =Vec-Nat, ...)

Martin-Löf introduced a new type x ≡ y, called the IDENTITY TYPE (NOT FUNCTION)

if x and y are equal, then x ≡ y has a single inhabitant refl
- behaves like the unit type ⊤

if x and y are distinct, then x ≡ y has no constructors
- behaves like the empty type ⊥
-}

data _≡_ {A : Set} : A → A → Set where
  -- ‘reflexivity’
  refl : {x : A} → x ≡ x
infix 4 _≡_
{-# BUILTIN EQUALITY _≡_ #-}

one-plus-one-IsTrue : IsTrue ((1 + 1) =Nat 2)
one-plus-one-IsTrue = is-true

one-plus-one : 1 + 1 ≡ 2
one-plus-one = refl

{-
zero-not-one-not-IsTrue : IsTrue (0 =Nat 1)
zero-not-one-not-IsTrue = {!!}
-}

zero-not-one : 0 ≡ 1 → ⊥
zero-not-one ()

-- prove facts about polymorphic types
id-returns-input : {A : Set}
                 → (x : A)
                 → id x ≡ x
id-returns-input _ = refl

-- unit tests using identity type
length-test1 : length (1 :: 2 :: []) ≡ 2
length-test1 = refl

-- symmetry of equality
sym : {A : Set} {x y : A} → x ≡ y → y ≡ x
sym refl = refl

-- transitivity of equality
trans : {A : Set} {x y z : A} → x ≡ y → y ≡ z → x ≡ z
trans refl refl = refl

-- congruence of equality
cong : {A B : Set} {x y : A} → (f : A → B) → x ≡ y → f x ≡ f y
cong f refl = refl

{-
If a and b can be unified by instantiating some of the variables, then can match on refl.
If a and b are different (e.g. different constructors), then match on absurb pattern ().
Agda canNOT always tell if they are different.
-}

------------------------------------------------------------------------------
-- 4 EQUATIONAL REASONING

begin_ : {A : Set} → {x y : A} → x ≡ y → x ≡ y
begin p = p

_end : {A : Set} → (x : A) → x ≡ x
x end = refl

_=⟨_⟩_ : {A : Set} → (x : A) → {y z : A} → x ≡ y → y ≡ z → x ≡ z
x =⟨ p ⟩ q = trans p q

_=⟨⟩_  : {A : Set} → (x : A) → {y   : A} → x ≡ y         → x ≡ y
x =⟨⟩ q = x =⟨ refl ⟩ q

infix 1 begin_
infix 3 _end
infixr 2 _=⟨_⟩_
infixr 2 _=⟨⟩_

-- create singleton list
[_] : {A : Set} → A → List A
[ x ] = x :: []

reverse : {A : Set} → List A → List A
reverse       []  = []
reverse (x :: xs) = reverse xs ++ [ x ]

-- reverse has no effect on singleton lists
reverse-singleton : {A : Set}
                  → (x : A)
                  → reverse [ x ] ≡ [ x ]
reverse-singleton x =
  begin
  reverse       [ x ]  =⟨⟩ -- definition of [_]
  reverse   (x :: [])  =⟨⟩ -- applying reverse (second clause)
  reverse [] ++ [ x ]  =⟨⟩ -- applying reverse (first clause)
          [] ++ [ x ]  =⟨⟩ -- applying _++_
                [ x ]
  end

-- proof by cases and induction
not-not : (b : Bool)
        → not (not b) ≡ b

not-not true  =
  begin
  not (not true) =⟨⟩
       not false =⟨⟩ -- apply inner not
           true      -- apply not
  end

not-not false =
  begin
  not (not false) =⟨⟩
       not true   =⟨⟩
           false
  end

-- prove fact about Nat by induction (i.e., recursion)
add-n-zero : (n : Nat)
           → n + zero ≡ n
add-n-zero  zero   =
  begin
  zero + zero =⟨⟩
  zero        =⟨⟩
  zero
  end

add-n-zero (suc n) =
  begin
  (suc n) + zero  =⟨⟩                          -- applying +
  suc (n  + zero) =⟨ cong suc (add-n-zero n) ⟩ -- using induction hypothesis
  suc n
  end

add-n-zero' : (n : Nat)
            → n + zero ≡ n
add-n-zero'  zero   = refl
add-n-zero' (suc n) = cong suc (add-n-zero' n)

add-assoc : (x y z : Nat)
          → x + (y + z) ≡ (x + y) + z
add-assoc zero y z =
  begin
  zero + (y + z) =⟨⟩ -- def of +
          y + z  =⟨⟩ -- unapply +
  (zero + y) + z
  end
add-assoc (suc x) y z =
  begin
     (suc   x) + (y   + z)  =⟨⟩ -- def of +
      suc  (x  + (y   + z)) =⟨ cong suc (add-assoc x y z) ⟩ -- inductive hypo
      suc ((x  +  y)  + z)  =⟨⟩ -- unapply outer add
     (suc  (x  +  y)) + z   =⟨⟩ -- unapply inner add
    ((suc   x) +  y)  + z
  end

-- ex 4.1
add-suc : (m n : Nat)
        → m + suc n ≡ suc (m + n)
add-suc zero n    =
  begin
  zero + suc n =⟨⟩ -- def of +
         suc n =⟨⟩
  suc (zero + n)
  end
add-suc (suc m) n = -- cong suc (add-suc m n)
  begin
  suc m + suc n    =⟨⟩ -- def of +
  suc (m + suc n)  =⟨ cong suc (add-suc m n) ⟩
  suc (suc m + n)
  end

--use add-suc and add-n-zero
+-comm : (m n : Nat)
       → m + n ≡ n + m
+-comm zero n = sym (add-n-zero n)
+-comm (suc m) n
  rewrite                 -- suc  m     +     n  ≡ n + suc m
                          -- suc (m     +     n) ≡ n + suc m
    +-comm m n            -- suc (n     +     m) ≡ n + suc m
  | sym (add-suc n m)     --      n     + suc m  ≡ n + suc m
  = refl

+-comm' : (m n : Nat)
        → m + n ≡ n + m
+-comm' zero n =
  begin
  zero + n =⟨⟩                     -- def of +
         n =⟨ sym (add-n-zero n) ⟩ -- add zero to right
  n + zero
  end
+-comm' (suc m) n =
  begin
  suc  m     +     n  =⟨⟩ -- def of +
  suc (m     +     n) =⟨ cong suc (+-comm m n) ⟩
  suc (n     +     m) =⟨ sym (add-suc n m) ⟩
       n     + suc m
  end

--------------------------------------------------
-- induction on lists

-- ex 4.2
replicate : {A : Set} → Nat → A → List A
replicate  zero   x = []
replicate (suc n) x = x :: replicate n x

length-replicate : {A : Set} {a : A}
                 → (n : Nat)
                 → length (replicate n a) ≡ n
length-replicate  zero   = refl
length-replicate (suc n) = cong suc (length-replicate n)

-- ex 4.3
append-[] : {A : Set}
          → (xs : List A)
          → xs ++ [] ≡ xs
append-[]       []  = refl
append-[] (x :: xs) = cong (x ::_) (append-[] xs)

append-assoc : {A : Set}
             → (xs ys zs : List A)
             → (xs ++ ys) ++ zs ≡ xs ++ (ys ++ zs)
append-assoc       []  ys zs = refl
append-assoc (x :: xs) ys zs -- (((x ::   xs) ++ ys) ++ zs) ≡ ((x ::  xs) ++ (ys ++ zs))
                             --    x :: ((xs  ++ ys) ++ zs) ≡   x :: (xs  ++ (ys ++ zs))
  = cong (x ::_) (append-assoc xs ys zs)

reverse-distributivity : {A : Set}
                       → (xs ys : List A)
                       → reverse (xs ++ ys) ≡ reverse ys ++ reverse xs
reverse-distributivity       []  ys = sym (append-[] (reverse ys))
reverse-distributivity (x :: xs) ys
                   -- reverse ((x :: xs) ++ ys)         ≡ (reverse ys ++  reverse       (x :: xs))
                   -- (reverse (xs ++ ys) ++ (x :: [])) ≡ (reverse ys ++ (reverse xs ++ (x :: [])))
  rewrite
    reverse-distributivity xs ys
                   -- ((reverse ys ++ reverse xs) ++ (x :: []))
                   --                                   ≡ (reverse ys ++ (reverse xs ++ (x :: [])))
  | sym (append-assoc (reverse ys) (reverse xs) (x :: []))
  = refl

reverse-reverse : {A : Set}
                → (xs : List A)
                → reverse (reverse xs) ≡ xs
reverse-reverse       [] = refl
reverse-reverse (x :: xs) -- reverse (reverse (x :: xs))       ≡ x :: xs
                          -- reverse (reverse xs ++ (x :: [])) ≡ x :: xs
  rewrite
    --                       NOTE
    --                        v
    reverse-distributivity (reverse xs) [ x ]
                          --         x :: reverse (reverse xs) ≡ x :: xs
-- can rewrite here then refl
--| reverse-reverse xs    --                          x :: xs  ≡ x :: xs
--= refl
-- or use 'cong' on rightof '='
  = cong (x ::_) (reverse-reverse xs)

{-
map satisfies functor laws:
- identity    : map id      = id
- composition : map (g . h) = map g . h
-}

map-id : {A : Set}
       → (xs : List A)
       → map id xs ≡ xs
map-id       []  = refl
map-id (x :: xs) --         map id (x :: xs) ≡ x :: xs
                 -- id x :: map id       xs  ≡ x :: xs
  = cong (x ::_) (map-id xs)

_◦_ : {A B C : Set} → (B → C) → (A → B) → (A → C)
g ◦ h = λ x → g (h x)

map-compose : {A B C : Set}
            → (f : B → C) → (g : A → B) → (xs : List A)
            → map (f ◦ g) xs ≡ map f (map g xs)
map-compose f g       [] = refl
map-compose f g (x :: xs)
                         --              map (f ◦ g) (x :: xs) ≡            map f (map g (x :: xs))
                         -- (f ◦ g) x :: map (f ◦ g)       xs  ≡ f (g x) :: map f (map g       xs)
{- this
  rewrite
    sym (map-compose f g xs)
                         --   f (g x) :: map (λ x₁ → f (g x₁)) xs ≡
                              f (g x) :: map (λ x₁ → f (g x₁)) xs
  = refl
-}
  -- or this
  = cong (f (g x) ::_) (map-compose f g xs)

-- ex 4.4
length-map : {A B : Set} {f : A → B}
           → (xs : List A)
           → length (map f xs) ≡ length xs
length-map       []  = refl
length-map (x :: xs) --      length (map f (x :: xs)) ≡      length (x :: xs)
                     -- suc (length (map f       xs)) ≡ suc (length       xs)
  = cong suc (length-map xs)

-- ex 4.5
take : {A : Set} → List A → Nat → List A
take        _   zero   = []
take       []  (suc n) = []
take (x :: xs) (suc n) = x :: take xs n

take-0 : take ex-xs 0 ≡ []
take-0 = refl
take-1 : take ex-xs 1 ≡ 0 :: []
take-1 = refl
take-2 : take ex-xs 2 ≡ 0 :: 1 :: []
take-2 = refl
take-3 : take ex-xs 3 ≡ 0 :: 1 :: 2 :: []
take-3 = refl
take-4 : take ex-xs 4 ≡ ex-xs
take-4 = refl
take-5 : take ex-xs 5 ≡ ex-xs
take-5 = refl

drop : {A : Set} → List A → Nat → List A
drop       xs   zero   = xs
drop       []  (suc x) = []
drop (_ :: xs) (suc n) = drop xs n

drop-0 : drop ex-xs 0 ≡ ex-xs
drop-0 = refl
drop-1 : drop ex-xs 1 ≡ 1 :: 2 :: 3 :: []
drop-1 = refl
drop-2 : drop ex-xs 2 ≡      2 :: 3 :: []
drop-2 = refl
drop-3 : drop ex-xs 3 ≡           3 :: []
drop-3 = refl
drop-4 : drop ex-xs 4 ≡                []
drop-4 = refl
drop-5 : drop ex-xs 5 ≡                []
drop-5 = refl

take-drop : {A : Set} → (xs : List A) → (n : Nat) → take xs n ++ drop xs n ≡ xs
take-drop       []   zero   = refl
take-drop       []  (suc n) = refl
take-drop (x :: xs)  zero   = refl
take-drop (x :: xs) (suc n) --      (take (x :: xs) (suc n) ++ drop (x :: xs) (suc n)) ≡ x :: xs
                            -- x :: (take       xs       n  ++ drop       xs       n)  ≡ x :: xs
  = cong (x ::_) (take-drop xs n)

--------------------------------------------------
-- verifying optimizations

-- list optimization

reverse-acc : {A : Set} → List A → List A → List A
reverse-acc       []  ys = ys
reverse-acc (x :: xs) ys = reverse-acc xs (x :: ys)

reverse' : {A : Set} → List A → List A
reverse' xs = reverse-acc xs []

reverse-acc-lemma : {A : Set}
                  → (xs ys : List A)
                  → reverse-acc xs ys ≡ reverse xs ++ ys
reverse-acc-lemma       []   _ = refl
reverse-acc-lemma (x :: xs) ys   -- reverse-acc (x :: xs) ys  ≡  (reverse (x :: xs)        ++ ys)
                                 -- reverse-acc xs (x :: ys)  ≡ ((reverse xs ++ (x :: [])) ++ ys)
  rewrite
    append-assoc (reverse xs) [ x ] ys
                                 -- reverse-acc xs (x :: ys)  ≡ (reverse xs ++ (x :: ys))
  | reverse-acc-lemma xs (x :: ys)
                                 -- (reverse xs ++ (x :: ys)) ≡ (reverse xs ++ (x :: ys))
  = refl

reverse'-reverse : {A : Set}
                 → (xs : List A)
                 → reverse' xs ≡ reverse xs
reverse'-reverse       []  = refl
reverse'-reverse (x :: xs)     --  reverse' (x :: xs)       ≡  reverse (x :: xs)
                               --  reverse' (x :: xs)       ≡ (reverse xs ++ (x :: []))
  rewrite
    reverse-acc-lemma xs [ x ] -- (reverse xs ++ (x :: [])) ≡ (reverse xs ++ (x :: []))
  = refl

-- tree optimization

data Tree (A : Set) : Set where
  leaf : A               → Tree A
  node : Tree A → Tree A → Tree A

flatten : {A : Set} → Tree A → List A
flatten (leaf a)     = [ a ]
flatten (node t1 t2) = flatten t1 ++ flatten t2

flatten-acc : {A : Set} → Tree A → List A → List A
flatten-acc (leaf x)     xs = x :: xs
flatten-acc (node t1 t2) xs = flatten-acc t1 (flatten-acc t2 xs)

flatten' : {A : Set} → Tree A → List A
flatten' t = flatten-acc t []

flatten-acc-lemma : {A : Set}
                  → (t : Tree A) → (ys : List A)
                  → flatten-acc t ys ≡ flatten t ++ ys
flatten-acc-lemma (leaf x)    _ = refl
flatten-acc-lemma (node t1 t2) ys
                     --  flatten-acc (node t1 t2) ys         ≡  (flatten (node t1 t2)      ++ ys)
                     --  flatten-acc t1 (flatten-acc t2 ys)  ≡ ((flatten t1 ++ flatten t2) ++ ys)
  rewrite
    flatten-acc-lemma t1 (flatten-acc t2 ys)
                     -- (flatten t1 ++   flatten-acc t2 ys)  ≡ ((flatten t1 ++ flatten t2) ++ ys)
  | flatten-acc-lemma t2 ys
                     -- (flatten t1 ++  (flatten t2 ++  ys)) ≡ ((flatten t1 ++ flatten t2) ++ ys)
  | append-assoc (flatten t1) (flatten t2) ys
                     -- (flatten t1 ++  (flatten t2 ++  ys)) ≡  (flatten t1 ++ (flatten t2 ++ ys))
  = refl

flatten-acc-flatten : {A : Set}
                    → (t : Tree A) → (ys : List A)
                    → flatten-acc t ys ≡ flatten t ++ ys
flatten-acc-flatten (leaf x)     ys = refl
                      --  flatten-acc (leaf x)           ys   ≡  (flatten (leaf x)          ++ ys)
flatten-acc-flatten (node t1 t2) ys
                      --  flatten-acc (node t1 t2)       ys   ≡  (flatten (node t1 t2)      ++ ys)
                      --  flatten-acc t1 (flatten-acc t2 ys)  ≡ ((flatten t1 ++ flatten t2) ++ ys)
  rewrite
    flatten-acc-lemma t2 ys
                      --  flatten-acc t1 (flatten t2 ++  ys)  ≡ ((flatten t1 ++ flatten t2) ++ ys)
  | flatten-acc-lemma t1 (flatten t2 ++ ys)
                      -- (flatten t1 ++  (flatten t2 ++  ys)) ≡ ((flatten t1 ++ flatten t2) ++ ys)

  | append-assoc (flatten t1) (flatten t2) ys
                      -- (flatten t1 ++  (flatten t2 ++  ys)) ≡  (flatten t1 ++ (flatten t2 ++ ys))
  = refl

-- ex 4.6
flatten'-flatten : {A : Set}
                 → (t : Tree A)
                 → flatten' t ≡ flatten t
flatten'-flatten (leaf x)     = refl
flatten'-flatten (node t1 t2)
                    -- flatten' (node t1 t2)              ≡  flatten (node t1 t2)
                    -- flatten' (node t1 t2)              ≡ (flatten t1 ++ flatten t2)
  rewrite
    sym (flatten-acc-flatten t1 (flatten t2))
                    -- flatten-acc t1 (flatten-acc t2 []) ≡ flatten-acc t1 (flatten t2)
  | flatten-acc-flatten t2 []
                    -- flatten-acc t1 (flatten t2 ++  []) ≡ flatten-acc t1 (flatten t2)
  | append-[] (flatten t2)
                    -- flatten-acc t1 (flatten t2)        ≡ flatten-acc t1 (flatten t2)
  = refl

--------------------------------------------------
-- compiler correctness

data Expr : Set where
  valE : Nat  →        Expr
  addE : Expr → Expr → Expr

eval : Expr → Nat
eval (valE x)     = x
eval (addE e1 e2) = eval e1 + eval e2

data Op : Set where
  PUSH : Nat → Op
  ADD  : Op

Stack = List Nat
Code  = List Op

exec : Code → Stack → Stack
exec []                       s  = s
exec (PUSH x :: c)            s  = exec c (x :: s)
exec (ADD :: c)    (m :: n :: s) = exec c (n + m :: s)
exec (ADD :: c)               _  = []

compile' : Expr → Code → Code
compile' (valE x) c = PUSH x :: c
compile' (addE e1 e2) c = compile' e1 (compile' e2 (ADD :: c))

compile : Expr → Code
compile e = compile' e []

compile'-exec-eval : (e : Expr) → (s : Stack) → (c : Code)
                   → exec (compile' e c) s ≡ exec c (eval e :: s)
compile'-exec-eval (valE _)     _ _ = refl
compile'-exec-eval (addE e1 e2) s c
        -- exec (compile' (addE e1 e2) c)              s ≡ exec c (eval (addE e1 e2) :: s)
        -- exec (compile' e1 (compile' e2 (ADD :: c))) s ≡ exec c (eval e1 + eval e2 :: s)
  rewrite
    compile'-exec-eval e1 s (compile' e2 (ADD :: c))
        -- exec (compile' e2 (ADD :: c)) (eval e1 :: s)  ≡ exec c (eval e1 + eval e2 :: s)
  | compile'-exec-eval e2 (eval e1 :: s) (ADD :: c)
        -- exec c (eval e1 + eval e2 :: s)               ≡ exec c (eval e1 + eval e2 :: s)
  = refl

compile-exec-eval : (e : Expr) → exec (compile e) [] ≡ [ eval e ]
compile-exec-eval e = compile'-exec-eval e [] []
