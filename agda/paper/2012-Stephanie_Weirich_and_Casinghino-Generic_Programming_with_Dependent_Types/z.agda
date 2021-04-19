{-# OPTIONS --type-in-type #-}


module z where

{-
Generic Programming with Dependent Types
Stephanie Weirich and Chris Casinghino, University of Pennsylvania, {sweirich,ccasin}@cis.upenn.edu

ccasin@cis.upenn.edu

2010 : https://www.seas.upenn.edu/~sweirich/papers/aritygen.pdf
2012 : https://www.cis.upenn.edu/~sweirich/papers/ssgip-journal.pdf

------------------------------------------------------------------------------
-- Abstract and Intro

2 types of genericity
- datatype
  - operate on type structure of data
  - so no need to define for each new type
- arity
  - enables functions to be applied to a variable number of args.

e.g., generalize arity of map
repeat   :: a               → [a]
map      :: (a → b)         → [a] → [b]
zipWith  :: (a → b → c)     → [a] → [b] → [c]
zipWith3 :: (a → b → c → d) → [a] → [b] → [c] → [d]


code        : http://www.seas.upenn.edu/~sweirich/papers/aritygen-lncs.tar.gz
tested with : Agda version 2.2.10

------------------------------------------------------------------------------
2 SIMPLE TYPE-GENERIC PROGRAMMING IN AGDA

using Agda with flags (which implies cannot do proofs)
{-# OPTIONS --type-in-type #-}
–no-termination-check
{-# OPTIONS --no-positivity-check #-}
-}

data Bool : Set where
  true  : Bool
  false : Bool

¬ : Bool → Bool
¬ true = false
¬ false = true

_∧_ : Bool → Bool → Bool
true ∧ true = true
_    ∧ _    = false

if_then_else : ∀ {A : Set} → Bool → A → A → A
if true  then a1 else a2 = a1
if false then a1 else a2 = a2

data ℕ : Set where
  zero :     ℕ
  suc  : ℕ → ℕ
{-# BUILTIN NATURAL ℕ #-}

_+_ : ℕ → ℕ → ℕ
zero  + m =          m
suc n + m = suc (n + m)
infixl 40 _+_

data List (A : Set) : Set where
  []   :              List A
  _::_ : A → List A → List A
infixr 5 _::_ -- https://agda.github.io/agda-stdlib/Data.Vec.Functional.html

replicate : ∀ {A} → ℕ → A → List A
replicate  zero   _ = []
replicate (suc n) x = x :: replicate n x

data Vec (A : Set) : ℕ → Set where
  [] : Vec A zero
  _::_ : ∀ {n} → A → (Vec A n) → Vec A (suc n)

repeat : ∀ {n} {A} → A → Vec A n
repeat {zero}  x = []
repeat {suc n} x = x :: repeat x

------------------------------------------------------------------------------
-- 2.1 BASIC TYPE-GENERIC PROGRAMMING

eq-bool : Bool → Bool → Bool
eq-bool true  true  = true
eq-bool false false = true
eq-bool _     _     = false

eq-nat : ℕ → ℕ → Bool
eq-nat  zero    zero   = true
eq-nat (suc n) (suc m) = eq-nat n m
eq-nat      _       _  = false

{-
Structural equality functions, like above, motivate type-generic programming:
- enables defining functions that observe and use structure of types

In a dependently typed language, type-generic is accomplished using universes [17, 21].

E.G., universe of natural number, boolean and product types
-}

-- datatype 'Type', called a "universe"
data Type : Set where
  TNat  :               Type
  TBool :               Type
  TProd : Type → Type → Type

open import Data.Product

-- define an interpretation function '⟨_⟩'
-- maps elements of this universe to Agda types
⟨_⟩ : Type → Set
⟨ TNat ⟩        = ℕ
⟨ TBool ⟩       = Bool
⟨ TProd t1 t2 ⟩ = ⟨ t1 ⟩ × ⟨ t2 ⟩

geq : (t : Type) → ⟨ t ⟩ → ⟨ t ⟩ → Bool
geq TNat  n1 n2 = eq-nat  n1 n2
geq TBool b1 b2 = eq-bool b1 b2
geq (TProd a b) (a1 , b1) (a2 , b2) = geq a a1 a2 ∧ geq b b1 b2

geqEx : Bool
geqEx = geq (TProd TNat TBool) (1 , false) (1 , false)

{-
------------------------------------------------------------------------------
-- 3 ARITY-GENERIC PROGRAMMING : generalize over number of arguments

e.g., map-like (above), +, foldl, foldr

e.g., generalize following functions into one definition : maps for vectors
-}

-- infix zipping application, pronounced “zap” for “zip with apply”
_⊗_ : {A B : Set} {n : ℕ} → Vec (A → B) n → Vec A n → Vec B n
[] ⊗ [] = []
(a :: As) ⊗ (b :: Bs) = a b :: As ⊗ Bs
infixl 40 _⊗_

map0 : {m : ℕ} {A     : Set} →  A          →                     Vec A m
map0         = repeat

map1 : {m : ℕ} {A B   : Set} → (A → B)     → Vec A m →           Vec B m
map1 f x     = repeat f ⊗ x

map2 : {m : ℕ} {A B C : Set} → (A → B → C) → Vec A m → Vec B m → Vec C m
map2 f x1 x2 = repeat f ⊗ x1 ⊗ x2

{-
Intuitively, each map defined by a application of repeat and n copies of _⊗_.
nvec-map f n v1 v2 ... vn = repeat f ⊗ v1 ⊗ v2 ⊗ ... ⊗ vn

-- recursion on n in accumulator style
-- after repeating f :  have a vector of functions
-- then zap this vector across n argument vectors
-- NEEDS type declaration - given below (in Arity section)
nvec-map n f = g n (repeat f)
 where
  g0 a = a
  g (suc n) f = (λ a → g n (f ⊗ a))

--------------------------------------------------
3.1 Typing Arity-Generic Vector Map

arity-generic map : instances have different types

Given arity n, generate corresponding type in the sequence

Part of the difficulty is that generic function is curried in both its type and term args.

This subsection starts with an initial def that takes
- all of the type args in a vector,
- curries term arguments

next subsection, shows how to uncurry type args

ℕ for arity

store types in vector of Agda types, Bool :: N :: [].

vector has type Vec Set 2, so can use standard vector operations (such as _⊗_)

(given in OPTIONS at top of file)
--type-in-type : gives Set the type Set
- simplifies presentation by hiding Agda’s infinite hierarchy of Set levels
- at cost of making Agda’s logic inconsistent
-}

-- folds arrow type constructor (→) over vector of types
-- used to construct the type of 1sr arg to nvec-map
arrTy : {n : ℕ} → Vec Set (suc n) → Set
arrTy {0} (A :: []) = A
arrTy {suc n} (A :: As) = A → arrTy As

arrTyTest : Set
arrTyTest = arrTy (ℕ :: ℕ :: Bool :: [])
-- C-c C-n arrTyTest
-- ℕ → ℕ → Bool

-- Constructs result type of arity-generic map for vectors.
-- Map Vec constructor onto the vector of types, then placing arrows between them.
-- There are two indices:
-- - n : number of types (the arity)
-- - m : length of vectors to be mapped over
arrTyVec : {n : ℕ} → ℕ → Vec Set (suc n) → Set
arrTyVec m As = arrTy (repeat (λ A → Vec A m) ⊗ As)

{- alternate type sigs of previous mapN examples:
map0' : {m : ℕ} {A     : Set} → arrTy (A :: [])           → arrTyVec m (A :: [])
map1' : {m : ℕ} {A B   : Set} → arrTy (A :: B :: [])      → arrTyVec m (A :: B :: [])
map2' : {m : ℕ} {A B C : Set} → arrTy (A :: B :: C :: []) → arrTyVec m (A :: B :: C :: [])
-}

nvec-map : {m : ℕ}
         → (n : ℕ) → {As : Vec Set (suc n)}
         → arrTy As → arrTyVec m As
nvec-map n f = g n (repeat f)
 where
  g : {m : ℕ}
    → (n : ℕ) → {As : Vec Set (suc n)}
    → Vec (arrTy As) m → arrTyVec m As
  g      0  {A :: []} a = a
  g (suc n) {A :: As} f = (λ a → g n (f ⊗ a))

nvec-map-Test : Vec ℕ 2 -- 11 :: 15 :: []
nvec-map-Test = nvec-map 1 { ℕ :: ℕ :: [] } (λ x → 10 + x) (1 :: 5 :: [])

{-
annoying : must explicitly supply types
next section : enable inference

--------------------------------------------------
3.2 A Curried Vector Map

Curry type args so they are supplied individually (rather than a ector).
Enables inference (usually).
-}

-- quantify : creates curried version of a type which depends on a vector
∀⇒ : {n : ℕ} {A : Set} → (Vec A n → Set)
   → Set
∀⇒ {zero}       B = B []
∀⇒ {suc n} {A}  B = (a : A) → ∀⇒ {n} (λ as → B (a :: as))

-- curry : creates curried version of a corresponding function term
λ⇒ : {n : ℕ} {A : Set} {B : Vec A n → Set}
   → ((X : Vec A n) → B X)
   → (∀⇒ B)
λ⇒ {zero}       f = f []
λ⇒ {suc n} {A}  f = (λ a → λ⇒ {n} (λ as → f (a :: as))) -- **** HERE
-- 'a' is implicit in paper
-- Victor says 'hidden' lambdas are magic.
-- see: Eliminating the problems of hidden-lambda insertion
--      https://www.cse.chalmers.se/~abela/MScThesisJohanssonLloyd.pdf

-- uncurry (from 2010 paper)
/⇒ : (n : ℕ) → {K : Set} {B : Vec K n → Set}
   → (∀⇒ B)
   → (A : Vec K n)
   → B A
/⇒ (zero)  f       []  = f
/⇒ (suc n) f (a :: as) = /⇒ n (f a) as

-- arity-generic map
nmap : {m : ℕ}
     → (n : ℕ) -- specifies arity
     → ∀⇒ (λ (As : Vec Set (suc n)) → arrTy As → arrTyVec m As)
nmap {m} n = λ⇒ (λ As → nvec-map {m} n {As})

{-
nmap 1
has type
{m : N} → {A B : Set} → (A → B) → (Vec A m) → (Vec B m)
C-c C-d
(a a₁ : Set) → (a → a₁) → arrTyVec _m_193 (a :: a₁ :: [])


nmap 1 (λ x → 10 + x) (10 :: 5 :: [])
evaluates to
11 :: 15 :: []
C-c C-d
(x : ℕ) → ℕ !=< Set
when checking that the expression λ x → 10 + x has type Set

nmap 2
has type
{m : N} → {ABC : Set} → (A → B → C) → Vec A m → Vec B m → Vec C m
C-c C-d
(a a₁ a₂ : Set) → (a → a₁ → a₂) → arrTyVec _m_193 (a :: a₁ :: a₂ :: [])

nmap 2 ( , ) (1 :: 2 :: 3 :: []) (4 :: 5 :: 6 :: [])
evaluates to
(1 , 4) :: (2 , 5) :: (3 , 6) :: []

now : do not need to explicitly specify type of data in vectors
-}
