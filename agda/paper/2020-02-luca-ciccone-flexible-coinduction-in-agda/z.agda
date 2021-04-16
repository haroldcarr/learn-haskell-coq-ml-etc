module z where

------------------------------------------------------------------------------
-- inductive data types and function that are defined by pattern matching

data ℕ : Set where
  zero : ℕ
  suc  : ℕ → ℕ

_+_ : ℕ → ℕ → ℕ
zero  + n =          n
suc m + n = suc (m + n)

data List {ℓ} (A : Set ℓ) : Set ℓ where
  []   :                         List A
  _::_ : (x : A) (xs : List A) → List A

_#_ : {A : Set} → List A → List A → List A
[]        # ys =            ys
(x :: xs) # ys = x :: (xs # ys)

sum : List ℕ → List ℕ → List ℕ
sum       []        ys  =                   ys
sum (x :: xs)       []  =  x      ::     xs
sum (x :: xs) (y :: ys) = (x + y) :: sum xs ys

-- inductively defined predicates
-- P over S is a data type S -> Set

data _reverseOf_ {A : Set} : List A → List A → Set where
  rev-Λ :       []  reverseOf  []
  rev-t : {x : A} {xs ys : List A}
        →       xs  reverseOf  ys
        → (x :: xs) reverseOf (ys # (x :: []))

data _⊆_ {A : Set} : List A → List A → Set where
  sub-Λ     : [] ⊆ []
  sub-right : {n : A} {ys xs : List A}
            → ys ⊆ xs →       ys  ⊆ (n :: xs)
  sub-ind   : {n : A} {ys xs : List A}
            → ys ⊆ xs → (n :: ys) ⊆ (n :: xs)

------------------------------------------------------------------------------
-- coinductive records and copattern matching

-- inductive pair
record Pair (A B : Set) : Set where
  constructor _,_
  field
    fst : A
    snd : B

-- coinductive infinite list (e.g., "stream")
record Stream (A : Set) : Set where
  coinductive
  field
    hd :        A
    tl : Stream A
open Stream

-- functions cannot be defined inductively (by pattern matching).
-- use copattern matching [APTS13]
-- - specify how the result of the function will be observed

zeros : Stream ℕ
hd zeros = zero
tl zeros = zeros

natsFrom : ℕ → Stream ℕ
hd (natsFrom n) = n
tl (natsFrom n) = natsFrom (suc n)

sumS : Stream ℕ → Stream ℕ → Stream ℕ
hd (sumS a b) =       hd a + hd b
tl (sumS a b) = sumS (tl a) (tl b)

------------------------------------------------------------------------------
-- possibly infinite streams

--open import Codata.Thunk
open import Size
open import Relation.Unary

{-
A thunk is a coinductive record with only one field, the suspended computation.
Takes a function from Size to Set (e.g., Colist A).
Accessing forces the computation and implicitly decreases the size.
'Size < i' represents all the sizes smaller than i.
-}
record Thunk {ℓ} (F : Size → Set ℓ) (i : Size) : Set ℓ where
  coinductive
  field force : {j : Size< i} → F j
open Thunk public

-- 'Size' represents an approximation level.
-- - Can be ∞.
-- - Can help termination checker by tracking depth of data structures.
-- 'Thunk' simulates laziness
data Colist {a} (A : Set a) (i : Size) : Set a where
  []   : Colist A i
  _::_ : A → Thunk (Colist A) i → Colist A i

{-
Alternative Colist impl: follows pattern used for coinductive types.
To represent a structure which can be either finite or infinite, use a coinductive record
with a field representing the whole observation which can be made on the structure.
This field is typically a variant type, since the observation can take different shapes,
e.g., if colist is non-empty then observe the pair consisting of head and tail, otherwise nothing.
-}
open import Data.Maybe
open import Data.Product

record MyColist (A : Set) : Set where
  constructor CoL_
  coinductive
  field
    list : Maybe (A × MyColist A)

-- Either of these two approaches can be used for possibly infinite structures.

data StreamT {ℓ} (A : Set ℓ) (i : Size) : Set ℓ where
  _::_ : A → Thunk (StreamT A) i → StreamT A i

record MyStream_bis (A : Set) : Set where
  coinductive
  field
    stream : A × (MyStream_bis A)

------------------------------------------------------------------------------
-- equality properties

-- stdlib equality and properties

data _≡_ {a} {A : Set a} (x : A) : A → Set a where
  instance refl : x ≡ x

-- Each property is a function that takes proofs as input and returns a new proof.

sym : ∀ {A : Set} {x y : A} → x ≡ y → y ≡ x
sym refl = refl

trans : ∀ {A : Set} {x y z : A} → x ≡ y → y ≡ z → x ≡ z
trans refl refl = refl

cong : ∀ {A B : Set} (f : A → B) {x y : A} → x ≡ y → f x ≡ f y
cong f refl = refl

subst : ∀ {A : Set} {x y : A} (P : A → Set) → x ≡ y → P x → P y
subst P refl px = px

------------------------------------------------------------------------------
-- Chapter 2 - Inductive reasoning

data _memberOf_ {A : Set} : A → List A → Set where
  mem-h : {x   : A} → {xs : List A} → x memberOf     (x            :: xs)
  mem-t : {x y : A} → {xs : List A} → x memberOf xs → x memberOf(y :: xs)
