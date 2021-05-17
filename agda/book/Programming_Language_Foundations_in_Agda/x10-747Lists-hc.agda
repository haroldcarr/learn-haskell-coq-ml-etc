module x10-747Lists-hc where

-- Library

import Relation.Binary.PropositionalEquality as Eq
open Eq                         using (_≡_; refl; sym; trans; cong)
open Eq.≡-Reasoning
open import Data.Bool           using (Bool; true; false; T; _∧_; _∨_; not)
open import Data.Nat            using (ℕ; zero; suc; _+_; _*_; _∸_; _≤_; s≤s; z≤n)
open import Data.Nat.Properties using (+-assoc; +-identityˡ; +-identityʳ; *-assoc; *-identityˡ; *-identityʳ)
open import Relation.Nullary    using (¬_; Dec; yes; no)
open import Data.Product        using (_×_; ∃; ∃-syntax) renaming (_,_ to ⟨_,_⟩)
open import Data.Sum            using (_⊎_; inj₁; inj₂)
open import Function            using (_∘_)
open import Level               using (Level)
open import Data.Empty          using (⊥)

------------------------------------------------------------------------------
-- Copied from 747Isomorphism.

infix 0 _≃_
record _≃_ (A B : Set) : Set where
  constructor mk-≃  -- This has been added, not in PLFA
  field
    to   : A → B
    from : B → A
    from∘to : ∀ (x : A) → from (to x) ≡ x
    to∘from : ∀ (y : B) → to (from y) ≡ y
open _≃_

infix 0 _≲_
record _≲_ (A B : Set) : Set where
  field
    to      : A → B
    from    : B → A
    from∘to : ∀ (x : A) → from (to x) ≡ x
open _≲_

record _⇔_ (A B : Set) : Set where
  field
    to   : A → B
    from : B → A
open _⇔_

------------------------------------------------------------------------------
-- Polymorphic lists (parameterized version).

data List (A : Set) : Set where
  []  :              List A
  _∷_ : A → List A → List A

infixr 5 _∷_

-- example
_ : List ℕ
_ = 0 ∷ 1 ∷ 2 ∷ []

-- equivalent indexed version
data List' : Set → Set where
  []'  : ∀ {A : Set} →               List' A
  _∷'_ : ∀ {A : Set} → A → List' A → List' A

-- using implicit arguments in above example (why?)
_ : List ℕ
_ = _∷_ {ℕ} 0 (_∷_ {ℕ} 1 (_∷_ {ℕ} 2 ([] {ℕ})))

-- tell Agda to use Haskell lists internally
{-# BUILTIN LIST List #-}

-- useful syntax
pattern [_] z = z ∷ []
pattern [_,_] y z = y ∷ z ∷ []
pattern [_,_,_] x y z = x ∷ y ∷ z ∷ []
pattern [_,_,_,_] w x y z = w ∷ x ∷ y ∷ z ∷ []
pattern [_,_,_,_,_] v w x y z = v ∷ w ∷ x ∷ y ∷ z ∷ []
pattern [_,_,_,_,_,_] u v w x y z = u ∷ v ∷ w ∷ x ∷ y ∷ z ∷ []

-- append for lists
infixr 5 _++_

_++_ : ∀ {A : Set} → List A → List A → List A
[]       ++ ys =            ys
(x ∷ xs) ++ ys = x ∷ (xs ++ ys)

_ : [ 0 , 2 , 4 ] ++ [ 3 , 5 ] ≡ [ 0 , 2 , 4 , 3 , 5 ]
_ = refl

-- associativity of append
++-assoc : ∀ {A : Set} → (xs ys zs : List A) → (xs ++ ys) ++ zs ≡ xs ++ (ys ++ zs)
++-assoc      []  ys zs = refl
++-assoc (x ∷ xs) ys zs rewrite ++-assoc xs ys zs = refl

-- left/right identities for append
++-identityˡ : ∀ {A : Set} → (xs : List A) → [] ++ xs ≡ xs
++-identityˡ xs = refl

++-identityʳ : ∀ {A : Set} → (xs : List A) → xs ++ [] ≡ xs
++-identityʳ      [] = refl
++-identityʳ (x ∷ xs) rewrite ++-identityʳ xs = refl

-- length of a list
length : ∀ {A : Set} → List A → ℕ
length      []  = zero
length (x ∷ xs) = suc (length xs)

_ : length [ 0 , 1 , 2 ] ≡ 3
_ = refl

-- reasoning about length.
length-++ : ∀ {A : Set} → (xs ys : List A) → length (xs ++ ys) ≡ length xs + length ys
length-++      []  ys = refl
length-++ (x ∷ xs) ys rewrite length-++ xs ys = refl

-- quadratic time reverse using structural recursion
reverse : ∀ {A : Set} → List A → List A
reverse      []  = []
reverse (x ∷ xs) = reverse xs ++ [ x ]

_ : reverse [ 0 , 1 , 2 ] ≡ [ 2 , 1 , 0 ]
_ = refl

-- 747/PLFA exercise: RevCommApp (1 point)
-- reverse commutes with ++
-- https://gist.github.com/pedrominicz/012e842362f6c65361722ed1aaf10178
-- The key to this one is NOT splitting ys.
reverse-++-commute : ∀ {A : Set}
  → (xs ys : List A)
  → reverse (xs ++ ys) ≡ reverse ys ++ reverse xs
reverse-++-commute      []  ys  rewrite ++-identityʳ (reverse ys) = refl
reverse-++-commute (x ∷ xs) ys
-- reverse   ((x ∷ xs) ++ ys)          ≡ reverse ys ++ reverse (x ∷ xs)
-- reverse        (xs  ++ ys) ++ [ x ] ≡ reverse ys ++ reverse      xs ++ [ x ]
  rewrite
    reverse-++-commute xs ys
-- (reverse ys ++ reverse xs) ++ [ x ] ≡ reverse ys ++ reverse      xs ++ [ x ]
  | ++-assoc (reverse ys) (reverse xs) [ x ]
--  reverse ys ++ reverse xs  ++ [ x ] ≡ reverse ys ++ reverse      xs ++ [ x ]
  = refl

-- NOT USED
snoc : {A : Set} → List A → A → List A
snoc     []  x = x ∷ []
snoc (y ∷ l) x = y ∷ (snoc l x)

-- NOT USED
snoc≡app : {A : Set} → (l : List A) → (a : A) → snoc l a ≡ l ++ [ a ]
snoc≡app     []  a = refl
snoc≡app (x ∷ l) a rewrite (snoc≡app l a) = refl

-- NOT USED
reverse-snoc : ∀ {A : Set} → (xs : List A) → List A
reverse-snoc      []  = []
reverse-snoc (x ∷ xs) = snoc (reverse-snoc xs) x

_ : reverse-snoc [ 0 , 1 , 2 ] ≡ [ 2 , 1 , 0 ]
_ = refl

-- NOT USED
reverse≡reverse-snoc : ∀ {A : Set} → (xs : List A) → reverse xs ≡ reverse-snoc xs
reverse≡reverse-snoc      [] = refl
reverse≡reverse-snoc (x ∷ xs)     -- reverse (x ∷ xs)         ≡ reverse-snoc  (x ∷ xs)
                                  -- reverse      xs ++ [ x ] ≡ snoc (reverse-snoc xs) x
  rewrite
    reverse≡reverse-snoc xs       -- reverse-snoc xs ++ [ x ] ≡ snoc (reverse-snoc xs) x
  | snoc≡app (reverse-snoc xs) x  -- reverse-snoc xs ++ [ x ] ≡ reverse-snoc xs ++   [ x ]
  = refl

-- 747/PLFA exercise: RevInvol (1 point)
-- Reverse is its own inverse.
reverse-involutive : ∀ {A : Set} → (xs : List A) → reverse (reverse xs) ≡ xs
reverse-involutive      []  = refl
reverse-involutive (x ∷ xs)                 -- reverse (reverse (x ∷ xs))         ≡ x ∷ xs
                                            -- reverse (reverse      xs ++ [ x ]) ≡ x ∷ xs
  rewrite
    (reverse-++-commute (reverse xs) [ x ]) -- x ∷ reverse (reverse xs)           ≡ x ∷ xs
  | reverse-involutive xs                   -- x ∷ xs                             ≡ x ∷ xs
  = refl

-- towards more efficient linear time reverse

-- generalization of reverse
shunt : ∀ {A : Set} → List A → List A → List A
shunt      []  ys =               ys
shunt (x ∷ xs) ys = shunt xs (x ∷ ys)

-- explanation of what shunt is doing
shunt-reverse : ∀ {A : Set} → (xs ys : List A) → shunt xs ys ≡ reverse xs ++ ys
shunt-reverse      []       ys  = refl
shunt-reverse (x ∷ xs)      [] -- shunt (x ∷ xs)   []    ≡ reverse (x ∷ xs)           ++ []
                               -- shunt      xs    [ x ] ≡ (reverse     xs  ++ [ x ]) ++ []
  rewrite
   ++-identityʳ (reverse     xs  ++ [ x ])
                               -- shunt      xs    [ x ] ≡ reverse xs ++ [ x ]
  | shunt-reverse xs [ x ]     -- reverse    xs ++ [ x ] ≡ reverse xs ++ [ x ]
  = refl
shunt-reverse (x ∷ xs) (y ∷ ys)
                           -- shunt (x ∷ xs)      (y ∷ ys) ≡ reverse (x ∷ xs)          ++ y ∷ ys
                           -- shunt      xs   (x ∷ y ∷ ys) ≡ (reverse     xs ++ [ x ]) ++ y ∷ ys
  rewrite
    shunt-reverse xs (x ∷ y ∷ ys)
                           -- reverse    xs ++ x ∷ y ∷ ys  ≡ (reverse     xs ++ [ x ]) ++ y ∷ ys
  | ++-assoc (reverse xs) [ x ] (y ∷ ys)
                           -- reverse    xs ++ x ∷ y ∷ ys  ≡  reverse     xs ++   x ∷     y ∷ ys
  = refl

-- linear reverse is a special case of shunt
reverse' : ∀ {A : Set} → List A → List A
reverse' xs = shunt xs []

_ : reverse' [ 0 , 1 , 2 ] ≡ [ 2 , 1 , 0 ]
_ = refl

-- prove quadratic and linear reverse are equivalent
reverses : ∀ {A : Set} → (xs : List A) → reverse' xs ≡ reverse xs
reverses      []  = refl
reverses (x ∷ xs)                -- reverse' (x ∷ xs)   ≡ reverse (x ∷ xs)
                                 -- reverse' (x ∷ xs)   ≡ reverse xs ++ [ x ]
  rewrite shunt-reverse xs [ x ] -- reverse xs ++ [ x ] ≡ reverse xs ++ [ x ]
  = refl

-- common higher-order list functions

map : ∀ {A B : Set} → (A → B) → List A → List B
map f [] = []
map f (x ∷ xs) = f x ∷ map f xs

_ : map suc [ 0 , 1 , 2 ] ≡ [ 1 , 2 , 3 ]
_ = refl

-- 747/PLFA exercise: MapCompose (1 point)
-- The map of a composition is the composition of maps.
-- Changed from PLFA: some arguments made explicit, uses pointwise equality.
map-compose : ∀ {A B C : Set} (f : A → B) (g : B → C) (xs : List A)
  → map (g ∘ f) xs ≡ (map g ∘ map f) xs
map-compose f g      []  = refl
map-compose f g (x ∷ xs) --              map (g ∘ f) (x ∷ xs) ≡ (map g ∘ map f) (x ∷ xs)
                         -- (g ∘ f) x  ∷ map (g ∘ f)      xs  ≡ (map g ∘ map f) (x ∷ xs)
  rewrite
    map-compose f g xs   --    g (f x) ∷ map g (map f xs)     ≡ g (f x) ∷ map g (map f xs)
  = refl

-- 747/PLFA exercise: MapAppendComm (1 point)
-- The map of an append is the append of maps.
-- Changed from PLFA: some arguments made explicit.
map-++-commute : ∀ {A B : Set} (f : A → B) (xs ys : List A)
 →  map f (xs ++ ys) ≡ map f xs ++ map f ys
map-++-commute f      []  ys = refl
map-++-commute f (x ∷ xs) ys -- map f ((x ∷ xs) ++ ys) ≡ map f (x ∷ xs) ++ map f ys
                             -- f x ∷ map f (xs ++ ys) ≡ f x ∷ map f xs ++ map f ys
  rewrite
    map-++-commute f xs ys   -- f x ∷ map f xs ++ map f ys ≡ f x ∷ map f xs ++ map f ys
  = refl

------------------------------------------------------------------------------

-- PLFA exercise: map over trees
-- trees with leaves of type A and internal nodes of type B
data Tree (A B : Set) : Set where
  leaf : A                       → Tree A B
  node : Tree A B → B → Tree A B → Tree A B

map-Tree : ∀ {A B C D : Set} → (A → C) → (B → D) → Tree A B → Tree C D
map-Tree f g (leaf a)       = leaf (f a)
map-Tree f g (node tl b tr) = node (map-Tree f g tl) (g b) (map-Tree f g tr)

------------------------------------------------------------------------------

-- Fold-right: put operator ⊗ between each list element (and supplied final element).
--             ⊗ is considered right-associative.
-- Fold-right is universal for structural recursion on one argument.

foldr : ∀ {A B : Set} → (A → B → B) → B → List A → B
foldr _⊗_ e [] = e
foldr _⊗_ e (x ∷ xs) = x ⊗ foldr _⊗_ e xs

_ : foldr _+_ 0 [ 1 , 2 , 3 , 4 ] ≡ 10
_ = refl

-- Summing a list using foldr.

sum : List ℕ → ℕ
sum = foldr _+_ 0

_ : sum [ 1 , 2 , 3 , 4 ] ≡ 10
_ = refl

-- PLFA exercise: use foldr to define product on lists of naturals
product : List ℕ → ℕ
product = foldr _*_ 1

_ : product [ 1 , 2 , 3 , 4 ] ≡ 24
_ = refl

-- 747/PLFA exercise: FoldrOverAppend (1 point)
-- prove foldr over an append can be expressed as foldrs over each list.
foldr-++ : ∀ {A B : Set} (_⊗_ : A → B → B) (b : B) (xs ys : List A) →
  foldr _⊗_ b (xs ++ ys) ≡ foldr _⊗_ (foldr _⊗_ b ys) xs
foldr-++ _⊗_ b      []  ys = refl
foldr-++ _⊗_ b (x ∷ xs) ys
  -- foldr _⊗_ b ((x ∷ xs) ++ ys)        ≡ foldr _⊗_ (foldr _⊗_ b ys) (x ∷ xs)
  -- (x ⊗ foldr _⊗_ b (xs ++ ys))        ≡  (x ⊗ foldr _⊗_ (foldr _⊗_ b ys) xs)
  rewrite foldr-++ _⊗_ b xs ys
  -- (x ⊗ foldr _⊗_ (foldr _⊗_ b ys) xs) ≡ (x ⊗ foldr _⊗_ (foldr _⊗_ b ys) xs)
  = refl

-- 747/PLFA exercise: MapIsFoldr (1 point)
-- Show that map can be expressed as a fold.
-- Changed from PLFA: some arguments made explicit, uses pointwise equality.
map-is-foldr : ∀ {A B : Set} (f : A → B) (xs : List A)
  → map f xs ≡ foldr (λ x rs → f x ∷ rs) [] xs
map-is-foldr f      []  = refl
map-is-foldr f (x ∷ xs) rewrite map-is-foldr f xs = refl

-- PLFA exercise: write a fold for trees
fold-Tree : ∀ {A B C : Set} → (A → C) → (C → B → C → C) → Tree A B → C
fold-Tree f g (leaf a)       = f a
fold-Tree f g (node tl b tr) = g (fold-Tree f g tl) b (fold-Tree f g tr)

-- PLFA exercise: the downFrom function computes a countdown list
-- Prove an equality about its sum

downFrom : ℕ → List ℕ
downFrom  zero   = []
downFrom (suc n) = n ∷ downFrom n

_ : downFrom 4 ≡ [ 3 , 2 , 1 , 0 ]
_ = refl

_ : sum (downFrom 4) ≡ 6
_ = refl
{- TODO
sum-downFrom : ∀ (n : ℕ) → sum (downFrom n) * 2 ≡ n * (n ∸ 1)
sum-downFrom n = {!!}
-}

------------------------------------------------------------------------------

-- 'Monoid' : set with
-- - an associative operator
-- - an element which is the left and right identity
record IsMonoid (A : Set) : Set where
  field
    id        : A
    _⊗_       : A → A → A
    assoc     : ∀ (x y z : A) → (x ⊗ y) ⊗ z ≡ x ⊗ (y ⊗ z)
    identityˡ : ∀ (x : A) → id ⊗ x          ≡ x
    identityʳ : ∀ (x : A) → x ⊗ id          ≡ x

-- The following open command is different from PLFA; it uses instance arguments,
-- which work like typeclasses in Haskell (allow overloading, which is cleaner).

open IsMonoid {{ ...}} public

-- These pragmas make displays of goal and context look nicer.
{-# DISPLAY IsMonoid.id  _ = id #-}
{-# DISPLAY IsMonoid._⊗_ _ = _⊗_ #-}

-- instances of Monoid
instance

 +-monoid : IsMonoid ℕ
 IsMonoid.id        +-monoid = 0
 IsMonoid._⊗_       +-monoid = _+_
 IsMonoid.assoc     +-monoid = +-assoc
 IsMonoid.identityˡ +-monoid = +-identityˡ
 IsMonoid.identityʳ +-monoid = +-identityʳ

 *-monoid : IsMonoid ℕ
 IsMonoid.id        *-monoid = 1
 IsMonoid._⊗_       *-monoid = _*_
 IsMonoid.assoc     *-monoid = *-assoc
 IsMonoid.identityˡ *-monoid = *-identityˡ
 IsMonoid.identityʳ *-monoid = *-identityʳ

 ++-monoid : ∀ {A : Set} → IsMonoid (List A)
 IsMonoid.id        ++-monoid = []
 IsMonoid._⊗_       ++-monoid = _++_
 IsMonoid.assoc     ++-monoid = ++-assoc
 IsMonoid.identityˡ ++-monoid = ++-identityˡ
 IsMonoid.identityʳ ++-monoid = ++-identityʳ

-- property of foldr over a monoid
foldr-monoid : ∀ {A : Set} → {{m : IsMonoid A}} →
  ∀ (xs : List A) (y : A)
  → foldr _⊗_ y xs ≡ (foldr _⊗_ id xs) ⊗ y
foldr-monoid {A} ⦃ m ⦄ [] y
  rewrite identityˡ y = refl
foldr-monoid {A} ⦃ m ⦄ (x ∷ xs) y
  with foldr-monoid xs y
... | xxx
  rewrite
      xxx
    | sym (assoc x (foldr _⊗_ id xs) y)
  = refl

foldr-monoid-++ : ∀ {A : Set} → {{m : IsMonoid A}} →
  ∀ (xs ys : List A)
  → foldr _⊗_ id (xs ++ ys) ≡ foldr _⊗_ id xs ⊗ foldr _⊗_ id ys
foldr-monoid-++ {A} ⦃ m ⦄ [] ys
  rewrite
    sym (foldr-monoid {A} {{m}} [] (foldr _⊗_ id ys))
  = refl
foldr-monoid-++ {A} ⦃ m ⦄ (x ∷ xs) ys
  rewrite
    foldr-monoid-++ {A} {{m}} xs ys
  | assoc x (foldr _⊗_ id xs) (foldr _⊗_ id ys)
  = refl

-- 747/PLFA exercise: Foldl (1 point)
-- Define foldl, which associates left instead of right, e.g.
--   foldr _⊗_ e [ x , y , z ]  =  x ⊗ (y ⊗ (z ⊗ e))
--   foldl _⊗_ e [ x , y , z ]  =  ((e ⊗ x) ⊗ y) ⊗ z

foldl : ∀ {A B : Set} → (B → A → B) → B → List A → B
foldl _⊗_ e      []  = e
foldl _⊗_ e (x ∷ xs) = foldl _⊗_ (e ⊗ x) xs

sum-foldl : foldl _+_ 0 [ 4 , 3 , 2 , 1 ] ≡ 10
sum-foldl = refl

monus-foldl : foldl _∸_ 20 [ 4 , 3 , 2 ] ≡ 11
monus-foldl = refl

monus-foldr : foldr _∸_ 20 [ 4 , 3 , 2 ] ≡ 1
monus-foldr = refl

-- 747/PLFA exercise: FoldrMonFoldl (2 points)
-- Show that foldr and foldl compute the same value on a monoid
-- when the base case is the identity.
-- Hint: generalize to when the base case is an arbitrary value.

foldl-r-mon-helper : ∀ {A : Set} {{m : IsMonoid A}}
  → ∀ (xs : List A) (y : A)
  → foldl _⊗_ y xs ≡ y ⊗ foldl _⊗_ id xs
foldl-r-mon-helper      []  y rewrite identityʳ y = refl
foldl-r-mon-helper (x ∷ xs) y      -- foldl _⊗_ y (x ∷ xs) ≡ (y ⊗ foldl _⊗_ id (x ∷ xs))
                                   -- foldl _⊗_ (y ⊗ x) xs ≡ (y ⊗ foldl _⊗_ (id ⊗ x) xs)
  rewrite
    identityˡ x                    --             foldl _⊗_ (y ⊗ x) xs ≡ (y ⊗ foldl _⊗_ x xs)
  | foldl-r-mon-helper xs (y ⊗ x)  -- ((y ⊗ x) ⊗ foldl _⊗_ id xs)     ≡ (y ⊗ foldl _⊗_ x xs)
  | assoc y x (foldl _⊗_ id xs)    -- (y ⊗ (x ⊗ foldl _⊗_ id xs))     ≡ (y ⊗ foldl _⊗_ x xs)
  | foldl-r-mon-helper xs x        -- (y ⊗ (x ⊗ foldl _⊗_ id xs)) ≡ (y ⊗ (x ⊗ foldl _⊗_ id xs))
  = refl

foldl-r-mon : ∀ {A : Set} → {{m : IsMonoid A}}
  → ∀ (xs : List A)  → foldl _⊗_ id xs ≡ foldr _⊗_ id xs
foldl-r-mon      []  = refl
foldl-r-mon (x ∷ xs)
                            -- foldl _⊗_ id (x ∷ xs) ≡ foldr _⊗_ id (x ∷ xs)
                            -- foldl _⊗_ (id ⊗ x) xs ≡ (x ⊗ foldr _⊗_ id xs)
  rewrite
    identityˡ x             -- foldl _⊗_       x  xs ≡ (x ⊗ foldr _⊗_ id xs)
  | foldl-r-mon-helper xs x -- (x ⊗ foldl _⊗_ id xs) ≡ (x ⊗ foldr _⊗_ id xs)
  | foldl-r-mon xs          -- (x ⊗ foldr _⊗_ id xs) ≡ (x ⊗ foldr _⊗_ id xs)
  = refl

------------------------------------------------------------------------------

-- Inductively-defined predicates over lists

-- All P xs means P x holds for every element of xs
data All {A : Set} (P : A → Set) : List A → Set where
  []  :                                            All P []
  _∷_ : ∀ {x : A} {xs : List A} → P x → All P xs → All P (x ∷ xs)

_ : All (_≤ 2) [ 0 , 1 , 2 ]
_ = z≤n ∷ s≤s z≤n ∷ s≤s (s≤s z≤n) ∷ []

-- Any P xs means P x holds for some element of xs
data Any {A : Set} (P : A → Set) : List A → Set where
  here  : ∀ {x : A} {xs : List A} →     P x  → Any P (x ∷ xs)
  there : ∀ {x : A} {xs : List A} → Any P xs → Any P (x ∷ xs)

-- membership in list as application of Any
infix 4 _∈_ _∉_
_∈_ : ∀ {A : Set} (x : A) (xs : List A) → Set
x ∈ xs = Any (x ≡_) xs

_∉_ : ∀ {A : Set} (x : A) (xs : List A) → Set
x ∉ xs = ¬ (x ∈ xs)

_ : 0 ∈ [ 0 , 1 , 0 , 2 ]
_ = here refl

_ : 0 ∈ [ 1 , 2 , 0 ]
_ = there (there (here refl))

not-in : 3 ∉ [ 0 , 1 , 0 , 2 ]
not-in (here ())
not-in (there (here ()))
not-in (there (there (here ())))
not-in (there (there (there (here ()))))

-- The development in PLFA, repeated with our notation.

All-++-⇔ : ∀ {A : Set} {P : A → Set}
         → (xs ys : List A)
         → All P (xs ++ ys) ⇔ (All P xs × All P ys)
to   (All-++-⇔ xs ys) = to'   xs ys
 where
  to' : ∀ {A : Set} {P : A → Set} (xs ys : List A)
      → All P (xs ++ ys) → (All P xs × All P ys)
  to' []       ys = λ All-P-ys   → ⟨ [] , All-P-ys ⟩
  to' (x ∷ xs) ys (Px ∷ All-P-xs++ys) with to' xs ys All-P-xs++ys
  ... | ⟨ All-P-xs , All-PP-ys ⟩ = ⟨ Px ∷ All-P-xs , All-PP-ys ⟩
from (All-++-⇔ xs ys) = from' xs ys
 where
  from' : ∀ { A : Set} {P : A → Set} (xs ys : List A)
        → All P xs × All P ys → All P (xs ++ ys)
  from'      []  ys = λ { ⟨ All-P-[]      , All-P-ys ⟩ → All-P-ys }
  from' (x ∷ xs) ys = λ { ⟨ Px ∷ All-P-xs , All-P-ys ⟩ → Px ∷ from' xs ys ⟨ All-P-xs , All-P-ys ⟩ }

-- PLFA exercise: state and prove Any-++-⇔
Any-++-⇔ : ∀ {A : Set} {P : A → Set}
         → (xs ys : List A)
         → Any P (xs ++ ys) ⇔ (Any P xs ⊎ Any P ys)
to   (Any-++-⇔ xs ys) = to'   xs ys
 where
  to' : ∀ {A : Set} {P : A → Set}
      → (xs ys : List A)
      →  Any P (xs ++ ys)
      → (Any P  xs ⊎ Any P ys)
  to'      []  ys = λ Any-P-ys → inj₂ Any-P-ys
  to' (x ∷ xs) ys (here  Px )  = inj₁ (here Px)
  to' (x ∷ xs) ys (there Any-P-xs++ys) with to' xs ys Any-P-xs++ys
  ... | inj₁ Any-P-xs          = inj₁ (there Any-P-xs)
  ... | inj₂ Any-P-ys          = inj₂        Any-P-ys
from (Any-++-⇔ xs ys) = from' xs ys
 where
  from' : ∀ {A : Set} {P : A → Set}
        → (xs ys : List A)
        → (Any P  xs ⊎ Any P ys)
        →  Any P (xs ++ ys)
  from'      []  ys (inj₂        Any-P-ys)  = Any-P-ys
  from' (x ∷ xs) ys (inj₂        Any-P-ys)  = there (from' xs ys (inj₂ Any-P-ys))
  from' (x ∷ xs) ys (inj₁ (here      Px))   = here Px
  from' (x ∷ xs) ys (inj₁ (there Any-P-xs)) = there (from' xs ys (inj₁ Any-P-xs))

-- use Any-++-⇔ to demonstrate an equivalence relating ∈ and _++_ TODO


-- PLFA exercise: Show that the equivalence All-++-⇔ can be extended to an isomorphism.

-- PLFA exercise: Here is a universe-polymorphic version of composition,
-- and a version of DeMorgan's law for Any and All expressed using it.

_∘'_ : ∀ {ℓ₁ ℓ₂ ℓ₃ : Level} {A : Set ℓ₁} {B : Set ℓ₂} {C : Set ℓ₃}
     → (B → C) → (A → B) → A → C
(g ∘' f) x = g (f x)

¬Any≃All¬ : ∀ {A : Set} (P : A → Set) (xs : List A)
          → (¬_ ∘' Any P) xs ≃ All (¬_ ∘' P) xs
to      (¬Any≃All¬ _      [])  ¬_∘'AnyPxs = []
to      (¬Any≃All¬ P (_ ∷ xs)) ¬_∘'AnyPxs with to (¬Any≃All¬ P xs)
... | AnyPxs→⊥→Allλx₁→Px₁→⊥xs
  = (λ Px → ¬ here Px ∘'AnyPxs) ∷ AnyPxs→⊥→Allλx₁→Px₁→⊥xs (λ AnyPxs → ¬ there AnyPxs ∘'AnyPxs)

from    (¬Any≃All¬ _      [])  All¬_∘'Pxs ()
from    (¬Any≃All¬ P (x ∷ xs)) All¬_∘'Pxs (here     Px) with from (¬Any≃All¬ P xs)
... | All-λx₁→Px₁→⊥-xs→AnyPxs→⊥
  = All-λx₁→Px₁→⊥-xs→AnyPxs→⊥ {!!} {!!}
from    (¬Any≃All¬ _ (x ∷ xs)) All¬_∘'Pxs (there AnyPxs) = {!!}

from∘to (¬Any≃All¬ P xs) = {!!}
to∘from (¬Any≃All¬ P xs) = {!!}
{-
-- Can we prove the following? If not, explain why.
--  ¬All≃Any¬ : ∀ {A : Set} (P : A → Set) (xs : List A)
--    → (¬_ ∘' All P) xs ≃ Any (¬_ ∘' P) xs

-- End of PLFA exercise

-- Decidability of All

-- A Boolean analogue of All

all : ∀ {A : Set} → (A → Bool) → List A → Bool
all p  =  foldr _∧_ true ∘ map p

-- A Dec analogue of All

-- A definition of a predicate being decidable

Decidable : ∀ {A : Set} → (A → Set) → Set
Decidable {A} P  =  ∀ (x : A) → Dec (P x)

All? : ∀ {A : Set} {P : A → Set} → Decidable P → Decidable (All P)
All? P? [] = yes []
All? P? (x ∷ xs) with P? x | All? P? xs
All? P? (x ∷ xs) | yes p | yes p₁ = yes (p ∷ p₁)
All? P? (x ∷ xs) | yes p | no ¬p = no (λ { (x ∷ x₁) → ¬p x₁})
All? P? (x ∷ xs) | no ¬p | _ = no (λ { (x ∷ x₁) → ¬p x})

-- PLFA exercise: repeat above for Any

-- PLFA exercises: All-∀ and Any-∃
-- You will need the stronger version of extensionality
-- (for dependent function types) given in PLFA Isomorphism.

-- PLFA exercise: a version of 'filter' for decidable predicates

-- filter? : ∀ {A : Set} {P : A → Set}
--   → (P? : Decidable P) → List A → ∃[ ys ]( All P ys )
-- filter? P? xs = {!!}
-}
