module z05-00-internal-verification where

open import bool
open import eq
open import nat
open import nat-thms
open import product
open import sum

{-
------------------------------------------------------------------------------
so far: EXTERNAL VERIFICATION
- written programs (e.g., 'length')
- proved properties (e.g., 'length-reverse')

This style of verification in type theory is called external verification
- proofs are external to programs
- proofs are distinct artifacts about some pre-existing programs

INTERNAL VERIFICATION

write functions with semantically expressive types
write datatypes that put restrictions on data
may require embedding proofs in code

------------------------------------------------------------------------------
-- p 99 VECTORS - length of vector included in type : vector is INDEXED by its length

-- vector.agda
-}
data 𝕍 {ℓ} (A : Set ℓ) : ℕ → Set ℓ where
  []   : 𝕍 A 0
  _::_ : {n : ℕ} (x : A) (xs : 𝕍 A n) → 𝕍 A (suc n)

-- compare to list (overloaded constructors OK)
data L {ℓ} (A : Set ℓ) :     Set ℓ where
  []   : L A
  _::_ :         (x : A) (xs : L A)   → L A

infixr 6 _::_ _++𝕍_

-- p 101

[_]𝕍 : ∀ {ℓ} {A : Set ℓ} → A → 𝕍 A 1
[ x ]𝕍 = x :: []

-- type level addition on length
_++𝕍_ : ∀ {ℓ} {A : Set ℓ}{n m : ℕ} → 𝕍 A n → 𝕍 A m → 𝕍 A (n + m)
[]        ++𝕍 ys = ys
(x :: xs) ++𝕍 ys = x :: xs ++𝕍 ys

-- p 102

-- no 'nil' list corner case
head𝕍 : ∀ {ℓ} {A : Set ℓ} {n : ℕ} → 𝕍 A (suc n) → A
head𝕍 (x :: _) = x

-- type level subtraction
tail𝕍 : ∀ {ℓ} {A : Set ℓ} {n : ℕ} → 𝕍 A n → 𝕍 A (pred n)
tail𝕍       []  = []
tail𝕍 (_ :: xs) = xs

-- p 103

-- length preserving (for lists, length preservation is separate proof)
map𝕍 : ∀ {ℓ ℓ'} {A : Set ℓ} {B : Set ℓ'} {n : ℕ} → (A → B) → 𝕍 A n → 𝕍 B n
map𝕍 f       []  = []
map𝕍 f (x :: xs) = f x :: map𝕍 f xs

-- p 104

-- takes a vector of length m
-- each element is vector of length n
-- concats into single vector of length m * n
concat𝕍 : ∀{ℓ} {A : Set ℓ} {n m : ℕ} → 𝕍 (𝕍 A n) m → 𝕍 A (m * n)
concat𝕍       []  = []
concat𝕍 (x :: xs) = x ++𝕍 (concat𝕍 xs)

--  p 104

-- no need for maybe result as in lists by requiring n < m
nth𝕍 : ∀ {ℓ} {A : Set ℓ} {m : ℕ}
  → (n : ℕ)
  → n < m ≡ tt
  → 𝕍 A m
  → A
nth𝕍      0   _ (x :: _) = x
-- Proof p (that index is less than length of vector) reused in recursive call.
-- index is suc n
-- length of list is suc m, for implicit m
-- Agda implicitly introduces .m with suc .m
-- p proves suc n < suc m ≡ tt
-- def/eq to n < m ≡ tt
-- so p has correct type to make the recursive call
nth𝕍 (suc n)  p (_ :: xs) = nth𝕍 n p xs
-- us absurd pattern for the proof in last two cases
-- length of list is zero, so no index can be smaller than that length
-- must case-split on the index so Agda can the absurdity
-- because the definition of _<_ splits on both inputs
-- - returns ff separately for when the first input is is 0 and the second is 0
-- - and  for the first input being suc n and second is 0
nth𝕍 (suc n) ()       []
nth𝕍      0  ()       []

-- p 105

repeat𝕍 : ∀ {ℓ} {A : Set ℓ} → (a : A) (n : ℕ) → 𝕍 A n
repeat𝕍 a      0  = []
repeat𝕍 a (suc n) = a :: (repeat𝕍 a n)

{-
------------------------------------------------------------------------------
-- p 106 BRAUN TREES : balanced binary heaps

either empty or
node consisting of some data x and a left and a right subtree

data may be stored so that x is smaller than all data in left and right subtrees
if such an ordering property is desired

BRAUN TREE PROPERTY (BTP) : crucial property : sizes of left and right trees:

for each node in the tree
- either size (left) = size ( right ) or
         size (left) = size ( right ) + 1

ensures depth of the trees is ≤ log₂(N), where N is the number of nodes

property maintained (via types) during insert

make the type A and ordering on that type be parameters of the module

braun-tree.adga
-}

module braun-tree {ℓ} (A : Set ℓ) (_<A_ : A → A → 𝔹) where

  -- index n is size (number of elements of type A) of the tree
  data braun-tree : (n : ℕ) → Set ℓ where
    bt-empty : braun-tree 0
    bt-node : ∀ {n m : ℕ}
      → A
      → braun-tree n
      → braun-tree m
      → n ≡ m ∨ n ≡ suc m        -- 'v' defined in sum.agda for disjunction of two types
      → braun-tree (suc (n + m))

{- -- p 107
sum.agda

-- types A and B, possibly at different levels, accounted via ⊔ in return type
-- ⊔ part of Agda’s primitive level system : imported from Agda.Primitive module in level.agda
-- use this in code that intended to be run
data _⊎_ {ℓ ℓ'} (A : Set ℓ) (B : Set ℓ') : Set (ℓ ⊔ ℓ') where
  inj₁ : (x : A) → A ⊎ B -- built from an A
  inj₂ : (y : B) → A ⊎ B -- built from an B

-- use this to represent a logical proposition
_∨_ : ∀ {ℓ ℓ'} (A : Set ℓ) (B : Set ℓ') → Set (ℓ ⊔ ℓ')
_∨_ = _⊎_

NO SEMANTIC DIFFERENCE - just different notation to help understanding code
-}

  {-
  --------------------------------------------------
  -- p 107-108 INSERTION
  -- this version keeps smaller (_<A_) elements closer to root when inserting
  -}

  -- type says given BT of size n, returns BT of size suc n
  bt-insert : ∀ {n : ℕ} → A → braun-tree n → braun-tree (suc n)

  -- insert into empty
  -- Create node with element and empty subtrees (both with size 0).
  -- 4th arg to BT constructor is BTP proof
  -- - both 0 so 'refl'
  -- - wrap in inj₁ to say 0 ≡ 0 (not n ≡ suc n)
  bt-insert a bt-empty = bt-node a bt-empty bt-empty (inj₁ refl)

  -- insert info non empty: tree has left and right satisfying BTP
  -- left of size n; right of size m
  -- p is BTP proof
  -- inferred type of return is BT (suc (suc (n + m)))
  -- because type before insert is BT (suc (n + m)) - left plus node element plus right
  -- insert adds ONE, so BT (suc (suc (m + n)))
  bt-insert a (bt-node{n}{m} a' l r p)
    -- regardless of what happens, left and right will be swapped, so size sum will have m first

    -- do rewrite before case splitting on which disjunct of BTP holds (n ≡ m or n ≡ suc m)
    -- does not change structure of tree
    -- will change what proof is used for BTP for new node returned.

    -- case split via WITH on P

    -- could split on p directly in pattern for input BT,
    -- but here rewrite is factored to be done once

    -- could do WITH on an if_then_else_ term, to put the min of element being inserted (a)
    -- and element at current root (a') as 1st component pair (a1), max as 2nd (a2)
    -- want min (a1) to be data at root of new BT
    -- want to insert max (a2) recursively into right
    rewrite +comm n m
    with p | if a <A a' then (a , a') else (a' , a)

  -- inj₁ case
  -- case where p is inj₁ for NEW new pattern variable 'p'
  -- underscore in place of original proof/p
  -- because considering case where original is 'inj₁ p'

  -- p : n ≡ m
  -- so new node
  -- with smaller element a1 at root and then swapped left and update right
  -- has type 'inj₂ refl'

  -- BTP for new node is suc m ≡ n v suc m ≡ suc n
  -- because size of new left is suc m, since it is the updated version of old right
  -- case has proof n ≡ m
  -- rewrite with that proof changes that to suc m ≡ suc n
  -- 'inj 2 refl' proves it
  bt-insert a (bt-node{n}{m} a' l r _) | inj₁ p | (a1 , a2)
    rewrite p = (bt-node a1 (bt-insert a2 r) l (inj₂ refl))

  -- inj₂ case : n ≡ suc m
  -- so need proof suc m ≡ n v suc m ≡ suc n
  -- 'sym p' gives 'suc m ≡ n'
  -- wrap in 'inj₁'
  bt-insert a (bt-node{n}{m} a' l r _) | inj₂ p | (a1 , a2) =
                (bt-node a1 (bt-insert a2 r) l (inj₁ (sym p)))

  {-
  --------------------------------------------------
  -- p 110 REMOVE MIN ELEMENT
  -}

  -- input has at least one element; returns pair of element and a BT one smaller
  bt-remove-min : ∀ {p : ℕ} → braun-tree (suc p) → A × braun-tree p

  -- no need for case of empty input
  -- because size would be 0, but input is 'suc p'

  -- removing sole node; return data and bt-empty
  bt-remove-min (bt-node a bt-empty bt-empty u) = a , bt-empty

  -- next two equations for left is empty and right subtree is a node -- IMPOSSIBLE by BTP
  -- still need to handle both proves with absurd
  bt-remove-min (bt-node a bt-empty (bt-node _ _ _ _) (inj₁ ()))
  bt-remove-min (bt-node a bt-empty (bt-node _ _ _ _) (inj₂ ()))

  -- right empty, left node (implies left size is 1, but not needed)
  -- return data left
  -- need to confirm size relationships satisfied, because
  -- size of input  is suc (suc (n’ + m’) + 0)
  -- size of output is      suc (n’ + m’)
  -- use +0 to drop the '+ 0'
  bt-remove-min (bt-node a (bt-node{n’}{m’} a’ l’ r’ u’) bt-empty u)
    rewrite +0 (n’ + m’) = a , bt-node a’ l’ r’ u’


  -- left and right of input both nodes (not empty)
  -- return data input (the min data)
  -- reassemble output BT: remove min from left:
  bt-remove-min (bt-node a (bt-node a1 l1 r1 u1) (bt-node a2 l2 r2 u2) u)
    with bt-remove-min (bt-node a1 l1 r1 u1)
  -- then match on result of recursive call to bt-remove-min.
  -- produces min a1’ of left and updated left l’
  -- then WITH to pick smaller of a1’ (minimum of left) and a2, minimum of right
  -- similar to the bt-insert with an if_then_else_ term.
  bt-remove-min (bt-node a (bt-node a1 l1 r1 u1) (bt-node a2 l2 r2 u2) u) | a1’ , l’
    with if a1’ <A a2 then (a1’ , a2) else (a2 , a1’)

  -- p 113 first words TODO
  bt-remove-min (bt-node a (bt-node{n1}{m1} a1 l1 r1 u1) (bt-node{n2}{m2} _ l2 r2 u2) u)
    | _ , l’ | smaller , other
    rewrite +suc  (n1 + m1) (n2 + m2) |
            +comm (n1 + m1) (n2 + m2) = a , bt-node smaller (bt-node other l2 r2 u2) l’ (lem u)
    where lem : ∀ {x y}
              → suc x ≡ y ∨ suc x ≡ suc y
              →     y ≡ x ∨     y ≡ suc x
          lem (inj₁ p) = inj₂ (sym          p)
          lem (inj₂ p) = inj₁ (sym (suc-inj p))

{-
------------------------------------------------------------------------------
-- p 114 Sigma Types

Above expresses invariant properties of data using internally verified datatypes.

Any data constructed via the constructors are guaranteed to satisfy the property,
due to restrictions enforced by the constructors.

Different case: state that a property holds of an existing data type.

done using Σ-types (“sigma”)

similar to Cartesian product type A × B (elements of A × B are pairs (a, b))
- but generalization where type of 2nd element can depend on type of 1st
- aka "dependent product type" (though the notation comes from sum types, see below)

see nat-nonzero.agda : type for nonzero natural numbers:

-- a nat 'n' AND a proof 'iszero n ≡ ff'
ℕ⁺ : Set
ℕ⁺ = Σ ℕ (λ n → iszero n ≡ ff)

conceptually similar to Cartesian product : N × (iszero n ≡ ff)
- pair of number and equality proof
- in Cartesian product version, 'n' is free
- Σ-types enable referring to 1st of pair

see product.agda : def of Σ

parametrized by
- type A
- function B
  - input : type A
  - returns a type
- types can be at different levels
- Like sum types, Σ type is then at level ℓ ⊔ ℓ' (least upper bound of the two levels)
data Σ {ℓ ℓ'} (A : Set ℓ) (B : A → Set ℓ') : Set (ℓ ⊔ ℓ') where
  _,_ : (a : A) → (b : B a) → Σ A B
                         ^
               B depends on

------------------------------------------------------------------------------
-- p 115 example: addition on nonzero nats
-}

open import nat-nonzero hiding (_+⁺_)

_+⁺_ : ℕ⁺ → ℕ⁺ → ℕ⁺
(zero         , ()) +⁺ n2 -- cannot happen, so uses absurd pattern
(1            , p1) +⁺ y = suc⁺ y
(suc (suc n1) , p1) +⁺ y = suc⁺ ((suc n1 , refl) +⁺ y) -- recursive call

{-
-- p 115 5.3.1 Why Sigma and Pi?

why Σ symbol for type of dependent pairs?
- because Σ-types generalize disjoint unions
- in math, a disjoint union is union of two sets
  - where elements are tagged to indicate from which set they have come
  - cardinality of A ⊎ B, is sum of cardinalities of A and B
    even if they have a nonempty intersection.
- This is where we get the notation for sum types in Figure 5.2.
- The disjoint union can be defined mathematically as
    ({0} × A) ∪ ({1} × B)
  - each element of union looks like (n, x)
    where if the tag n is 0, then x P A, and
          if         n is 1, then x P B.

------------------------------------------------------------------------------
-- p 116 Binary Search Trees

for some type A and an ordering relation on that type
values in left  subtree always ≤ value at node ℕ
values in right subtree always > value at node ℕ

see z05-01-bst-test.agda
    z05-01-bst.agda

-- p 117-120
TODO : read/understand discussion of
       bool-relations.agda
       relations.agda

------------------------------------------------------------------------------
-- p 123 Internal vs. External Verification

internal verification : datatypes defined with invariants; functions take proofs of preconditions
- Datatypes with essential invariants : enforce via internal
- Complex programs
  - doing external of complex will cause reasoning about complexity
    not relevant to property being proved
  - internal weaves proofs thru code and datatype

external verification : theorems about functions proved separately
- Algebraic Properties e.g., proving associativity
- Functions used in an internal verification's specification
  -- e.g., min/max used in bst - need to externally prove properties about min/max

------------------------------------------------------------------------------
-- p 126 Exercises

1. Nested vector type.
   Fill in the hole to define a type for matrices of nats
   where the type lists the dimensions of the matrix:
-}

-- inner vector is a row
_by_matrix : ℕ → ℕ → Set
numRows by lenRow matrix = 𝕍 (𝕍 ℕ lenRow) numRows

matrix-to-vecvec : ∀ {numRows lenRow : ℕ} → numRows by lenRow matrix → 𝕍 (𝕍 ℕ lenRow) numRows
matrix-to-vecvec 𝕞 = 𝕞

-- 2a
zero-matrix : (numRows : ℕ) → (lenRow : ℕ) → numRows by lenRow matrix
zero-matrix numRows lenRow = repeat𝕍 (repeat𝕍 0 lenRow) numRows
-- 2b
matrix-elt : ∀ {numRows lenRow : ℕ}
  → numRows by lenRow matrix
  → (nr : ℕ)
  → (lr : ℕ)
  → nr < numRows ≡ tt
  → lr < lenRow  ≡ tt
  → ℕ
matrix-elt 𝕞 nr lr nr<numRows lr<lenRow = nth𝕍 lr lr<lenRow (nth𝕍 nr nr<numRows (matrix-to-vecvec 𝕞))

-- 2c
diagonal-matrix : ℕ → (n : ℕ) → n by n matrix
diagonal-matrix d n = mkRows n n n
 where
  mkRow : ℕ → (n : ℕ) → 𝕍 ℕ n
  mkRow _     zero   = []
  mkRow i sn@(suc n) = (if i =ℕ sn then d else zero) :: mkRow i n

  mkRows : ℕ → (n : ℕ) → (x : ℕ) → 𝕍 (𝕍 ℕ n) x
  mkRows _ _  zero   = []
  mkRows i n (suc x) = mkRow i n :: mkRows (i ∸ 1) n x

identity-matrix : (n : ℕ) → n by n matrix
identity-matrix = diagonal-matrix 1

-- 2d
-- 1 2 3  T  1 0
-- 0 6 7     2 6
--           3 7
transpose : ∀ {m n : ℕ} → n by m matrix →  m by n matrix
transpose = {!!}
