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
rows by cols matrix = 𝕍 (𝕍 ℕ cols) rows

matrix-to-vec-vec : ∀ {rows cols : ℕ} → rows by cols matrix → 𝕍 (𝕍 ℕ cols) rows
matrix-to-vec-vec 𝕞 = 𝕞

-- 2a
zero-matrix : (rows cols : ℕ) → rows by cols matrix
zero-matrix rows cols = repeat𝕍 (repeat𝕍 0 cols) rows

_ : zero-matrix 2 3 ≡ (0 :: 0 :: 0 :: []) ::
                      (0 :: 0 :: 0 :: []) :: []
_ = refl

_ : zero-matrix 0 0 ≡ []
_ = refl
_ : zero-matrix 0 1 ≡ []
_ = refl
_ : zero-matrix 1 0 ≡ [] :: []
_ = refl
_ : zero-matrix 1 1 ≡ (0 :: []) :: []
_ = refl

-- 2b
matrix-elt : ∀ {rows cols : ℕ}
  → rows by cols matrix
  → (r : ℕ)
  → (c : ℕ)
  → r < rows ≡ tt
  → c < cols ≡ tt
  → ℕ
matrix-elt 𝕞 r c r<rows c<cols = nth𝕍 c c<cols (nth𝕍 r r<rows (matrix-to-vec-vec 𝕞))

-- 2c
diagonal-matrix : ℕ → (n : ℕ) → n by n matrix
diagonal-matrix d n = mkRows n n n
 where
  -- when constructing rows/cols
  -- - row/col param corresponds to row/col - rows/cols
  -- - e.g., for 2 x 3 matrix
  --   row param 2 corresponds 2 - 2 = 0

  mkElt  : ℕ → ℕ → ℕ
  mkElt i col = if i =ℕ col then d else zero

  mkCols : (ℕ → ℕ) → (cols : ℕ) → 𝕍 ℕ cols
  mkCols _     zero   = []
  mkCols f sc@(suc c) = f sc :: mkCols f c

  mkRows : ℕ → (rows : ℕ) → (cols : ℕ) → 𝕍 (𝕍 ℕ cols) rows
  mkRows _  zero   _ = []
  mkRows i (suc r) c = mkCols (mkElt i) c :: mkRows (i ∸ 1) r c

identity-matrix : (n : ℕ) → n by n matrix
identity-matrix = diagonal-matrix 1

idm5 : 5 by 5 matrix
idm5 = identity-matrix 5

_ : idm5 ≡ (1 :: 0 :: 0 :: 0 :: 0 :: []) ::
           (0 :: 1 :: 0 :: 0 :: 0 :: []) ::
           (0 :: 0 :: 1 :: 0 :: 0 :: []) ::
           (0 :: 0 :: 0 :: 1 :: 0 :: []) ::
           (0 :: 0 :: 0 :: 0 :: 1 :: []) :: []
_ = refl

_ : matrix-elt idm5 0 0 refl refl ≡ 1
_ = refl
_ : matrix-elt idm5 1 1 refl refl ≡ 1
_ = refl
_ : matrix-elt idm5 0 1 refl refl ≡ 0
_ = refl

-- 2d
-- BEGIN https://typeslogicscats.gitlab.io/posts/agda-matrix.lagda.html
prepend-column
  : ∀ {m n : ℕ}
  → 𝕍 ℕ n                    -- a column
  → n by     m matrix
  → n by suc m matrix        -- prepends the given column to the matrix
prepend-column      []           []  = []
prepend-column (x :: xs) (vec :: vecs) = (x :: vec) :: (prepend-column xs vecs)

-- inverse of prepend-column (NOT USED)
unprepend-column
  : ∀ {m n : ℕ}
  →            n by suc m matrix
  → (𝕍 ℕ n) × (n by     m matrix)
unprepend-column                     [] = ([] , [])
unprepend-column ((x :: vec) :: matrix) = let xs-vecs = unprepend-column matrix
                                in x :: fst xs-vecs , vec :: snd xs-vecs

fill-empty : (n : ℕ) → n by 0 matrix
fill-empty       0 = []
fill-empty (suc n) = [] :: fill-empty n

transpose : ∀ {i : ℕ} {j : ℕ} → i by j matrix → j by i matrix
transpose     {0} {j}           []  = fill-empty j
transpose {suc _} {_} (row :: rows) = prepend-column row (transpose rows)
-- END https://typeslogicscats.gitlab.io/posts/agda-matrix.lagda.html

ex2x3 : 2 by 3 matrix
ex2x3 = (1 :: 2 :: 3 :: []) ::
        (0 :: 6 :: 7 :: []) :: []

_ : transpose ex2x3 ≡ (1 :: 0 :: []) ::
                      (2 :: 6 :: []) ::
                      (3 :: 7 :: []) :: []
_ = refl

-- BEGIN https://www.cs.nott.ac.uk/~psztxa/g53cfr/solutions/ex02.agda
vreturn : {A : Set} {n : ℕ} → A → 𝕍 A n
vreturn {n = zero}  a = []
vreturn {n = suc m} a = a :: vreturn {n = m} a

vapp : {A B : Set} {n : ℕ} → 𝕍 (A → B) n → 𝕍 A n → 𝕍 B n
vapp      []       []  = []
vapp (f :: fs) (a :: as) = f a :: vapp fs as

transposeX : {m n : ℕ} → m by n matrix → n by m matrix
transposeX         []  = vreturn []
transposeX (as :: ass) = vapp (vapp (vreturn _::_ ) as) (transposeX ass)

_ : transposeX ex2x3 ≡ (1 :: 0 :: []) ::
                       (2 :: 6 :: []) ::
                       (3 :: 7 :: []) :: []
_ = refl
-- END https://www.cs.nott.ac.uk/~psztxa/g53cfr/solutions/ex02.agda

-- BEGIN HORRIBLE HACKY TRY
postulate
  yyy : (n : ℕ) → (rc : ℕ) → rc < n ≡ tt

xx : ∀ {x y : ℕ}
   → x =ℕ 0 ≡ ff
   → y =ℕ 0 ≡ ff
   → x ∸ y < x ≡ tt
xx {x} {suc y} x≠0 y≠0
  rewrite ∸< {x} {y} x≠0
  = refl

transpose' : ∀ {n m : ℕ} → n by m matrix → m by n matrix
transpose'             {0}       {m} _ = zero-matrix m 0
transpose'             {1}       {m} _ = zero-matrix m 1
transpose' n@{suc (suc _)}    {zero} 𝕞 = zero-matrix zero n
transpose' n@{suc (suc _)} m@{suc _} 𝕞 = mkRows m n
 where
  mkElt : (newRow : ℕ)
        → newRow =ℕ 0 ≡ ff
        → (newCol : ℕ)
        → newCol =ℕ 0 ≡ ff
        → ℕ
  mkElt newRow rp newCol cp =
    matrix-elt 𝕞 (n ∸ newCol)         (m ∸ newRow)
--                 (xx cp refl) (xx rp refl)
--              (yyy (n ∸ newCol) n) (yyy (m ∸ newRow) m)
                    {!!} {!!}

  mkCols : (∀ (new : ℕ) → new =ℕ 0 ≡ ff → ℕ) → (cols : ℕ) → 𝕍 ℕ cols
  mkCols _     zero   = []
  mkCols f sc@(suc c) = f sc refl :: mkCols f c

  mkRows : (rows : ℕ) → (cols : ℕ) → 𝕍 (𝕍 ℕ cols) rows
  mkRows     zero   _ = []
  mkRows sr@(suc r) c = mkCols (mkElt sr refl) c :: mkRows r c

_ : transpose' ex2x3 ≡ (1 :: 0 :: []) ::
                       (2 :: 6 :: []) ::
                       (3 :: 7 :: []) :: []
_ = refl
-- END HORRIBLE HACKY TRY

-- 2e
dotProduct𝕍 : ∀ {n : ℕ} → 𝕍 ℕ n → 𝕍 ℕ n → ℕ
dotProduct𝕍       []        []  = 0
dotProduct𝕍 (a :: as) (b :: bs) = a * b + (dotProduct𝕍 as bs)

_ : dotProduct𝕍 (1 :: 3 :: 5 :: []) (4 :: 2 :: 1 :: []) ≡ 15
_ = refl

foldr : ∀ {A B : Set} {n : ℕ} → (A → B → B) → B → 𝕍 A n → B
foldr f z       []  = z
foldr f z (x :: xs) = f x (foldr f z xs)

zipWith : ∀ {A B C : Set} {n : ℕ} → (A → B → C) → 𝕍 A n → 𝕍 B n → 𝕍 C n
zipWith _       []        []  = []
zipWith f (x :: xs) (y :: ys) = f x y :: zipWith f xs ys

dotProduct𝕍' : ∀ {n : ℕ} → 𝕍 ℕ n → 𝕍 ℕ n → ℕ
dotProduct𝕍' as bs = foldr _+_ 0 (zipWith _*_ as bs)

_ : dotProduct𝕍' (1 :: 3 :: 5 :: []) (4 :: 2 :: 1 :: []) ≡ 15
_ = refl

-- 2f
matrix-* : ∀ {m n p : ℕ} → m by n matrix → n by p matrix → m by p matrix
matrix-* [] _ = []
matrix-* {m} {n} {p} (a :: as) bs =
  doRow {n} {p} a (transpose bs) :: matrix-* as bs
 where
  doRow : ∀ {n p : ℕ} → 𝕍 ℕ n → p by n matrix → 𝕍 ℕ p
  doRow a [] = []
  doRow {n} {p} a (b :: bs) = dotProduct𝕍 a b :: doRow a bs

ma : 2 by 3 matrix
ma = (2 :: 3 :: 4 :: []) ::
     (1 :: 0 :: 0 :: []) :: []
mb : 3 by 2 matrix
mb = (0 :: 1000 :: []) ::
     (1 ::  100 :: []) ::
     (0 ::   10 :: []) :: []

_ : matrix-* ma mb ≡ (3 :: 2340 :: []) ::
                     (0 :: 1000 :: []) :: []
_ = refl

identity-2 : 2 by 2 matrix
identity-2 = identity-matrix 2

some-mat : 2 by 2 matrix
some-mat =
  (1 :: 2 :: []) ::
  (3 :: 4 :: []) :: []

some-mat-trans : 2 by 2 matrix
some-mat-trans =
  (1 :: 3 :: []) ::
  (2 :: 4 :: []) :: []

_ : matrix-* some-mat identity-2 ≡ some-mat
_ = refl

_ : transpose some-mat ≡ some-mat-trans
_ = refl

left-mat : 2 by 3 matrix
left-mat =
  (1 :: 2 :: 3 :: []) ::
  (4 :: 5 :: 6 :: []) :: []

right-mat : 3 by 2 matrix
right-mat =
  ( 7 ::  8 :: []) ::
  ( 9 :: 10 :: []) ::
  (11 :: 12 :: []) :: []

product : 2 by 2 matrix
product =
  ( 58 ::  64 :: []) ::
  (139 :: 154 :: []) :: []

_ : matrix-* left-mat right-mat ≡ product
_ = refl

-- TODO https://www.cs.nott.ac.uk/~psztxa/g53cfr/solutions/ex02.agda

-- 3
-- from list.agda
data 𝕃 {ℓ} (A : Set ℓ) : Set ℓ where
  [] : 𝕃 A
  _::_ : (x : A) (xs : 𝕃 A) → 𝕃 A

-- from vector.agda
𝕍-to-𝕃 : ∀ {ℓ} {A : Set ℓ} {n : ℕ} → 𝕍 A n → 𝕃 A
𝕍-to-𝕃 [] = []
𝕍-to-𝕃 (x :: xs) = x :: (𝕍-to-𝕃 xs)

𝕃-to-𝕍 : ∀ {ℓ} {A : Set ℓ} → 𝕃 A → Σ ℕ (λ n → 𝕍 A n)
𝕃-to-𝕍 [] = (0 , [])
𝕃-to-𝕍 (x :: xs) with 𝕃-to-𝕍 xs
... | (n , v) = (suc n , x :: v)

e3 : ∀ {ℓ} {A : Set ℓ} {n : ℕ}
   → (v : 𝕍 A n)
   → 𝕃-to-𝕍 (𝕍-to-𝕃 v) ≡ n , v
e3       [] = refl
e3 (x :: v) with e3 v
... | zz rewrite zz = refl

-- 4. fun takes V (A × B) n ; returns pair V A n and V B n
--    similar to Haskell unzip
unzip : ∀ {ℓ} {A B : Set ℓ} {n : ℕ}
      → 𝕍 (A × B) n
      → 𝕍 A n × 𝕍 B n
unzip       [] = [] , []
unzip ((a , b) :: v) =
  let rest = unzip v
   in a :: fst rest , b :: snd rest

_ : unzip ((1 , 10) :: (2 , 20) :: (3 , 30) :: []) ≡   ( 1 ::  2 ::  3 :: [])
                                                     , (10 :: 20 :: 30 :: [])
_ = refl

{- TODO
-- 5. Implement remove-min / remove-max functions for bst. type.
Using remove-min, define a general remove function
- finds first value isomorphic to given one
- returns bst without that value.
If node holding that value has two (non-leaf) nodes as left and right sub-trees,
then necessary to replace the removed element with its successor.
This is the minimum value in the right subtree.

-- 6. In list-merge-sort.agda : merge-sort using Braun trees.
State and prove theorems about merge-sort.
E.g., prove length of input list and length of returned sorted list are the same.
-}
