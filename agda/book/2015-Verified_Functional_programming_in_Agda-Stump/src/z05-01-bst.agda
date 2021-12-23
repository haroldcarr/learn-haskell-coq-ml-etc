-- binary search trees (not balanced)

open import bool
open import bool-thms2
open import eq
open import maybe
open import product
open import product-thms
open import bool-relations using (transitive ; total)

module z05-01-bst (A : Set)                    -- type of elements
                  (_≤A_ : A → A → 𝔹)           -- ordering function
                  (≤A-trans : transitive _≤A_) -- proof of transitivity of given ordering
                  (≤A-total : total      _≤A_) -- proof of totality     of given ordering
                  where
{-
IAL
- relations.agda
  - models binary relation on A as functions from A to A to Set
- bool-relations.agda
  - models binary realtion on A as functions from A to A to 𝔹

Cannot use A→A→Set relations here because code cannot manipulate types.
But code can pattern match on values of type 𝔹, so boolean relations more useful.
-}
open import bool-relations _≤A_ hiding (transitive ; total)
open import minmax _≤A_ ≤A-trans ≤A-total

-- able to store values in bounds 'l' and 'u'
--         l(ower) bound
--         |   u(upper) bound
--         v   v
data bst : A → A → Set where
  bst-leaf : ∀ {l u : A}
           → l ≤A u ≡ tt
           → bst l u
  bst-node : ∀ {l l' u' u : A}
           → (d : A)      -- value stored in this node
           → bst l' d     -- left  subtree of values ≤ stored value
           → bst d u'     -- right subtree of values ≥ stored value
           → l ≤A l' ≡ tt
           → u' ≤A u ≡ tt
           → bst l u

------------------------------------------------------------------------------
bst-search : ∀ {l u : A}
           → (d : A)                             -- find a node that is isomorphic (_=A_) to d
           → bst l u
           → maybe (Σ A (λ d' → d iso𝔹 d' ≡ tt)) -- return that node or nothing
bst-search d (bst-leaf _) = nothing -- could return proof not in tree instead
-- compare given element with stored element
bst-search d (bst-node d' L R _ _) with keep (d ≤A d')
-- compare given element with stored element (other direction)
bst-search d (bst-node d' L R _ _) | tt , p1 with keep (d' ≤A d)
-- both are true implying iso, so return that element and proof
bst-search d (bst-node d' L R _ _) | tt , p1 | tt , p2 = just (d' , iso𝔹-intro p1 p2)
-- if either is false then search the appropriate branch
bst-search d (bst-node d' L R _ _) | tt , p1 | ff , p2 = bst-search d L -- search in left
bst-search d (bst-node d' L R _ _) | ff , p1           = bst-search d R -- search in right

------------------------------------------------------------------------------
-- change the lower bound from l' to l
bst-dec-lb : ∀ {l l' u' : A}
           → bst l' u'
           → l ≤A l' ≡ tt
           → bst l u'
bst-dec-lb (bst-leaf p)           q = bst-leaf       (≤A-trans q p)
bst-dec-lb (bst-node d L R p1 p2) q = bst-node d L R (≤A-trans q p1) p2

-- change the upper bound from u' to u
bst-inc-ub : ∀ {l' u' u : A}
           → bst l' u'
           → u' ≤A u ≡ tt
           → bst l' u
bst-inc-ub (bst-leaf p)           q = bst-leaf          (≤A-trans p q)
bst-inc-ub (bst-node d L R p1 p2) q = bst-node d L R p1 (≤A-trans p2 q)

bst-insert : ∀{l u : A}
           → (d : A)                 -- insert 'd'
           → bst l u                 -- into this 'bst'
           → bst (min d l) (max d u) -- type might change
bst-insert d (bst-leaf p) =
  bst-node d (bst-leaf ≤A-refl) (bst-leaf ≤A-refl) min-≤1 max-≤1

bst-insert d (bst-node d' L R p1 p2) with keep (d ≤A d')

bst-insert d (bst-node d' L R p1 p2) | tt , p with bst-insert d L

bst-insert d (bst-node d' L R p1 p2) | tt , p | L' rewrite p =
  bst-node d' L'                (bst-inc-ub R (≤A-trans p2 max-≤2))
           (min2-mono p1) ≤A-refl

bst-insert d (bst-node d' L R p1 p2) | ff , p with bst-insert d R

bst-insert d (bst-node d' L R p1 p2) | ff , p | R' rewrite p =
  bst-node d' (bst-dec-lb L p1) R'
           min-≤2         (max2-mono p2)
