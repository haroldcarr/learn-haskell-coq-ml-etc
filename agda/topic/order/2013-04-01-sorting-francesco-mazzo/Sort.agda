open import AgdaSort

module Sort
         {X    : Set}
         {_≈_  : Rel X}
         {_≤_  : Rel X}
         (_≤?_ : Decidable _≤_)
         (ord  : TotalOrder _≈_ _≤_)
  where
open TotalOrder  ord         using (total; equivalence)
open Equivalence equivalence using (refl)

--Insertion sort

-- represent bounded lists, but want bounds to be open
-- so lift X in a type that contains a top and bottom elements
data ⊥X⊤ : Set where
  ⊤ ⊥ : ⊥X⊤
  ⟦_⟧ : X → ⊥X⊤

-- lift ordering to work with ⊥X⊤
data _≤̂_ : Rel ⊥X⊤ where
  ⊥≤̂     : ∀ {x} → ⊥ ≤̂ x
  ≤̂⊤     : ∀ {x} → x ≤̂ ⊤
  ≤-lift : ∀ {x y} → x ≤ y → ⟦ x ⟧ ≤̂ ⟦ y ⟧

-- bounded, ordered lists
-- elements ordered according to ≤ relation
data OList (l u : ⊥X⊤) : Set where
  -- nil works with any bounds l ≤ u
  nil  : l ≤̂ u
       → OList l u
  -- cons x to a list with x as a lower bound,
  -- return a list with lower bound l, l ≤̂ ⟦ x ⟧
  cons : ∀ x
         (xs : OList ⟦ x ⟧ u)
       → l ≤̂ ⟦ x ⟧
       → OList l u

toList : ∀ {l u} → OList l u → List X
toList (nil _)       = []
toList (cons x xs _) = x ∷ toList xs

insert : ∀ {l u} x
       → OList l u
       → l ≤̂ ⟦ x ⟧
       → ⟦ x ⟧ ≤̂ u
       → OList l u
insert x   (nil _)             l≤̂⟦x⟧ ⟦x⟧≤̂u =
  cons x  (nil ⟦x⟧≤̂u)
          l≤̂⟦x⟧

insert x L@(cons x' xs l≤̂⟦x'⟧) l≤̂⟦x⟧ ⟦x⟧≤̂u with x ≤? x'
... | left   x≤x'        =
  cons x  (cons   x' xs (≤-lift x≤x'))
          l≤̂⟦x⟧
... | (right x≤x'→Empty) =
  cons x' (insert x  xs ([ ≤-lift
                         , (λ y≤x → absurd (x≤x'→Empty y≤x)) ]
                         (total x' x))
                        ⟦x⟧≤̂u)
          l≤̂⟦x'⟧

-- Insertion sort uses OList ⊥ ⊤ to represent a sorted list with open bounds:
isort' : List X → OList ⊥ ⊤
isort' = foldr (λ x xs → insert x xs ⊥≤̂ ≤̂⊤) (nil ⊥≤̂)

isort : List X → List X
isort xs = toList (isort' xs)

------------------------------------------------------------------------------
-- Tree sort (more efficient)

-- bounded, ordered binary tree
data Tree (l u : ⊥X⊤) : Set where
  leaf : l ≤̂ u
       → Tree l       u
  node : (x : X)
       → Tree l ⟦ x ⟧
       → Tree   ⟦ x ⟧ u
       → Tree l       u


t-insert : ∀ {l u}
         → (x : X)
         → Tree l       u
         →      l   ≤̂ ⟦ x ⟧
         →    ⟦ x ⟧ ≤̂   u
         → Tree l       u
t-insert x (leaf _)       l≤x x≤u =
  node x    (leaf l≤x) (leaf x≤u)
t-insert x (node y ly yu) l≤x x≤u with x ≤? y
... | left x≤y  =
  node y    (t-insert x ly l≤x (≤-lift x≤y)) yu
... | right x>y =
  node y ly (t-insert x yu ([ (λ x≤y → absurd (x>y x≤y)) , ≤-lift ] (total x y)) x≤u)

t-fromList : List X → Tree ⊥ ⊤
t-fromList = foldr (λ x xs → t-insert x xs ⊥≤̂ ≤̂⊤) (leaf ⊥≤̂)

-- OList concatenation, including inserting a new element in the middle
_⇒_++_ : ∀ {l u} x
       → OList l ⟦ x ⟧
       → OList   ⟦ x ⟧ u
       → OList l      u
x ⇒ nil l≤u       ++ xu = cons x            xu  l≤u
x ⇒ cons y yx l≤y ++ xu = cons y (x ⇒ yx ++ xu) l≤y

t-flatten : ∀ {l u} → Tree l u → OList l u
t-flatten (leaf l≤u)     = (nil l≤u)
t-flatten (node x lx xu) = x ⇒ t-flatten lx ++ t-flatten xu

t-sort' : List X → OList ⊥ ⊤
t-sort' xs = t-flatten (foldr (λ x xs → t-insert x xs ⊥≤̂ ≤̂⊤) (leaf ⊥≤̂) xs)

t-sort : List X → List X
t-sort xs = toList (t-sort' xs)

