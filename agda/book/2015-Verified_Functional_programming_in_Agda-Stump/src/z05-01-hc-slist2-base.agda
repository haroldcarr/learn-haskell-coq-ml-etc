{-
https://mazzo.li/posts/AgdaSort.html
2013-04-01Agda by Example: Sorting
-}

module z05-01-hc-slist2-base where

data List (X : Set) : Set where
  []  :              List X
  _∷_ : X → List X → List X
infixr 5 _∷_

foldr : ∀ {A} {B : Set} → (A → B → B) → B → List A → B
foldr f b      []  = b
foldr f b (a ∷ as) = f a (foldr f b as)

data Either (A : Set) (B : Set) : Set where
  left  : A → Either A B
  right : B → Either A B

[_,_] : ∀ {A B} {C : Set} → (A → C) → (B → C) → Either A B → C
[ f , g ] (left  x) = f x
[ f , g ] (right x) = g x

-- Unhabited type
data Empty : Set where

absurd : {X : Set} → Empty → X
absurd ()

-- use Empty to define something close to negation in logic:
-- e.g., terms of type ¬ (3 > 4)
infix 3 ¬_
¬_ : Set → Set
¬ X = X → Empty

-- binary relation on a type X
Rel : Set → Set₁
Rel X = X → X → Set

-- decidable relations
Decidable : ∀ {X} → Rel X → Set
Decidable R = ∀ x y → Either (R x y) (¬ (R x y))

-- To sort a list, need two relations on elements of list:

-- equality
record Equivalence {X} (_≈_ : Rel X) : Set₁ where
  field
    refl  : ∀ {x}     → x ≈ x
    sym   : ∀ {x y}   → x ≈ y → y ≈ x
    trans : ∀ {x y z} → x ≈ y → y ≈ z → x ≈ z

-- and ordering
record TotalOrder {X} (_≈_ : Rel X) (_≤_ : Rel X) : Set₁ where
  field
    antisym     : ∀ {x y}   → x ≤ y → y ≤ x → x ≈ y
    trans       : ∀ {x y z} → x ≤ y → y ≤ z → x ≤ z
    total       : ∀  x y    → Either (x ≤ y) (y ≤ x)
    reflexive   : ∀ {x y}   → x ≈ y → x ≤ y
    equivalence : Equivalence _≈_
