{-
https://mazzo.li/posts/AgdaSort.html
2013-04-01Agda by Example: Sorting
-}

open import z05-01-hc-slist2-base

module z05-01-hc-slist2
         {X}
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
  {-
Tree sort
Now for something more efficient, a tree sort. Firstly we’ll define a bounded, ordered binary tree:


  data Tree (l u : ⊥X⊤) : Set where
    leaf : l ≤̂ u → Tree l u
    node : (x : X) → Tree l ⟦ x ⟧ → Tree ⟦ x ⟧ u → Tree l u
The technique is similar to that employed in OList. Then we need a procedure to insert an element in an existing tree:


  newLeaf : ∀ {l u} → (x : X) → Tree l u → l ≤̂ ⟦ x ⟧ → ⟦ x ⟧ ≤̂ u → Tree l u
  newLeaf x (leaf _)       l≤x x≤u = node x (leaf l≤x) (leaf x≤u)
  newLeaf x (node y ly yu) l≤x x≤u with x ≤? y
  newLeaf x (node y ly yu) l≤x x≤u | left x≤y  =
    node y (newLeaf x ly l≤x (≤-lift x≤y)) yu
  newLeaf x (node y ly yu) l≤x x≤u | right x>y =
    node y ly (newLeaf x yu ([ (λ x≤y → absurd (x>y x≤y)) , ≤-lift ] (total x y)) x≤u)
Again, the only tricky bit is the last one, where we need to convince Agda that y ≤ x given that ¬ (x ≤ y).

Similar to isort′, turning a List into a Tree is a simple fold:


  fromList : List X → Tree ⊥ ⊤
  fromList = foldr (λ x xs → newLeaf x xs ⊥≤̂ ≤̂⊤) (leaf ⊥≤̂)
Now we can define OList concatenation, with the twist of inserting a new element in the middle; and finally flatten:


  _⇒_++_ : ∀ {l u} x → OList l ⟦ x ⟧ → OList ⟦ x ⟧ u → OList l u
  x ⇒ nil l≤u       ++ xu = cons x xu l≤u
  x ⇒ cons y yx l≤y ++ xu = cons y (x ⇒ yx ++ xu) l≤y

  flatten : ∀ {l u} → Tree l u → OList l u
  flatten (leaf l≤u)     = (nil l≤u)
  flatten (node x lx xu) = x ⇒ flatten lx ++ flatten xu
Then we are good with yet another fold.


  treeSort′ : List X → OList ⊥ ⊤
  treeSort′ xs = flatten (foldr (λ x xs → newLeaf x xs ⊥≤̂ ≤̂⊤) (leaf ⊥≤̂) xs)

  treeSort : List X → List X
  treeSort xs = toList (treeSort′ xs)
Propositional equality
Now lets put our module to work. We will need a type equipped with the appropriate relations: in this post I am going to use natural numbers.

For what concerns equality, we can actually define an inductive family that relates equal terms:


module PropositionalEquality where
  data _≡_ {X} : Rel X where
    refl : ∀ {x} → x ≡ x
It’s worth mentioning what equal means here. I have mentioned earlier that “evaluation and typechecking are intertwined”: when the type checker has to decide if two types, or more generally two terms, are “the same”, it simply reduces them as far as possible (to their normal form) and then compares them syntactically, plus some additional laws.6 Remember, every Agda term is terminating, so this procedure itself is guaranteed to terminate. Thus, refl : ((λ x → x) 1) ≡ 1 is acceptable, and so on.

This notion of equality is often called definitional equality, as opposed to the user-level equality expressed by the inductive family we have just defined, which takes the name of propositional equality. Note that having a prop. equality in scope does not imply definitional equality for the related terms, unless the prop. equality is a closed term.7 In the general case we might have prop. equalities in scope that do not necessarily hold or involve abstracted variables, think of λ (p : 3 ≡ 1) → ....

Let’s prove that ≡ is an equivalence relation, and a congruence law which will be useful later:


  sym : ∀ {X} {x y : X} → x ≡ y → y ≡ x
  sym refl = refl

  trans : ∀ {X} {x y z : X} → x ≡ y → y ≡ z → x ≡ z
  trans refl refl = refl

  equivalence : ∀ {X} → Equivalence {X} _≡_
  equivalence = record { refl = refl; sym = sym; trans = trans }

  cong : ∀ {X} {x y : X} → (f : X → X) → x ≡ y → f x ≡ f y
  cong _ refl = refl
Here we use pattern matching in a new way: since the value of the indices of ≡ depends on the constructors, matching on a constructor refines the context with the new information. For example in sym pattern matching refl will unify y and x, turning them into the same variable in the context for the body of sym, and thus letting us invoke refl again. Pattern matching is a much more powerful notion in Agda that is in in most (even dependently typed) programming languages—it can not only change the context, but it will also constraint the possible constructors of other parameters, if they are of a type with indices and those indices have been refined. This collection of techniques is known as dependent pattern matching.

Natural numbers

module Nat where
  data ℕ : Set where
    zero : ℕ
    suc  : ℕ → ℕ

  {-# BUILTIN NATURAL ℕ    #-}
The definition for naturals is the usual one—the pragmas are there so that we can use number literals.

Now for our ordering relation. Every number is greater or equal than zero, and if x ≤ y then x + 1 ≤ y + 1:


  data _≤_ : Rel ℕ where
    z≤n : ∀ {x}   → zero ≤ x
    s≤s : ∀ {x y} → x ≤ y → suc x ≤ suc y
With the help of the dual of s≤s, we can write our decision function for ≤:


  ≤-suc : ∀ {x y} → suc x ≤ suc y → x ≤ y
  ≤-suc (s≤s x≤y) = x≤y

  _≤?_ : Decidable _≤_
  zero  ≤? _     = left z≤n
  suc _ ≤? zero  = right λ()
  suc x ≤? suc y with x ≤? y
  ... | left x≤y  = left  (s≤s x≤y)
  ... | right x>y = right (λ sx≤sy → x>y (≤-suc sx≤sy))
And the required laws to make a total order out of ≤:


  open PropositionalEquality using (_≡_; refl; cong; equivalence)

  antisym : ∀ {x y} → x ≤ y → y ≤ x → x ≡ y
  antisym z≤n       z≤n       = refl
  antisym (s≤s x≤y) (s≤s y≤x) = cong suc (antisym x≤y y≤x)

  trans : ∀ {x y z} → x ≤ y → y ≤ z → x ≤ z
  trans z≤n       _         = z≤n
  trans (s≤s x≤y) (s≤s y≤z) = s≤s (trans x≤y y≤z)

  total : ∀ x y → Either (x ≤ y) (y ≤ x)
  total zero    _       = left  z≤n
  total (suc x) zero    = right z≤n
  total (suc x) (suc y) with total x y
  ... | left  x≤y = left  (s≤s x≤y)
  ... | right y≤x = right (s≤s y≤x)

  reflexive : ∀ {x y} → x ≡ y → x ≤ y
  reflexive {zero}  refl = z≤n
  reflexive {suc _} refl = s≤s (reflexive refl)

  totalOrder : TotalOrder _≡_ _≤_
  totalOrder = record
    { antisym     = antisym
    ; trans       = trans
    ; total       = total
    ; reflexive   = reflexive
    ; equivalence = equivalence
    }
Finally, we can import the sorting functions. We’re done!


  open Sort _≤?_ totalOrder using (isort; treeSort)
We can test our function:


  willIBeSorted? : List ℕ
  willIBeSorted? = treeSort (12 ∷ 3 ∷ 7 ∷ 4 ∷ 40 ∷ 5 ∷ 0 ∷ [])
A tap on C-c C-n willIBeSorted? will give the expected result:

0 ∷ 3 ∷ 4 ∷ 5 ∷ 7 ∷ 12 ∷ 40 ∷ []
Comments?
This is my first decently-sized blog post, so please complain on Reddit!

For the interested, the logical core of most of said systems is an intensional Intuitionistic Type Theory.↩︎

Specifically:

Functions must be structurally recursive, where the arguments in the recursive calls are decreasing.
Disallows data type declarations that are not strictly positive, for example the infamous data Mu f = Mu (f (Mu f)) in Haskell.
Has a hierarchy of types—more on this later.
↩︎
In type theory this is known as Girards’ paradox, you can find an Agda (with Set : Set enabled) rendition here.↩︎

Some might ask why Agda doesn’t treat all parameters uniformly, simply allowing indices at will. This is definitely an option (taken by other programming languages, and GHC’s GADTs) but separating them brings more clarity in the interface and lets Agda deal with inductive families more straightforwardly.↩︎

Some might rightfully complain that actually we are only proving half of the story, since we need to guarantee that the result list is a permutation of the input list to prove a sorting algorithm correct. That is doable but a bit more involved.↩︎

For example partial applications are expanded, so that if f : A -> B, then f ≡ λ x → f x. Similary, if we have a record Tuple (A B : Set) : Set with fields fst : A and snd : B, and constructor _,_; if x : Tuple A B then x ≡ fst x , snd x. Apart from these η laws, other additions can be made to have more terms deemed as equal by the type checker, details vary from system to system.↩︎

This is the reason why we did not just use ≡ from the beginning and we instead chose to parametrise our equality relation: sometimes propositional equality does not cut it, for example when working with functions.↩︎

f@mazzo.li · twitter
-}
