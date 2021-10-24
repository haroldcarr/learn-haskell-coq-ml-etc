{-# OPTIONS --type-in-type #-} -- enables cheating
                               -- 2:10 : why


-- https://www.youtube.com/watch?v=2LxtHeZlaVw

module HC-Lec3 where

open import HC-Lec1
open import HC-Lec2

postulate
  extensionality : {S : Set} {T : S -> Set}
                -> (f g : (x : S) -> T x)
                -> ((x : S) -> f x == g x)
                ->             f   == g

-- 2:45

record Category : Set where
  field
    -- things
    Obj  : Set
    _~>_ : Obj -> Obj -> Set

    -- operations
    id~>  : {T     : Obj} -> T ~> T
    _>~>_ : {R S T : Obj} -> R ~> S -> S ~> T -> R ~> T

    -- laws
    law-id~>>~> : {S T : Obj} -> (f : S ~> T) -> (id~> >~> f)    == f
    law->~>id~> : {S T : Obj} -> (f : S ~> T) -> (f    >~> id~>) == f
    law->~>>~>  : {Q R S T : Obj}
               -> (f : Q ~> R) -> (g : R ~> S) -> (h : S ~> T)
               -> ((f >~> g) >~> h) == (f >~> (g >~> h))

-- 7:50

SET : Category
SET = record
        { Obj         = Set
        ; _~>_        = λ S T -> S -> T
        ; id~>        = id
        ; _>~>_       = _>>_
        ; law-id~>>~> = refl
        ; law->~>id~> = refl
        ; law->~>>~>  = λ f g h → refl (λ x → h (g (f x)))
        }

-- 11:50
-- A PREORDER is a category where there is at most one arrow between any two objects.
-- Therefore arrows are unique.

NAT->= : Category
NAT->= = record
           { Obj         = Nat
           ; _~>_        = _>=_
           ; id~>        = λ {n : Nat} → refl->= n
           ; _>~>_       = λ {r s t : Nat} r>=s s>=t → trans->= r s t r>=s s>=t
           ; law-id~>>~> = λ {m n : Nat} f → unique m n (trans->= m m n (refl->= m) f) f
           ; law->~>id~> = λ {m n : Nat} f → unique m n (trans->= m n n f (refl->= n)) f
           ; law->~>>~>  = λ {q r s t : Nat} q>=r r>=s s>=t →
                            unique q t
                              (trans->= q s t (trans->= q r s q>=r r>=s) s>=t)
                              (trans->= q r t  q>=r (trans->= r s t r>=s s>=t))
           }
 where
  -- all proofs of '>=' are equivalent
  unique : (m n : Nat) (p q : m >= n) -> p == q
  unique      m   zero   p q = refl <>
  unique (suc m) (suc n) p q = unique m n p q

-- 22:07

-- A MONOID is a category with Obj = One.
-- The values in the monoid are the *arrows*.
-- Only one object, so arrows are the ways to get from Nat to Nat.
ONE-Nat : Category
ONE-Nat = record
            { Obj         = One
            ; _~>_        = λ _ _ -> Nat
            ; id~>        = zero
            ; _>~>_       = _+N_
            ; law-id~>>~> = refl
            ; law->~>id~> = +N-zero
            ; law->~>>~>  = assocLR-+N
            }

-- 26:05

-- Note : this is controversial : because this notion of equality is structural.
-- But equality could be defined by isomorphism instead.
-- - In this case, there could be multiple ways for something to be equal to itself.
-- - equality on Bool : True to True  and False to False
--                  or  True to False and False to True

-- structural equality is unique
eqUnique : {X : Set} {x y : X} -> (p q : x == y) -> p == q
eqUnique (refl x) (refl _) = refl (refl x)

eqUnique' : {X : Set} {x y : X} -> (p q : x == y) -> p == q
eqUnique' {_} {_} {y} p q rewrite q | p = refl (refl y)

DISCRETE : (X : Set) -> Category
DISCRETE X = record
               { Obj         = X
               ; _~>_        = _==_
               ; id~>        = λ {x} -> refl x
               ; _>~>_       = λ {(refl x) (refl .x) → refl x}
               ; law-id~>>~> = λ f     → eqUnique _ f
               ; law->~>id~> = λ f     → eqUnique _ f
               ; law->~>>~>  = λ f g h → eqUnique _ _
               }

-- 40:50

module FUNCTOR where
  open Category

  record _=>_ (C D : Category) : Set where -- functor from C to D
    field
      -- operations
      F-Obj : Obj C -> Obj D
      F-map : {S T : Obj C} -> _~>_ C S T -> _~>_ D (F-Obj S) (F-Obj T)

      -- laws
      F-map-id~> : {T : Obj C} -> F-map (id~> C {T}) == id~> D {F-Obj T}
      F-map->~>  : {R S T : Obj C} (f : _~>_ C R S) (g : _~>_ C S T)
                -> F-map (_>~>_ C f g) == _>~>_ D (F-map f) (F-map g)
open FUNCTOR

-- https://www.youtube.com/watch?v=RCRddhYegzI
