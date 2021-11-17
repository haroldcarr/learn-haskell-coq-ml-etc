{-# OPTIONS --type-in-type #-}  -- cheating

module Lec4-HC-2 where

open import Lec1-HC
open import Lec2-HC
open import Lec3-HC
open import Lec4-HC

_[QED] : {X : Set}(x : X) -> x == x
x [QED] = refl x
_=[_>=_ : {X : Set}(x : X){y z : X} -> x == y -> y == z -> x == z
x =[ refl .x >= q = q
_=<_]=_ : {X : Set}(x : X){y z : X} -> y == x -> y == z -> x == z
x =< refl .x ]= q = q
infixr 1 _=[_>=_ _=<_]=_
infixr 2 _[QED]


open FUNCTOR public

module FUNCTOR-CP {C D E : Category} where
  open _=>_
  open Category

  _>=>_ : C => D  ->  D => E  ->  C => E

  F-Obj (F >=> G) = F-Obj F >> F-Obj G

  F-map (F >=> G) = F-map F >> F-map G

  F-map-id~> (F >=> G) = {!!}
{-
    F-map G (F-map F (id~> C))
      =[ refl (F-map G) =$= F-map-id~> F >=
    F-map G (id~> D)
      =[ F-map-id~> G >=
    id~> E
      [QED]
-}
  F-map->~> (F >=> G) f g = {!!}
{-
    F-map G (F-map F (_>~>_ C f g))
      =[ refl (F-map G) =$= F-map->~> F f g >=
    F-map G (_>~>_ D (F-map F f) (F-map F g))
      =[ F-map->~> G (F-map F f) (F-map F g) >=
    _>~>_ E (F-map G (F-map F f)) (F-map G (F-map F g))
      [QED]
-}

open FUNCTOR-CP public

module NATURAL-TRANSFORMATION {C D : Category} where
  open Category
  open _=>_

  record _~~>_ (F G : C => D) : Set where
    field
      -- one operation
      xf : {X : Obj C} -> _~>_ D (F-Obj F X) (F-Obj G X)
      -- one law
      naturality : {X Y : Obj C}(f : _~>_ C X Y) ->
                   _>~>_ D (F-map F f) (xf {Y})
                   ==
                   _>~>_ D (xf {X}) (F-map G f)

open NATURAL-TRANSFORMATION public
open _~~>_ public

YES : ID ~~> MAYBE
xf         YES = yes
naturality YES = λ _ → refl _

JOIN : (MAYBE >=> MAYBE) ~~> MAYBE
xf         JOIN = MAYBE-CAT.joinMaybe
naturality JOIN = λ x→y -> extensionality λ { (yes mx) → refl (maybe x→y mx)
                                            ; no       → refl no }

{-
-- 41:45
MONAD

you have a functor M
want to show it is a MONAD
equiped with:
- unit             : nat trans from ID to M : you can always make one M layer
                   : if you have a thing then you have a plan to make a thing
- mult(iplication) : given composition of two M layers, nat trans that joins them together
                   : if you have a plan to make a thing AND a plan to make a thing
                     then you have a plan to make a thing
need to show:
- laws
-}



