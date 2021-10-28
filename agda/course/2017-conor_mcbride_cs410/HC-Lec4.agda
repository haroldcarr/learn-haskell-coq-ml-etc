{-# OPTIONS --type-in-type #-}  -- cheating

module HC-Lec4 where


open import HC-Lec1
open import HC-Lec2
open import HC-Lec3

-- https://www.youtube.com/watch?v=vTmYvoDrBlc
-- 24:00

ID : SET => SET
ID = id~> where open Category CATEGORY

_>F>_ : (SET => SET) -> (SET => SET) -> (SET => SET)
F >F> G = F >~> G where open Category CATEGORY

data Maybe (X : Set) : Set where
  yes : (x : X) -> Maybe X
  no  :            Maybe X

maybe : {S T : Set} -> (S -> T) -> Maybe S -> Maybe T
maybe f (yes x) = yes (f x)
maybe f   no    = no

MAYBE : SET => SET
MAYBE = record
          { F-Obj      = Maybe
          ; F-map      = maybe
          ; F-map-id~> = extensionality
                         λ { (yes t) → refl (yes t)
                           ; no      → refl no }
          ; F-map->~>  = λ r->s s->t → extensionality
                         λ { (yes x) → refl (yes (s->t (r->s x)))
                           ; no      → refl no }
          }

-- 26:45
-- EXTENSIONALITY
-- The extension of something is its externally visible behavior.
-- INTENSIONALITY
-- Things are intensionality equal if they have the same implementation.

-- 31:32

-- plain old list
data List (X : Set) : Set where
  []   : List X
  _::_ : (x : X) -> (xs : List X) -> List X

-- called 'map' in Haskell
list : {X Y : Set} -> (X -> Y) -> List X -> List Y
list _       []  = []
list f (x :: xs) = f x :: list f xs

LIST : SET => SET
LIST = record
        { F-Obj      = List
        ; F-map      = list
        ; F-map-id~> = extensionality listId
        ; F-map->~>  = λ r->s s->t → extensionality (listCp r->s s->t)
        }
 where
  open Category SET
  listId : {T : Set} -> (xs : List T) -> list id~> xs == id~> xs
  listId       []  = refl []
  listId (x :: xs) rewrite listId xs = refl (x :: xs)

  listCp : {R S T : Set} (r->s : R ~> S) (s->t : S ~> T)
        -> (rs : List R)
        -> list (r->s >~> s->t) rs == (list r->s >~> list s->t) rs
  listCp    _    _       []  = refl []
  listCp r->s s->t (r :: rs)
    rewrite listCp r->s s->t rs
    = refl (s->t (r->s r) :: list s->t (list r->s rs))

-- 36:08

-- compose functions that may go wrong
module MAYBE-CAT where
  open Category SET -- working in the category of SET
  open _=>_ MAYBE   -- with the MAYBE functor

  joinMaybe : {T : Set} -> Maybe (Maybe T) -> Maybe T
  joinMaybe (yes mt) = mt
  joinMaybe no       = no

  MAYBE-CAT : Category
  MAYBE-CAT = record
    { Obj = Set
    ; _~>_ = λ S T -> S -> Maybe T -- a function from S to T that might go wrong (i.e., exception)
    ; id~> = yes
      -- work with arrows (instead of applied functions)
    ; _>~>_ = λ f g -> f >> (F-map g >> joinMaybe)
    ; law-id~>>~> = \f -> refl f
    ; law->~>id~> = λ s->mt → extensionality λ s -> idR s->mt s
    ; law->~>>~> = λ q->mr r->ms s->mt → extensionality λ q → cmp q->mr r->ms s->mt q
    }
   where
    idR : {S T : Set} (s->mt : S → Maybe T) (s : S)
        → joinMaybe (F-map yes (s->mt s)) == s->mt s
    idR s-mt s
      with s-mt s
    ... | yes t = refl (yes t)
    ... | no    = refl no

    cmp : ∀ {Q} {R} {S} {T}
          (q->mr : Q → Maybe R) (r->ms : R → Maybe S) (s->mt : S → Maybe T) (q : Q)
       -> joinMaybe (F-map s->mt (joinMaybe (F-map r->ms (q->mr q))))
       == joinMaybe (F-map (λ r → joinMaybe (F-map s->mt (r->ms r))) (q->mr q))
    cmp q->mr r->ms s->mt q
      with q->mr q
    ... | no = refl no
    ... | yes r
      with r->ms r
    ... | no = refl no
    ... | yes s
      with s->mt s
    ... | no = refl no
    ... | yes t
      = refl (yes t)

-- https://www.youtube.com/watch?v=2sykXdidZVA

-- 17:27
-- Discussion of CS410-Prelude (for use with exercise 2)
