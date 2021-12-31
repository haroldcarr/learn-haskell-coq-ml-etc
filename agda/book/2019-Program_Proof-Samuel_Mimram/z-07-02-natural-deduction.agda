module z-07-02-natural-deduction where

open import Data.Bool        hiding (if_then_else_ ; _≟_ ; _<_ ; _<?_)
open import Data.Empty
open import Data.Maybe
open import Data.Nat         renaming (_+_ to _+ℕ_ ; _<?_ to _<?ℕ_) hiding (_<_)
open import Data.Sum
open import Data.Unit
open import Function
open import Relation.Binary.PropositionalEquality
open import Relation.Nullary

{-
------------------------------------------------------------------------------
-- 7.2 Natural deduction

proofs in natural deduction presented in section 2.2

now present Agda formalization

only present here the implicative fragment.
-}

data Formula : Set where
  X   : ℕ → Formula                 -- a variable (whose name is given by a natural
  _⇒_ : Formula → Formula → Formula -- or the implication of two formulas (see section 2.2.1)

data Context : Set where
  ε   : Context                                 -- the empty context
  _,_ : (Γ : Context) → (A : Formula) → Context -- a pair of a context Γ and a formula A

-- could have formalized contexts as lists of formulas, but above gives more natural notation

-- concatenation of contexts
_,,_ : Context → Context → Context
Γ ,, ε = Γ
Γ ,, (Δ , A) = (Γ ,, Δ) , A

-- PROVABLE SEQUENTS, defined as an inductive predicate,
-- with one constructor corresponding to each inference rule
data _⊢'_ : Context → Formula → Set where
  ax : ∀ {Γ A Γ'}                  →  ((Γ , A) ,, Γ') ⊢' A -- axiom rule
  ⇒E : ∀ {Γ A B}   →  Γ ⊢' (A ⇒ B)  →  Γ ⊢' A → Γ ⊢' B     -- implication elimination
  ⇒I : ∀ {Γ A B}   →  (Γ , A) ⊢' B  →  Γ ⊢' (A ⇒ B)        -- implication introduction

{-
_⊢'_ is not convenient because the arg of 'ax' constructor uses concatenation
(_,,_)  which is a function (not a type constructor)
and will prevent pattern matching from working.
Specifically: this function does not have the property that

  Γ ,, Δ = Γ' ,, Δ'    implies   Γ = Γ’   and Δ = Δ’

to avoid this, use alternate def:
-}

data _⊢_ : Context → Formula → Set where
  ax : ∀ {Γ A }                                  →   (Γ , A) ⊢ A -- axiom rule
  wk : ∀ {Γ A B}   →   Γ ⊢ B                     →   (Γ , A) ⊢ B -- axiom rule
  ⇒E : ∀ {Γ A B}   →   Γ ⊢ (A ⇒ B)   →   Γ ⊢ A   →   Γ ⊢ B       -- implication elimination
  ⇒I : ∀ {Γ A B}   →   (Γ , A) ⊢ B               →   Γ ⊢ (A ⇒ B) -- implication introduction

{-
replaces the axiom rule

    ------------ (ax)
    Γ, A, Γ' ⊢ A

by two rules
                                  Γ ⊢ B
    -------- (ax)               --------- (wk)
    Γ, A ⊢ A                      Γ, A ⊢ B

giving an equivalent logical system.

ADMISSIBLE RULES : used to show that rules are admissible,
e.g., prove contraction rule

           Γ,A,A,Γ' ⊢ B
           ------------ (contr)
             Γ,A,Γ' ⊢ B
is admissible (see section 2.2.7) by induction,
both on the context Γ' and on the proof of the premise, by

-- TODO
-}
