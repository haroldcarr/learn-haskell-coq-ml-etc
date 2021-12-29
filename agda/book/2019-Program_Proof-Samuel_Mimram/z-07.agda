module z-07 where

open import Data.Bool        hiding (if_then_else_ ; _≟_ ; _<_ ; _<?_)
open import Data.Nat         renaming (_+_ to _+ℕ_ ; _<?_ to _<?ℕ_) hiding (_<_)
open import Relation.Binary.PropositionalEquality
open import Relation.Nullary

-- 7.1 Safety of a simple language : subject reduction and progress

data Value : Set where
  VNat  : ℕ    → Value
  VBool : Bool → Value

data Prog : Set where
  V             : Value → Prog
  _+_           : Prog  → Prog → Prog
  _<_           : Prog  → Prog → Prog
  if_then_else_ : Prog  → Prog → Prog → Prog

infix 50 _+_
infix 40 _<_
infix 30 if_then_else_

_<?_ : ℕ → ℕ → Bool
m <? n with m <?ℕ n
... | yes _ = true
... | no  _ = false

-- reduction relation
data _⇒_ : Prog → Prog → Set where
  ⇒-Add   : (m n : ℕ)
          → V (VNat  m) + V (VNat n)
          ⇒ V (VNat (m  +ℕ       n))
  ⇒-Add-l : {p p' : Prog} → p ⇒ p' → (q : Prog)
          → p  + q
          ⇒ p' + q
  ⇒-Add-r : {q q' : Prog} → (p : Prog) → q ⇒ q'
          → p + q
          ⇒ p + q'
  ⇒-Lt    : (m n : ℕ)
          → V (VNat   m) < V (VNat n)
          ⇒ V (VBool (m <?         n))
  ⇒-Lt-l  : {p p' : Prog} → p ⇒ p' → (q : Prog)
          → p  < q
          ⇒ p' < q
  ⇒-Lt-r  : {q q' : Prog} → (p : Prog) → q ⇒ q'
          → p < q
          ⇒ p < q'
  ⇒-If    : {p p' : Prog} → p ⇒ p' → (q r : Prog)
          → if p  then q else r
          ⇒ if p' then q else r
  ⇒-If-t  : (p q : Prog)
          → if V (VBool true ) then p else q ⇒ p
  ⇒-If-f  : (p q : Prog)
          → if V (VBool false) then p else q ⇒ q

_ : (V (VNat 3) + V (VNat 4)) ⇒ V (VNat 7)
_ = ⇒-Add 3 4

-- types
data Type : Set where
  TNat TBool : Type

-- typing relation
data ⊢_∷_ : Prog → Type → Set where
  ⊢-Nat  : (n : ℕ)
         → ⊢ V (VNat n)  ∷ TNat
  ⊢-Bool : (b : Bool)
         → ⊢ V (VBool b) ∷ TBool
  ⊢-Add  : {p q : Prog}
         → ⊢ p ∷ TNat → ⊢ q ∷ TNat
         → ⊢ p        +   q ∷ TNat
  ⊢-Lt   : {p q : Prog}
         → ⊢ p ∷ TNat → ⊢ q ∷ TNat
         → ⊢ p        <   q ∷ TBool
  ⊢-If   : {p q r : Prog} {A : Type}
         → ⊢ p ∷ TBool → ⊢ q ∷ A → ⊢ r ∷ A
         → ⊢ if p then q else r ∷ A

_ : ⊢ if V (VNat 3) < V (VNat 4) then V (VNat 3) else V (VNat 4) ∷ TNat
_ = ⊢-If (⊢-Lt (⊢-Nat 3) (⊢-Nat 4))
         (⊢-Nat 3)
         (⊢-Nat 4)

{-
-- type uniqueness property : theorem 1.4.3.1
in ⊢_∷_, the dependent pattern matching algorithm
- knows,
  - given the constructor of a program,
  - the possible types this program can have
- conversely
  - given a type
  - the possible program constructors which will give rise to this type)
therefore showing type uniqueness is almost immediate:
-}

tuniq : {p : Prog} {A A' : Type}
      → ⊢ p ∷ A → ⊢ p ∷ A'
      → A ≡ A'
tuniq (⊢-Nat  n)       (⊢-Nat     .n)    = refl
tuniq (⊢-Bool b)       (⊢-Bool    .b)    = refl
tuniq (⊢-Add  _  _)    (⊢-Add   _  _)    = refl
tuniq (⊢-Lt   _  _)    (⊢-Lt    _  _)    = refl
tuniq (⊢-If   _ lt lf) (⊢-If    _ rt rf) = tuniq lt rt -- 'tuniq lf rf' works too

-- subject reduction property : TODO

-- progress property : TODO
