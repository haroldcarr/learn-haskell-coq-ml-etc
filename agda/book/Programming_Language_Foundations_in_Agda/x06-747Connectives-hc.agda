module x06-747Connectives-hc where

import Relation.Binary.PropositionalEquality as Eq
open Eq using (_≡_; refl)
open Eq.≡-Reasoning
open import Data.Nat using (ℕ)
open import Function using (_∘_)

------------------------------------------------------------------------------
-- BEGIN : Copied from 747Isomorphism.

postulate
  extensionality : ∀ {A B : Set} {f g : A → B}
    → (∀ (x : A) → f x ≡ g x)
      -----------------------
    → f ≡ g

infix 0 _≃_
record _≃_ (A B : Set) : Set where
  constructor mk-≃  -- This has been added, not in PLFA
  field
    to   : A → B
    from : B → A
    from∘to : ∀ (x : A) → from (to x) ≡ x
    to∘from : ∀ (y : B) → to (from y) ≡ y
open _≃_

infix 0 _≲_
record _≲_ (A B : Set) : Set where
  field
    to      : A → B
    from    : B → A
    from∘to : ∀ (x : A) → from (to x) ≡ x
open _≲_

record _⇔_ (A B : Set) : Set where
  field
    to   : A → B
    from : B → A
open _⇔_

-- You may copy over the various reasoning modules if you wish.

-- END : Copied from 747Isomorphism.
------------------------------------------------------------------------------

------------------------------------------------------------------------------
-- logical AND is Cartesian product.

data _×_ (A : Set) (B : Set) : Set where

  ⟨_,_⟩ :
      A
    → B
      -----
    → A × B

-- destructors (aka eliminators) for ×.

proj₁ : ∀ {A B : Set}
  → A × B
    -----
  → A

proj₁ ⟨ a , _ ⟩ = a

proj₂ : ∀ {A B : Set}
  → A × B
    -----
  →     B

proj₂ ⟨ _ , b ⟩ = b

-- equivalent construction using records
record _×′_ (A B : Set) : Set where
  field
    proj₁′ : A
    proj₂′ : B
open _×′_

-- eta-equivalence relates constructors and destructors
η-× : ∀ {A B : Set} (w : A × B) → ⟨ proj₁ w , proj₂ w ⟩ ≡ w
η-× ⟨ a , b ⟩ = refl

-- type with two members
infixr 2 _×_
data Bool : Set where
  true  : Bool
  false : Bool

-- type with three members (used in examples)
data Tri : Set where
  aa : Tri
  bb : Tri
  cc : Tri

-- Bool × Tri has six members
×-count : Bool × Tri → ℕ
×-count ⟨ true  , aa ⟩  =  1
×-count ⟨ true  , bb ⟩  =  2
×-count ⟨ true  , cc ⟩  =  3
×-count ⟨ false , aa ⟩  =  4
×-count ⟨ false , bb ⟩  =  5
×-count ⟨ false , cc ⟩  =  6

-- Cartesian product is commutative and associative up to isomorphism.

swap : ∀ {A B : Set} → A × B → B × A
swap ⟨ a , b ⟩ = ⟨ b , a ⟩

×-comm : ∀ {A B : Set} → A × B ≃ B × A
to      ×-comm = swap
from    ×-comm = swap
from∘to ×-comm ⟨ _ , _ ⟩ = refl
to∘from ×-comm ⟨ _ , _ ⟩ = refl

×-assoc : ∀ {A B C : Set} → (A × B) × C ≃ A × (B × C)
to      ×-assoc ⟨ ⟨ a ,   b ⟩ , c ⟩   = ⟨   a , ⟨ b ,   c ⟩ ⟩
from    ×-assoc ⟨   a , ⟨ b ,   c ⟩ ⟩ = ⟨ ⟨ a ,   b ⟩ , c ⟩
from∘to ×-assoc ⟨ ⟨ _ ,   _ ⟩ , _ ⟩   = refl
to∘from ×-assoc ⟨   _ , ⟨ _ ,   _ ⟩ ⟩ = refl

-- A ⇔ B is isomorphic to (A → B) × (B → A)
iff-iso-if-onlyif : ∀ {A B : Set} → A ⇔ B ≃ (A → B) × (B → A)
_≃_.to         iff-iso-if-onlyif record { to = to ; from = from } = ⟨ to , from ⟩
to   (_≃_.from iff-iso-if-onlyif            ⟨ A→B , B→A ⟩)        = A→B
from (_≃_.from iff-iso-if-onlyif            ⟨ A→B , B→A ⟩)        = B→A
from∘to        iff-iso-if-onlyif record { to = to ; from = from } = refl
to∘from        iff-iso-if-onlyif            ⟨ A→B , B→A ⟩         = refl

------------------------------------------------------------------------------
-- logical True is a type with one member (unit)
data ⊤ : Set where
  tt :
    --
    ⊤

η-⊤ : ∀ (w : ⊤) → tt ≡ w
η-⊤ tt = refl

⊤-count : ⊤ → ℕ
⊤-count tt = 1

-- Unit is the left and right identity of product.

⊤-identityˡ : ∀ {A : Set} → ⊤ × A ≃ A
to      ⊤-identityˡ ⟨ ⊤  , a ⟩ =        a
from    ⊤-identityˡ        a   = ⟨ tt , a ⟩
from∘to ⊤-identityˡ ⟨ tt , a ⟩ = refl
to∘from ⊤-identityˡ        a   = refl

⊤-identityʳ : ∀ {A : Set} → (A × ⊤) ≃ A
to      ⊤-identityʳ ⟨ a , ⊤  ⟩ =   a
from    ⊤-identityʳ   a        = ⟨ a , tt ⟩
from∘to ⊤-identityʳ ⟨ a , tt ⟩ = refl
to∘from ⊤-identityʳ   a        = refl

------------------------------------------------------------------------------
-- logical OR (disjunction) is sum (disjoint union)

data _⊎_ : Set → Set → Set where

  inj₁ : ∀ {A B : Set}
    → A
      -----
    → A ⊎ B

  inj₂ : ∀ {A B : Set}
    →     B
      -----
    → A ⊎ B

-- a way to eliminate a sum (but easier to use pattern-matching to eliminate sums)
case-⊎ : ∀ {A B C : Set}
  → (A → C)
  → (B → C)
  →  A ⊎ B
    -----------
  →      C
case-⊎ f g (inj₁ x) = f x
case-⊎ f g (inj₂ x) = g x

-- eta equivalence for sums
η-⊎ : ∀ {A B : Set} (w : A ⊎ B) → case-⊎ inj₁ inj₂ w ≡ w
η-⊎ (inj₁ _) = refl
η-⊎ (inj₂ _) = refl

-- generalization
uniq-⊎ : ∀ {A B C : Set}
       → (h : A ⊎ B → C) → (w : A ⊎ B)
       → case-⊎ (h ∘ inj₁) (h ∘ inj₂) w ≡ h w
uniq-⊎ h (inj₁ _) = refl
uniq-⊎ h (inj₂ _) = refl

infix 1 _⊎_

-- Bool ⊎ Tri has five members
⊎-count : Bool ⊎ Tri → ℕ
⊎-count (inj₁ true)   =  1
⊎-count (inj₁ false)  =  2
⊎-count (inj₂ aa)     =  3
⊎-count (inj₂ bb)     =  4
⊎-count (inj₂ cc)     =  5

-- Sum is commutative up to isomorphism.
⊎-comm : ∀ {A B : Set} → A ⊎ B ≃ B ⊎ A
to      ⊎-comm (inj₁ a) = inj₂ a
to      ⊎-comm (inj₂ b) = inj₁ b
from    ⊎-comm (inj₁ b) = inj₂ b
from    ⊎-comm (inj₂ a) = inj₁ a
from∘to ⊎-comm (inj₁ _) = refl
from∘to ⊎-comm (inj₂ _) = refl
to∘from ⊎-comm (inj₁ _) = refl
to∘from ⊎-comm (inj₂ _) = refl

-- Sum is associative up to isomorphism.
⊎-assoc : ∀ {A B C : Set} → (A ⊎ B) ⊎ C ≃ A ⊎ (B ⊎ C)
to      ⊎-assoc (inj₁ (inj₁ a)) = inj₁ a
to      ⊎-assoc (inj₁ (inj₂ b)) = inj₂ (inj₁ b)
to      ⊎-assoc (inj₂ c)        = inj₂ (inj₂ c)
from    ⊎-assoc (inj₁ a)        = inj₁ (inj₁ a)
from    ⊎-assoc (inj₂ (inj₁ b)) = inj₁ (inj₂ b)
from    ⊎-assoc (inj₂ (inj₂ c)) = inj₂ c
from∘to ⊎-assoc (inj₁ (inj₁ _)) = refl
from∘to ⊎-assoc (inj₁ (inj₂ _)) = refl
from∘to ⊎-assoc (inj₂ _)        = refl
to∘from ⊎-assoc (inj₁ _)        = refl
to∘from ⊎-assoc (inj₂ (inj₁ _)) = refl
to∘from ⊎-assoc (inj₂ (inj₂ _)) = refl

------------------------------------------------------------------------------
-- logical False is the empty type ("bottom", "empty")

data ⊥ : Set where
  -- no clauses

-- Ex falso quodlibet "from falsehood, anything follows".
⊥-elim : ∀ {A : Set}
  → ⊥
    --
  → A

⊥-elim ()

uniq-⊥ : ∀ {C : Set}
       → (h : ⊥ → C) → (w : ⊥)
       → ⊥-elim w ≡ h w
uniq-⊥ h ()

⊥-count : ⊥ → ℕ
⊥-count ()

-- Empty is the left unit of sum up to isomorphism.
⊎-identityˡ : ∀ {A : Set} → ⊥ ⊎ A ≃ A
to      ⊎-identityˡ (inj₂ a) = a
from    ⊎-identityˡ       a  = inj₂ a
from∘to ⊎-identityˡ (inj₂ _) = refl
to∘from ⊎-identityˡ       _  = refl

-- Empty is the right unit of sum up to isomorphism.
⊎-identityʳ : ∀ {A : Set} → A ⊎ ⊥ ≃ A
to      ⊎-identityʳ (inj₁ a) = a
from    ⊎-identityʳ       a  = inj₁ a
from∘to ⊎-identityʳ (inj₁ _) = refl
to∘from ⊎-identityʳ       _  = refl

------------------------------------------------------------------------------
-- logical implication (if-then) is the function TYPE constructor.
-- Eliminating an if-then (modus ponens) is function application.

→-elim : ∀ {A B : Set}
  → (A → B)
  →  A
    -------
  →      B

→-elim L M = L M

-- →-elim works because eta-reduction for → is built in

η-→ : ∀ {A B : Set}
    → (f : A → B)
    → (λ (x : A) → f x) ≡ f
η-→ f = refl

-- The function space A → B is called the exponential Bᴬ.
-- Bool → Tri has 3² (9) members.
→-count : (Bool → Tri) → ℕ
→-count f with f true | f false
...          | aa     | aa      =   1
...          | aa     | bb      =   2
...          | aa     | cc      =   3
...          | bb     | aa      =   4
...          | bb     | bb      =   5
...          | bb     | cc      =   6
...          | cc     | aa      =   7
...          | cc     | bb      =   8
...          | cc     | cc      =   9


------------------------------------------------------------------------------
-- "currying" : functions of multiple parameters
-- math  : (p ^ n) ^ m = p ^ (n * m)
-- types : (A ^ B) ^ C ≃ A ^ (B × C)
currying : ∀ {A B C : Set} → (A → B → C) ≃ (A × B → C)
to      currying f = λ { ⟨ a , b ⟩ → f a b }
from    currying f = λ a b →  f ⟨ a , b ⟩
from∘to currying _ = refl
to∘from currying f = extensionality λ { ⟨ x , y ⟩ → refl }

-- math, : p ^ (n + m) =  (p ^ n) * (p ^ m)
-- types : (A ⊎ B → C) ≃ ((A → C) × (B → C))
→-distrib-⊎ : ∀ {A B C : Set} → (A ⊎ B → C) ≃ ((A → C) × (B → C))
to      →-distrib-⊎ f             = ⟨ (λ a → f (inj₁ a)) , (λ b → f (inj₂ b)) ⟩
from    →-distrib-⊎ ⟨ A→C , B→C ⟩ = λ { (inj₁ a) → A→C a ; (inj₂ b) → B→C b }
from∘to →-distrib-⊎ _             = extensionality λ { (inj₁ x) → refl ; (inj₂ x) → refl }
to∘from →-distrib-⊎               = λ{ ⟨ g , h ⟩ → refl }

-- math  : (p * n) ^ m = (p ^ m) * (n ^ m)
-- types : (A → B × C) ≃ (A → B) × (A → C)
→-distrib-× : ∀ {A B C : Set} → (A → B × C) ≃ (A → B) × (A → C)
to      →-distrib-× f = ⟨ (λ a → proj₁ (f a)) , (λ a → proj₂ (f a)) ⟩
from    →-distrib-× f = λ a → ⟨ proj₁ f a , proj₂ f a ⟩
from∘to →-distrib-×   = λ a→b×c → extensionality λ a → η-× (a→b×c a)
to∘from →-distrib-×   = λ { ⟨ _ , _ ⟩ → refl }

×-distrib-⊎ : ∀ {A B C : Set} → (A ⊎ B) × C ≃ (A × C) ⊎ (B × C)
to      ×-distrib-⊎ ⟨ inj₁ a , c ⟩   = inj₁ ⟨ a , c ⟩
to      ×-distrib-⊎ ⟨ inj₂ b , c ⟩   = inj₂ ⟨ b , c ⟩
from    ×-distrib-⊎ (inj₁ ⟨ a , c ⟩) = ⟨ inj₁ a , c ⟩
from    ×-distrib-⊎ (inj₂ ⟨ b , c ⟩) = ⟨ inj₂ b , c ⟩
from∘to ×-distrib-⊎                  = λ { ⟨ inj₁   a , c ⟩  → refl ; ⟨ inj₂   b , c ⟩  → refl }
to∘from ×-distrib-⊎                  = λ { ( inj₁ ⟨ a , c ⟩) → refl ; ( inj₂ ⟨ b , c ⟩) → refl }

-- Think of a counterexample to show this is not an isomorphism. TODO
⊎-distrib-× : ∀ {A B C : Set} → (A × B) ⊎ C ≲ (A ⊎ C) × (B ⊎ C)
to      ⊎-distrib-× (inj₁ ⟨ a , b ⟩)    = ⟨ inj₁ a , inj₁ b ⟩
to      ⊎-distrib-× (inj₂ c)            = ⟨ inj₂ c , inj₂ c ⟩
from    ⊎-distrib-× ⟨ inj₁ a , inj₁ b ⟩ = inj₁ ⟨ a , b ⟩
from    ⊎-distrib-× ⟨ inj₁ a , inj₂ c ⟩ = inj₂ c
from    ⊎-distrib-× ⟨ inj₂ c , inj₁ b ⟩ = inj₂ c
from    ⊎-distrib-× ⟨ inj₂ c , inj₂ C ⟩ = inj₂ c -- inj₂ C also valid (loses info)
from∘to ⊎-distrib-×                     = λ { (inj₁ ⟨ a , b ⟩) → refl ; (inj₂ c) → refl }

×-distrib-→ : ∀ {A B C : Set} → (C → (A × B)) ≃ (C → A) × (C → B)
to      ×-distrib-→ f = ⟨ (λ c →   proj₁ (f c)) , (λ c → proj₂ (f c)) ⟩
from    ×-distrib-→ f =    λ c → ⟨ proj₁  f c   ,        proj₂  f c   ⟩
from∘to ×-distrib-→   =    λ { c→a×b → extensionality λ c → η-× (c→a×b c) }
to∘from ×-distrib-→   =    λ { ⟨ c→a , c→b ⟩ → refl }

⊎-distrib-→ : ∀ {A B C : Set} → ((A ⊎ B) → C) ≃ (A → C) × (B → C)
to      ⊎-distrib-→ f = ⟨ (λ a →  f (inj₁ a)) , (λ b → f (inj₂ b)) ⟩
from    ⊎-distrib-→ f = λ { (inj₁ a) → (proj₁ f a) ; (inj₂ b) → (proj₂ f b) }
--from∘to ⊎-distrib-→ f = extensionality λ a⊎b → {!!} -- TODO
from∘to ⊎-distrib-→   = λ { a⊎b→c → extensionality λ { (inj₁ a) → refl ; (inj₂ b) → refl } }
to∘from ⊎-distrib-→   = λ { ⟨ a→c , b→c ⟩ → refl }

-- PLFA exercise: a weak distributive law.
⊎-weak-× : ∀ {A B C : Set} → (A ⊎ B) × C → A ⊎ (B × C)
⊎-weak-× ⟨ inj₁ a , c ⟩ = inj₁ a
⊎-weak-× ⟨ inj₂ b , c ⟩ = inj₂ ⟨ b , c ⟩
-- State and prove the strong law, and explain the relationship. TODO

-- SumOfProdImpProdOfSum : disjunct of conjuncts implies a conjunct of disjuncts.
⊎×-implies-×⊎ : ∀ {A B C D : Set}
              → (A × B) ⊎ (C × D)
              → (A ⊎ C) × (B ⊎ D)
⊎×-implies-×⊎ (inj₁ ⟨ a , b ⟩) = ⟨ inj₁ a , inj₁ b ⟩
⊎×-implies-×⊎ (inj₂ ⟨ c , d ⟩) = ⟨ inj₂ c , inj₂ d ⟩

-- the converse is NOT true
×⊎-implies-⊎×-not : ∀ {A B C D : Set}
                  → (A ⊎ C) × (B ⊎ D)
                  → (A × B) ⊎ (C × D)
×⊎-implies-⊎×-not ⟨ inj₁ a , inj₁ b ⟩ = inj₁ ⟨ a , b ⟩
×⊎-implies-⊎×-not ⟨ inj₁ a , inj₂ d ⟩ = {!!} -- A × B ⊎ C × D -- not possible
×⊎-implies-⊎×-not ⟨ inj₂ c , inj₁ b ⟩ = {!!} -- A × B ⊎ C × D -- not possible
×⊎-implies-⊎×-not ⟨ inj₂ c , inj₂ d ⟩ = inj₂ ⟨ c , d ⟩



