-- Length-indexed lists as a recursive family
-- From "Dependent Types at Work", section 3.1

module xx where

open import Level renaming (suc to lsuc)
open import Data.Nat hiding (_^_)
open import Data.Empty.Polymorphic
open import Data.Unit.Polymorphic
open import Data.Maybe hiding (map ; zip)
open import Data.Product hiding (map ; zip)

private
  variable
    m n : ℕ
    ℓ ℓ′ : Level
    A : Set ℓ
    B : Set ℓ′

-- Vec : Set ℓ → ℕ → Set ℓ
-- Vec A zero = ⊤
-- Vec A (suc n) = A × Vec A n

-- Generalize Vec

open import Function

iter : A → (A → A) → ℕ → A
iter z f zero    = z
iter z f (suc n) = f (iter z f n)

-- Is iter defined somewhere standard?

Vec : Set ℓ → ℕ → Set ℓ
Vec A = iter ⊤ (A ×_)

head : Vec A (suc n) → A
head (a , _) = a

tail : Vec A (suc n) → Vec A n
tail (_ , as) = as

map : (A → B) → Vec A n → Vec B n
map {n = zero } _ _ = tt
map {n = suc n} f (a , as) = f a , map {n = n} f as

zip : Vec A n → Vec B n → Vec (A × B) n
zip {n = zero } _ _ = tt
zip {n = suc n} (a , as) (b , bs) = (a , b) , zip {n = n} as bs

Fin : ℕ → Set
Fin zero    = ⊥
Fin (suc n) = Maybe (Fin n)

f5 : Fin 5
f5 = nothing
f4 : Fin 4
f4 = nothing
f3 : Fin 3
f3 = nothing
f2 : Fin 2
f2 = just nothing -- an element of the type
f1 : Fin 1
f1 = nothing
-- f0 : Fin 0
-- f0 = {!!} -- no solution found

_!_ : Vec A n → Fin n → A
_!_ {n = suc m} (a , _ ) nothing = a
_!_ {n = suc m} (_ , as) (just i) = as ! i

_!'_ : Vec A n → Fin n → A
_!'_ {n = suc m} (a , _)   nothing    = a
_!'_ {n = suc m} (_ , as) (just finm) = as !' finm

{-
(0 , 1 , 2 , tt) !                  nothing
(0 , 1 , 2 , tt) !             just nothing
(0 , 1 , 2 , tt) !       just (just nothing)
(0 , 1 , 2 , tt) ! just (just (just nothing))
-}

-- Perfect binary leaf tree
BTree : Set ℓ → ℕ → Set ℓ
BTree A = iter A (λ τ → τ × τ)
