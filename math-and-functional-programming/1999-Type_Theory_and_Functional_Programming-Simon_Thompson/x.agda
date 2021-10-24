module x where

open import Level

private
  variable
    a b c d e f ℓ p q r : Level
    A : Set a
    B : Set b
    C : Set c
    D : Set d
    E : Set e
    F : Set f


------------------------------------------------------------------------------

infixr 4 _,_
infixr 2 _^_

record _^_ (A : Set a) (B : Set b) : Set (a ⊔ b) where
  constructor _,_
  field
    fst : A
    snd : B
open _^_

swap : A ^ B → B ^ A
swap (a , b) = b , a

------------------------------------------------------------------------------

infixr 1 _v_

data _v_ (A : Set a) (B : Set b) : Set (a ⊔ b) where
  inl : (x : A) → A v B
  inr : (y : B) → A v B

cases : ∀ {A : Set a} {B : Set b} {C : Set c}
      → (A v B) → (A → C) → (B → C)
      → C
cases (inl q) f g = f q
cases (inr r) f g = g r

------------------------------------------------------------------------------

data ⊥ : Set where

⊥-elim : ∀ {A : Set a} → ⊥ → A
⊥-elim = λ ()

------------------------------------------------------------------------------

¬_ : Set → Set
¬ A = A → ⊥

------------------------------------------------------------------------------

-- 4.1
assoc-^ : (A ^ B) ^ C  → A ^ (B ^ C)
assoc-^  ((a , b) , c) = a , (b , c)

-- 4.2
e4-2 : (¬ A v B) → (A → B)
e4-2 (inl ¬a) = λ a → ⊥-elim (¬a a)
e4-2 (inr  b) = λ _ → b

{- not provable
e4-2' : (A → B) → (¬ A v B)
e4-2' a→b = {!!}
-}

-- 4.3
e4-3 : (A v ¬ A) → (¬ ¬ A → A)
e4-3 (inl  a) = λ   _ → a
e4-3 (inr ¬a) = λ ¬¬a → ⊥-elim (¬¬a ¬a)

e4-3' : (A → ¬ ¬ A)
e4-3' = λ a ¬¬a → ¬¬a a

-- 4.4 -- this is similar to v elimination by cases
e4-4 : ((A ^ B) → C) → A → (B → C)
e4-4 a^b→c a = λ b → a^b→c (a , b)

-- 4.5
e4-5 : (A → (B → C)) → ((A ^ B) → C)
e4-5 a→b→c = λ (a , b) → a→b→c a b

e4-5' : ((A ^ B) → C) → (A → (B → C))
e4-5' a^b→c = λ a → λ b → a^b→c (a , b)

-- 4.6
e4-6 : A → (B v C) → ((A ^ B) v (A ^ C))
e4-6 a (inl b) = inl (a , b)
e4-6 a (inr c) = inr (a , c)

-- 4.7
e4-7 : (A → C) → (B → D) → ((A ^ B) → (C ^ D))
e4-7 a→c b→d = λ (a , b) → (a→c a , b→d b)

-- section 4.5.2

-- function composition
_∘_ : (B → C) → (A → B) → (A → C)
_∘_ b→c a→b = λ a -> b→c (a→b a)

srl : (A → B) → (¬ B → ¬ A)
srl a→b = λ ¬b a → ¬b (a→b a)

-- section 4.5.3
idA^A : (A ^ A) → (A ^ A)
idA^A (a , a') = (a' , a)

-- section 4.5.5 Conjunction and disjunction

s455 : ((A v B) → C) → ((A → C) ^ (B → C))
s455 = λ avb→c → (avb→c ∘ inl , avb→c ∘ inr)

s455' : ((A → C) ^ (B → C)) → ((A v B) → C)
s455' (a→c , b→c) = λ { (inl a) → a→c a
                      ; (inr b) → b→c b }

dm : ¬ (A v B) → (¬ A ^ ¬ B)
dm notAvB = (notAvB ∘ inl , notAvB ∘ inr)

dm' : (¬ A v ¬ B) → ¬ (A ^ B)
dm' (inl ¬a) = λ (a , _) → ¬a a
dm' (inr ¬b) = λ (_ , b) → ¬b b

-- 4.8
e4-8a : A → ¬ ¬ A
e4-8a a = λ ¬a → ¬a a

e4-8b : (B v C) → ¬ (¬ B ^ ¬ C)
e4-8b (inl b) = λ (¬b ,  _) → ¬b b
e4-8b (inr c) = λ ( _ , ¬c) → ¬c c

e4-8c : (A → B) → ((A → C) → (A → (B ^ C)))
e4-8c a→b = λ a→c → λ a → (a→b a , a→c a)

