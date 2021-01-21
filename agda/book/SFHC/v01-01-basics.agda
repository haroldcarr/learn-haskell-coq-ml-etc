-- this is so this can be imported even though it has unresolved holes
{-# OPTIONS --allow-unsolved-metas #-}

module v01-01-basics where

import Relation.Binary.PropositionalEquality as Eq
open Eq             using (_≡_; refl)
open Eq.≡-Reasoning using (begin_; _≡⟨⟩_; _∎)

data day : Set where
 mon : day
 tue : day
 wed : day
 thu : day
 fri : day
 sat : day
 sun : day

next-weekday : day → day
next-weekday mon = tue
next-weekday tue = wed
next-weekday wed = thu
next-weekday thu = fri
next-weekday fri = mon
next-weekday sat = mon
next-weekday sun = mon

_ : next-weekday fri ≡ mon
_ = refl

_ : next-weekday (next-weekday sat) ≡ tue
_ = refl

data bool : Set where
  true  : bool
  false : bool

negb : bool → bool
negb true  = false
negb false = true

andb : bool → bool → bool
andb  true b2 = b2
andb false  _ = false

orb : bool → bool → bool
orb  true  _ = true
orb false b2 = b2

_ : orb false false ≡ false
_ = refl
_ : orb false  true ≡ true
_ = refl
_ : orb  true false ≡ true
_ = refl
_ : orb  true  true ≡ true
_ = refl

nandb : bool → bool → bool
nandb true true = false
nandb    _    _ = true

_ : nandb false false ≡ true
_ = refl
_ : nandb false  true ≡ true
_ = refl
_ : nandb  true false ≡ true
_ = refl
_ : nandb  true  true ≡ false
_ = refl

andb3 : bool → bool → bool → bool
andb3 true true true = true
andb3    _    _    _ = false

_ : andb3  true  true  true ≡ true
_ = refl
_ : andb3 false  true  true ≡ false
_ = refl
_ : andb3  true false  true ≡ false
_ = refl
_ : andb3  true  true false ≡ false
_ = refl

data rgb : Set where
  red   : rgb
  green : rgb
  blue  : rgb

data color : Set where
  black   : color
  white   : color
  primary : rgb → color

monochrome : color → bool
monochrome black = true
monochrome white = true
monochrome (primary _) = false

isred : color → bool
isred (primary red) = true
isred            _  = false

data bit : Set where
  B0 : bit
  B1 : bit

data nybble : Set where
  bits : (b0 b1 b2 b3 : bit) → nybble

all-zero : nybble → bool
all-zero (bits B0 B0 B0 B0) = true
all-zero                 _  = false

_ : all-zero (bits B1 B0 B1 B0) ≡ false
_ = refl
_ : all-zero (bits B0 B0 B0 B0) ≡ true
_ = refl

data nat : Set where
  O : nat
  S : nat → nat

pred : nat → nat
pred    O  = O
pred (S n) = n

{-# BUILTIN NATURAL nat #-}

_ : S (S (S (S O))) ≡ 4
_ = refl

minustwo : nat → nat
minustwo       O   = O
minustwo    (S O)  = O
minustwo (S (S n)) = n

_ : minustwo 4 ≡ 2
_ = refl

evenb : nat → bool
evenb       O   = true
evenb    (S O)  = false
evenb (S (S n)) = evenb n

oddb : nat → bool
oddb n = negb (evenb n)

_ : oddb 1 ≡ true
_ = refl
_ : oddb 4 ≡ false
_ = refl

plus : nat → nat → nat
plus    O  m = m
plus (S n) m = S (plus n m)

_ : plus 3 2 ≡ 5
_ = refl

mult : nat → nat → nat
mult    O  m = 0
mult (S n) m = plus m (mult n m)

_ : mult 3 3 ≡ 9
_ = refl

minus : nat → nat → nat
minus    O     _  = O
minus    x     O  = x
minus (S n) (S m) = minus n m

exp : (base power : nat) → nat
exp _    O  = S O
exp b (S p) = mult b (exp b p)

_ : exp 2 0 ≡ 1
_ = refl
_ : exp 2 1 ≡ 2
_ = refl
_ : exp 2 2 ≡ 4
_ = refl
_ : exp 2 3 ≡ 8
_ = refl

factorial : nat → nat
factorial    O  = 1
factorial (S n) = mult (S n) (factorial n)

_ : factorial 3 ≡ 6
_ = refl
_ : factorial 5 ≡ mult 10 12
_ = refl

_+_ : nat → nat → nat
_+_ = plus

_-_ : nat → nat → nat
_-_ = minus

_*_ : nat → nat → nat
_*_ = mult

infixl 6  _+_  _-_
infixl 7  _*_
{-# BUILTIN NATPLUS  _+_ #-}
{-# BUILTIN NATTIMES _*_ #-}
{-# BUILTIN NATMINUS _-_ #-}

eqb : (n m : nat) → bool
eqb    O     O  = true
eqb    O  (S _) = false
eqb (S _)    O  = false
eqb (S l) (S r) = eqb l r


leb : (n m : nat) → bool
leb    O     _  = true
leb (S n)    O  = false
leb (S n) (S m) = leb n m

_ : leb 2 2 ≡ true
_ = refl
_ : leb 2 4 ≡ true
_ = refl
_ : leb 4 2 ≡ false
_ = refl

_=?_ : (n m : nat) → bool
_=?_ = eqb

_<=?_ : (n m : nat) → bool
_<=?_ = leb

infix 4 _=?_
infix 4 _<=?_

_ : (4 <=? 2) ≡ false
_ = refl

ltb : (n m : nat) → bool
ltb    O     O  = false
ltb    O  (S m) = true
ltb (S n)    O  = false
ltb (S n) (S m) = ltb n m

_<?_ : (n m : nat) → bool
_<?_ = ltb
infix 4 _<?_

_ : ltb 2 2 ≡ false
_ = refl
_ : ltb 2 4 ≡ true
_ = refl
_ : ltb 4 2 ≡ false
_ = refl

-- proof by simplification

plus-O-n : ∀ {n : nat} → 0 + n ≡ n
plus-O-n = refl

plus-1-l : ∀ {n : nat} → 1 + n ≡ S n
plus-1-l = refl

mult-O-l : ∀ {n : nat} → 0 * n ≡ 0
mult-O-l = refl

-- proof by rewriting

plus-id-example : ∀ {n m : nat}
  →     n ≡ m
  → n + n ≡ m + m
plus-id-example n≡m rewrite n≡m = refl

plus_id_exercise : ∀ {n m o : nat}
  → n     ≡ m
  →     m ≡     o
  → n + m ≡ m + o
plus_id_exercise n≡m m≡o rewrite n≡m | m≡o = refl

*0 : ∀ (m : nat) → m * 0 ≡ 0
*0    0  = refl
*0 (S m) = *0 m

mult-n-O : ∀ (n : nat) → 0 ≡ n * 0
mult-n-O n rewrite *0 n = refl

*1 : ∀ (n : nat) → n * 1 ≡ n
*1    0 = refl
*1 (S n) rewrite *1 n = refl

1* : ∀ (n : nat) → 1 * n ≡ n
1*    0 = refl
1* (S n) rewrite 1* n = refl

mult-n-Sm : ∀ (n m : nat) → n * m + n ≡ n * S m
mult-n-Sm    O     m  = refl
mult-n-Sm    n     O  rewrite *0 n | plus-O-n {n} | *1 n = refl
mult-n-Sm (S n) (S m) = {!!} -- TODO

mult-n-0-m-0 : ∀ (p q : nat)
  → (p * 0) + (q * 0) ≡ 0
mult-n-0-m-0 p q rewrite *0 p | *0 q = refl

mult-n-1 : ∀ (p : nat) → p * 1 ≡ p
mult-n-1 = *1

-- proof by case analysis

plus-1-neq-0 : ∀ (n : nat) → ((n + 1) =? 0) ≡ false
plus-1-neq-0    O  = refl
plus-1-neq-0 (S n) rewrite plus-1-neq-0 n = refl

negb-involutive : ∀ (b : bool) → negb (negb b) ≡ b
negb-involutive true  = refl
negb-involutive false = refl

andb-commutative : ∀ (b c : bool) → andb b c ≡ andb c b
andb-commutative false false = refl
andb-commutative false  true = refl
andb-commutative  true false = refl
andb-commutative  true  true = refl

andb3-exchange : ∀ (b c d : bool) → andb (andb b c) d ≡ andb (andb b d) c
andb3-exchange  true  true  true = refl
andb3-exchange  true  true false = refl
andb3-exchange  true false  true = refl
andb3-exchange  true false false = refl
andb3-exchange false  true  true = refl
andb3-exchange false  true false = refl
andb3-exchange false false  true = refl
andb3-exchange false false false = refl

andb-true-elim2 : ∀ (b c : bool)
  → andb b c ≡ true
  →        c ≡ true
andb-true-elim2 false      _ ()
andb-true-elim2  true  false ()
andb-true-elim2  true   true _ = refl

zero-nbeq-plus-1 : ∀ (n : nat) → (0 =? (n + 1)) ≡ false
zero-nbeq-plus-1    O  = refl
zero-nbeq-plus-1 (S n) rewrite  zero-nbeq-plus-1 n = refl

identity-fn-applied-twice : ∀ {f : bool → bool} (b : bool)
  →    f b  ≡ b
  → f (f b) ≡ b
identity-fn-applied-twice b p rewrite p | p = refl

negation-fn-applied-twice : ∀ {f : bool → bool} {b : bool} -- TODO
  →    f b  ≡ negb b
  → f (f b) ≡ b
negation-fn-applied-twice {f} {false} p = {!!} -- TODO
negation-fn-applied-twice {f}  {true} p = {!!} -- TODO

andb-eq-orb : ∀ (b c : bool)
  → andb b c ≡ orb b c
  →      b   ≡       c
andb-eq-orb  true c p rewrite p = refl
andb-eq-orb false c p rewrite p = refl

-- NOTE: lowest order bit is on LEFT
data bin : Set where
  Z  : bin
  B₀ : bin → bin
  B₁ : bin → bin

incr : bin → bin
incr     Z  = B₁ Z
incr (B₀ x) = B₁ x
incr (B₁ x) = B₀ (incr x)

bin-to-nat : bin → nat
bin-to-nat     Z  = 0
bin-to-nat (B₀ x) =     2 * (bin-to-nat x)
bin-to-nat (B₁ x) = 1 + 2 * (bin-to-nat x)

_ :     (incr (B₁ Z))  ≡     B₀ (B₁ Z)
_ = refl
_ : (incr (B₀ (B₁ Z))) ≡     B₁ (B₁ Z)
_ = refl
_ : (incr (B₁ (B₁ Z))) ≡ B₀ (B₀ (B₁ Z))
_ = refl

_ : bin-to-nat         (B₀ (B₁ Z))  ≡ 2
_ = refl
_ : bin-to-nat       (incr (B₁ Z))  ≡ 1 + bin-to-nat (B₁ Z)
_ = refl
_ : bin-to-nat (incr (incr (B₁ Z))) ≡ 2 + bin-to-nat (B₁ Z)
_ = refl
