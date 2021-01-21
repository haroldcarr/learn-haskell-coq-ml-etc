module v01-02-induction where

import Relation.Binary.PropositionalEquality as Eq
open Eq             using (_≡_; refl; sym)
open Eq.≡-Reasoning using (begin_; _≡⟨⟩_; _∎)
open import v01-01-basics

plus-n-O : ∀ (n : nat) → n ≡ n + 0
plus-n-O    O  = refl
plus-n-O (S n) rewrite sym (plus-n-O n) = refl

minus-n-n : ∀ (n : nat) → minus n n ≡ 0
minus-n-n    O  = refl
minus-n-n (S n) rewrite sym (minus-n-n n) = refl

mult-0-r : ∀ (n : nat) → n * 0 ≡ 0
mult-0-r    O  = refl
mult-0-r (S n) rewrite mult-0-r n = refl

plus-n-Sm : ∀ (n m : nat) → S (n + m) ≡ n + (S m)
plus-n-Sm    O  m = refl
plus-n-Sm (S n) m rewrite sym (plus-n-Sm n m) = refl

plus-comm : ∀ (n m : nat) → n + m ≡ m + n
plus-comm    n     O  rewrite sym (plus-n-O n) = refl
plus-comm    O  (S m) rewrite sym (plus-n-O m) = refl
plus-comm (S n) (S m)
  rewrite
    sym (plus-n-Sm n m)
  | sym (plus-n-Sm m n)
  | plus-comm n m
  = refl

plus-assoc : ∀ (n m p : nat) → n + (m + p) ≡ (n + m) + p
plus-assoc    O  m p = refl
plus-assoc (S n) m p        --         S n + (m + p) ≡ S n + m + p
  rewrite
    plus-comm (S n + m) p        -- S (plus n (plus m p)) ≡ plus p (S (plus n m))
  | sym (plus-n-Sm p (plus n m)) -- S (plus n (plus m p)) ≡ S (plus p (plus n m))
  | plus-comm p (plus n m)       -- S (plus n (plus m p)) ≡ S (plus (plus n m) p)
  | plus-assoc n m p             -- S (plus (plus n m) p) ≡ S (plus (plus n m) p)
  = refl

double : (n : nat) → nat
double    O  = O
double (S n) = S (S (double n))

double-plus : ∀ (n : nat) → double n ≡ n + n
double-plus    O  = refl
double-plus (S n)          --     double (S n) ≡ S n + S n
                           -- S (S (double n)) ≡ S n + S n
  rewrite
    double-plus n          -- S (S (plus n n)) ≡ S (plus n (S n))
  | plus-n-Sm n n          -- S (plus n (S n)) ≡ S (plus n (S n))
  = refl

{-
Inconvenient in evenb def: recursive call on n - 2.
Makes inductibe proofs about evenb harder since may need IH about n - 2.
Following lemma gives alternative characterization of evenb (S n) that works better with induction.
-}

evenb-S : ∀ (n : nat) → evenb (S n) ≡ negb (evenb n)
evenb-S    O  = refl
evenb-S (S n)                 -- evenb (S (S n)) ≡ negb (evenb (S n))
                              --         evenb n ≡ negb (evenb (S n))
  rewrite
    evenb-S n                 --         evenb n ≡ negb (negb (evenb n))
  | negb-involutive (evenb n) --         evenb n ≡ evenb n
  = refl

mult-0-plus' : ∀ (n m : nat) → (0 + n) * m ≡ n * m
mult-0-plus'    O  m = refl
mult-0-plus' (S n) m       --     (0 + S n) * m ≡ S n * m
  rewrite plus-O-n {n}     -- plus m (mult n m) ≡ plus m (mult n m)
  = refl

plus-rearrange : ∀ (n m p q : nat)
  → (n + m) + (p + q) ≡ (m + n) + (p + q)
plus-rearrange n m p q
  rewrite plus-comm n m
  = refl

plus-swap : ∀ (n m p : nat)
  → n + (m + p) ≡ m + (n + p)
plus-swap n m p
  rewrite
    plus-assoc n m p
  | plus-comm  n m
  | plus-assoc m n p
  = refl

mult-comm : ∀ (m n : nat)
  → m * n ≡ n * m
mult-comm    O  n
  rewrite
    mult-O-l {n}
  | *0 n
  = refl
mult-comm (S m) n       --           S m * n ≡ n * S m
  rewrite               -- plus n (mult m n) ≡ plus (mult n m) n
    sym (mult-n-Sm n m)
  | plus-comm n (mult m n)
  | mult-comm m n
  = refl

leb-refl : ∀ (n : nat) → true ≡ (n <=? n)
leb-refl    O  = refl
leb-refl (S n) = leb-refl n

zero-nbeq-S : ∀ (n : nat) → (0 =? (S n)) ≡ false
zero-nbeq-S n = refl

andb-false-r : ∀ (b : bool) → andb b false ≡ false
andb-false-r true  = refl
andb-false-r false = refl

plus-ble-compat-l : ∀ (n m p : nat)
  → (n <=? m) ≡ true
  → ((p + n) <=? (p + m)) ≡ true
plus-ble-compat-l _ _    O  n<=?m = n<=?m
plus-ble-compat-l n m (S p) n<=?m = plus-ble-compat-l n m p n<=?m

S-nbeq-0 : ∀ (n : nat) → ((S n) =? 0) ≡ false
S-nbeq-0 n = refl

mult-1-l : ∀ (n : nat) → 1 * n ≡ n
mult-1-l    O  = refl
mult-1-l (S n) rewrite mult-1-l n = refl

all3-spec : ∀ (b c : bool)
  → orb
      (andb b c)
      (orb (negb b)
               (negb c))
  ≡ true
all3-spec  true  true = refl
all3-spec  true false = refl
all3-spec false  true = refl
all3-spec false false = refl

mult-plus-distr-r : ∀ (n m p : nat)
  → (n + m) * p ≡ (n * p) + (m * p)
mult-plus-distr-r    O  m p = refl
mult-plus-distr-r (S n) m p      --   (S n     + m) * p
                                 -- ≡  S n * p + m  * p
  rewrite
    mult-comm (S n + m) p        --   mult p (S (plus n m))
                                 -- ≡ plus (plus p (mult n p)) (mult m p)
  | sym (mult-n-Sm p (plus n m)) --   plus (mult p (plus n m)) p
                                 -- ≡ plus (plus p (mult n p)) (mult m p)
  | mult-comm p (plus n m)       --   plus (mult (plus n m) p) p
                                 -- ≡ plus (plus p (mult n p)) (mult m p)
  | mult-plus-distr-r n m p      --   plus (plus (mult n p) (mult m p)) p
                                 -- ≡ plus (plus p (mult n p)) (mult m p)
  | plus-comm (plus (mult n p) (mult m p)) p
                                 --   plus p (plus (mult n p) (mult m p))
                                 -- ≡ plus (plus p (mult n p)) (mult m p)
  | plus-assoc p (mult n p) (mult m p)
                                 --   plus (plus p (mult n p)) (mult m p)
                                 -- ≡ plus (plus p (mult n p)) (mult m p)
  = refl

mult-assoc : ∀ (n m p : nat)
  → n * (m * p) ≡ (n * m) * p
mult-assoc    O  m p = refl
mult-assoc (S n) m p                 -- S n * (m * p) ≡ S n * m * p
  rewrite
    mult-plus-distr-r m (mult n m) p -- plus (mult m p) (mult n (mult m p)) ≡
                                     -- plus (mult m p) (mult (mult n m) p)
  | mult-assoc n m p                 -- plus (mult m p) (mult (mult n m) p) ≡
                                     -- plus (mult m p) (mult (mult n m) p)
  = refl

eqb-refl : ∀ (n : nat) → true ≡ (n =? n)
eqb-refl    O  = refl
eqb-refl (S n) = eqb-refl n

plus-swap' : ∀ (n m p : nat)
  → n + (m + p) ≡ m + (n + p)
plus-swap' n m p           -- n + (m + p) ≡ m + (n + p)
  rewrite
    plus-assoc n m p       -- plus (plus n m) p ≡ plus m (plus n p)
  | plus-comm n m          -- plus (plus m n) p ≡ plus m (plus n p)
  | sym (plus-assoc m n p) -- plus m (plus n p) ≡ plus m (plus n p)
  = refl

{-

Exercise: 3 stars, standard, especially useful (binary_commute)
Prove this diagram commutes:

                            incr
              bin ----------------------> bin
               |                           |
    bin_to_nat |                           |  bin_to_nat
               |                           |
               v                           v
              nat ----------------------> nat
                             S

incrementing a binary number and then converting it to a (unary) natural number
yields the same result as
first converting it to a natural number and then incrementing
-}


bin-to-nat' : bin → nat
bin-to-nat'     Z  = 0
bin-to-nat' (B₀ x) =      double (bin-to-nat' x)
bin-to-nat' (B₁ x) = 1 + (double (bin-to-nat' x))

_ : bin-to-nat'         (B₀ (B₁ Z))  ≡ 2
_ = refl
_ : bin-to-nat'       (incr (B₁ Z))  ≡ 1 + bin-to-nat' (B₁ Z)
_ = refl
_ : bin-to-nat' (incr (incr (B₁ Z))) ≡ 2 + bin-to-nat' (B₁ Z)
_ = refl

--         PREServes
bin-to-nat-pres-incr : (b : bin) (n : nat)
  → bin-to-nat'       b  ≡   n
  → bin-to-nat' (incr b) ≡ S n
bin-to-nat-pres-incr     Z  n b≡n --           bin-to-nat (incr Z) ≡ S n
                                  --                             1 ≡ S n ; b≡n : 0 ≡ n
  rewrite sym b≡n                 --                             1 ≡ 1
  = refl
bin-to-nat-pres-incr (B₀ b) n b≡n --     bin-to-nat' (incr (B₀ b)) ≡ S n
                                  --    1 + double (bin-to-nat' b) ≡ S n
  rewrite b≡n                     --                           S n ≡ S n
  = refl
bin-to-nat-pres-incr (B₁ b) n b≡n --     bin-to-nat' (incr (B₁ b)) ≡ S n
                                  -- double (bin-to-nat' (incr b)) ≡ S n
                                  -- b≡n : 1 + double (bin-to-nat' b) ≡ n
  rewrite
    sym b≡n                       -- double (bin-to-nat' (incr b)) ≡ S (S (double (bin-to-nat' b)))
--  | bin-to-nat-pres-incr b n b≡n
  = {!!}

nat-to-bin : (n : nat) → bin
nat-to-bin n = {!!}

{-

Prove for any nat, convert it to binary; convert it back, get same nat
HINT If def of nat_to_bin uses other functions,
might need to prove lemma showing how functions relate to nat_to_bin.)

Theorem nat_bin_nat : ∀ n, bin_to_nat (nat_to_bin n) = n.
Proof.
  (* FILL IN HERE *) Admitted.

(b) One might naturally expect that we should also prove the opposite direction -- that starting with a binary number, converting to a natural, and then back to binary should yield the same number we started with. However, this is not the case! Explain (in a comment) what the problem is.

(* FILL IN HERE *)

(c) Define a normalization function -- i.e., a function normalize going directly from bin to bin (i.e., not by converting to nat and back) such that, for any binary number b, converting b to a natural and then back to binary yields (normalize b). Prove it. (Warning: This part is a bit tricky -- you may end up defining several auxiliary lemmas. One good way to find out what you need is to start by trying to prove the main statement, see where you get stuck, and see if you can find a lemma -- perhaps requiring its own inductive proof -- that will allow the main proof to make progress.) Don't define this using nat_to_bin and bin_to_nat!

(* FILL IN HERE *)
-}
