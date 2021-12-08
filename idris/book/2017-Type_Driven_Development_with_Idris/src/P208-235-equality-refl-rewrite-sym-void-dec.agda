module P208-235-equality-refl-rewrite-sym-void-dec where

open import Data.Bool
open import Data.Empty
open import Data.List
open import Data.Maybe
open import Data.Nat
open import Data.String hiding (_==_)
open import Data.Vec
open import Relation.Binary.PropositionalEquality using (_≡_; refl; cong; cong-app; sym; subst)
open import Relation.Nullary
open import Function.Base

Nat = ℕ
_==_ = _≡_

data EqNat : (num1 : Nat) -> (num2 : Nat) -> Set where
  Same : (num : Nat) -> EqNat num num

the : (A : Set) -> A -> A
the _ a = a

-- {-
-- Same 4
-- the (EqNat 3 4) (Same _)
-- -}

sameS : (k : Nat) -> (j : Nat) -> (eq : EqNat k j) -> EqNat (suc k) (suc j)
sameS k k (Same k) = Same (suc k)

checkEqNat : (num1 : Nat) -> (num2 : Nat) -> Maybe (EqNat num1 num2)
checkEqNat  zero    zero   = just (Same zero)
checkEqNat  zero   (suc k) = nothing
checkEqNat (suc k)  zero   = nothing
checkEqNat (suc k) (suc j) =
  case checkEqNat k j of λ where
    nothing   -> nothing
    (just eq) -> just (sameS _ _ eq)

exactLength : {a : Set} {m : Nat} -> (len : Nat) -> (input : Vec a m) -> Maybe (Vec a len)
exactLength {_} {m} len input =
  case checkEqNat m len of λ where
    nothing           -> nothing
    (just (Same len)) -> just input

-- ------------------------------------------------------------------------------
-- -- EQUALITY in general

{-
-- the ("H" == "H")

-- the ("H" == "H") refl

-- the (true == false) refl
-- -}
--                                                    uses equality instead of EqNat
--                                                        v
checkEqNat' : (num1 : Nat) -> (num2 : Nat) -> Maybe (num1 == num2)
checkEqNat'  zero    zero   = just refl
checkEqNat'  zero   (suc k) = nothing
checkEqNat' (suc k)  zero   = nothing
checkEqNat' (suc k) (suc j) =
  case checkEqNat' k j of λ where
    nothing    -> nothing
    (just prf) -> just (cong suc prf) -- uses 'cong' instead of 'sameS'

-- ------------------------------------------------------------------------------
-- -- exercises

same_cons : {a : Set} -> {x : a} -> {xs : List a} -> {ys : List a}
         ->      xs  ==      ys
         -> (x ∷ xs) == (x ∷ ys)
same_cons {_} {x} xsEQys = cong (x Data.List.∷_) xsEQys

same_lists : {a : Set} -> {x y : a} -> {xs : List a} -> {ys : List a}
          ->  x       ==  y
          ->      xs  ==                ys
          -> (x ∷ xs) == (y Data.List.∷ ys)
same_lists refl refl = refl

-- 3 values must be equal
data ThreeEq {X : Set} : X -> X -> X -> Set where
  teq : (x : X) -> ThreeEq x x x

allSameS : (x y z : Nat) -> ThreeEq x y z -> ThreeEq (suc x) (suc y) (suc z)
allSameS x .x .x (teq .x) = teq (suc x)

-- ------------------------------------------------------------------------------
-- -- REWRITE

postulate
  plusCommutative : (m n : ℕ) → m + n ≡ n + m

myReverse : {elem : Set} {n : Nat} -> Vec elem n -> Vec elem n
myReverse                 []  = []
myReverse {_} {suc k} (x ∷ xs)
  with myReverse xs Data.Vec.++ (x ∷ [])
...| result
  rewrite plusCommutative 1 k
  = result

myReverse' : {elem : Set} {n : Nat} -> Vec elem n -> Vec elem n
myReverse'      []  = []
myReverse' (x ∷ xs) = reverseProof (myReverse' xs Data.Vec.++ (x ∷ []))
 where
  reverseProof : {elem : Set} {k : Nat} -> Vec elem (k + 1) -> Vec elem (suc k)
  reverseProof {_} {k} result rewrite plusCommutative 1 k = result

-- ------------------------------------------------------------------------------
-- -- can avoid rewriting in types by taking care in how function types are written

-- -- this version of append as n + m - which lines up with 'plus' definition
append' : {elem : Set} {n m : Nat} -> Vec elem n -> Vec elem m -> Vec elem (n + m)
append'      []  ys = ys
append' (x ∷ xs) ys = x ∷ append' xs ys

-- -----
-- -- SYM

postulate
  plusZeroRightNeutral : (left : Nat) -> (left + 0) == left
  plusSuccRightSucc : (left : Nat) -> (right : Nat) -> (suc (left + right)) == (left + suc right)

-- -- this version of append has m + n, which does not line up, so more work is needed

append_nil : {elem : Set} {m : Nat} -> Vec elem m -> Vec elem (m + 0)
append_nil {_} {m} xs rewrite plusZeroRightNeutral m = xs

append_xs : {elem : Set} {m k : Nat} -> Vec elem (suc (m + k)) -> Vec elem (m + (suc k))
append_xs {_} {m} {k} xs rewrite sym (plusSuccRightSucc m k) = xs -- NOTE: 'sym'

append'' : {elem : Set} {m n : Nat} -> Vec elem n -> Vec elem m -> Vec elem (m + n)
append''       [] ys = append_nil ys
append'' (x ∷ xs) ys = append_xs (x ∷ append'' xs ys)

-- ------------------------------------------------------------------------------
-- -- exercises

myPlusCommutes : (n : Nat) -> (m : Nat) -> (n + m) == (m + n)
myPlusCommutes  zero   m = sym (plusZeroRightNeutral m)
myPlusCommutes (suc n) m
  rewrite
    sym (plusSuccRightSucc m n)
  | myPlusCommutes n m
  = refl

myReverse'' : {a : Set} {n : Nat} -> Vec a n -> Vec a n
myReverse'' xs = reverse' [] xs
 where
  reverseProof_nil : {a : Set} {n : Nat} -> Vec a n -> Vec a (n + 0)
  reverseProof_nil {_} {n} v rewrite plusZeroRightNeutral n = v

  reverseProof_xs : {a : Set} {n len : Nat} -> Vec a (suc n + len) -> Vec a (n + (suc len))
  reverseProof_xs {_} {n} {len} v rewrite sym (plusSuccRightSucc n len) = v

  reverse' : {a : Set} {m n : Nat} -> Vec a n -> Vec a m -> Vec a (n + m)
  reverse' acc      []  = reverseProof_nil acc
  reverse' acc (x ∷ xs) = reverseProof_xs (reverse' (x ∷ acc) xs)

-- ------------------------------------------------------------------------------
-- -- VOID

-- -- Refl states two values are equal.

-- -- to show that two values are not equal must give evidence that x = y CANNOT exist

-- {-
-- -- no constructor so values of this type CANNOT exist
-- data Void : Type where
-- -}

-- the only possible input, refl, can never be valid
twoPlusTwoNotFive : (2 + 2) == 5 -> ⊥
twoPlusTwoNotFive ()

valueNotSuc : (x : Nat) -> x == suc x -> ⊥
valueNotSuc _ ()

-- {-
-- If it was possible to provide a value of the empty type,
-- then it would be possible to produce a value of any type.
-- I.E., given a proof that an impossible value has happened, anything can be proved.

-- ⊥-elim : ∀ {w} {Whatever : Set w} → ⊥ → Whatever
-- ⊥-elim ()

-- The knowledge that something can’t happen, can be used to express
-- limitations about what can happen.
-- Possible to express more precisely what a function is intended to do.
-- -}

-- ------------------------------------------------------------------------------
-- -- DECIDABILITY

-- {-
-- checkEqNat (above) gives certainty when Nats are equal,
-- but no useful info when they are not.

-- DECIDABILITY : A property of some values is decidable if it can be determined
-- whether the property holds or not for specific values.

-- λΠ> the (Dec (2 + 2 == 4)) (yes refl)
-- Yes Refl : Dec (4 = 4)

-- λΠ> the (Dec (2 + 2 = 5)) (No twoPlusTwoNotFive)
-- No twoPlusTwoNotFive : Dec (4 = 5)
-- -}

zeroNotSuc : ∀ {k : Nat} -> (0 == suc k) -> ⊥
zeroNotSuc ()

sucNotZero : ∀ {k : Nat} -> (suc k == 0) -> ⊥
sucNotZero ()

noRec : ∀ {k j : Nat} -> (contra : (k == j) -> ⊥) -> (suc k == suc j) -> ⊥
noRec contra refl = contra refl

checkEqNatDec : (num1 : Nat) -> (num2 : Nat) -> Dec (num1 == num2)
checkEqNatDec  zero    zero   = yes refl
checkEqNatDec  zero   (suc k) = no zeroNotSuc
checkEqNatDec (suc k)  zero   = no sucNotZero
checkEqNatDec (suc k) (suc j) =
  case checkEqNatDec k j of λ where
    (yes prf)    -> yes (cong suc prf)
    (no  contra) -> no  (noRec contra)

-- {-
-- PROVING INPUTS ARE IMPOSSIBLE WITH VOID

-- When running checkEqNatDec,
-- a value of Void is NOT constructed using zeroNotSuc, sucNotZero, or noRec.

-- A function that produces a Void value is a proof that its args cannott all be provided at the
-- same time.

-- noRec says : given a proof that k doesn’t equal j and a proof that S k = S j,
-- then there’s a contradiction---therefore have a value of type Void.

-- The benefit of checkEqNatDec comes to the functions that use it,
-- because they INFO on Yes or No.

-- Idris has "generic" version of checkEqNatDec:

-- interface DecEq ty where
--   decEq : (val1 : ty) -> (val2 : ty) -> Dec (val1 = val2)
-- -}

exactLengthDec : {a : Set} {m : Nat} -> (len : Nat) -> (input : Vec a m) -> Maybe (Vec a len)
exactLengthDec {_} {m} len input =
  case m Data.Nat.≟ len of λ where
    (yes refl)   -> just input
    (no  contra) -> nothing

-- ------------------------------------------------------------------------------
-- -- exercises

open import Level using (Level)

-- -- Data.Vec provides a DecEq implementation, so define custom one
private
  variable
    a : Level
infixr 7 _:>_
data Vect (A : Set a) : ℕ → Set a where
  V0   : Vect A zero
  _:>_ : ∀ {n} (x : A) (xs : Vect A n) → Vect A (suc n)

-- -- if the first elements of two vectors are unequal, then the vectors are unequal
headUnequal
  : {n : Nat} {a : Set} {x y : a} {xs : Vect a n} {ys : Vect a n}
 -> (contra : (x == y) -> ⊥)
 -> ((x :> xs) == (y :> ys))
 -> ⊥
headUnequal cntr refl = cntr refl

-- -- if there are differences in the tails of two vectors, then the vectors are unequal.
tailUnequal
  : {n : Nat} {a : Set} {x y : a} {xs : Vect a n} {ys : Vect a n}
 -> (contra : (xs == ys) -> ⊥)
 -> ((x :> xs) == (y :> ys))
 -> ⊥
tailUnequal cntr refl = cntr refl

-- -- use above to define: TODO: generalize to Set (instead of Nat)
eqVect : {n : Nat} -> (v1 : Vect Nat n) -> (v2 : Vect Nat n) -> Dec (v1 == v2)
eqVect V0 V0 = yes refl
eqVect (x :> xs) (y :> ys) =
  case x Data.Nat.≟ y of λ where
    (yes xEQy)      ->
      case eqVect xs ys of λ where
        (yes xsEQys)       -> yesCase xEQy xsEQys
        (no  xsEQysToVoid) -> no (λ xCONSxsEQyCONSys ->
                                    ⊥-elim (tailUnequal xsEQysToVoid xCONSxsEQyCONSys))
    (no xEQyTOVoid) ->
      case eqVect xs ys of λ where
        (yes xsEQys)       -> no (λ xCONSxsEQyCONSys ->
                                        headUnequal xEQyTOVoid   xCONSxsEQyCONSys)
        (no  xsEQysToVoid) -> no (λ xCONSxsEQyCONSys ->
                                    ⊥-elim (tailUnequal xsEQysToVoid xCONSxsEQyCONSys))
 where
  yesCase : {n : Nat} {xs : Vect Nat n} {ys : Vect Nat n}
         -> x == y -> xs == ys
         -> Dec (x :> xs ≡ y :> ys)
  yesCase xEQy xsEQys rewrite xEQy | xsEQys = yes refl

{-
eqVect (the (Vect _ _) (1 :> 2 :> 3 :> V0)) (1 :> 2 :> 3 :> V0)
yes refl


eqVect (the (Vect _ _) (2 :> 2 :> 3 :> V0)) (1 :> 2 :> 3 :> V0)
no
(headUnequal
 (λ x →
    (λ ())
    (Data.Nat.Properties.≡⇒≡ᵇ 1 0
     (Data.Nat.Properties.suc-injective x))))


eqVect (the (Vect _ _) (1 :> 3 :> 2 :> V0)) (1 :> 2 :> 3 :> V0)
no
(λ xCONSxsEQyCONSys →
   ⊥-elim
   (tailUnequal
    (λ xCONSxsEQyCONSys₁ →
       ⊥-elim
       (tailUnequal
        (headUnequal
         (λ x →
            (λ ())
            (Data.Nat.Properties.≡⇒≡ᵇ 0 1
             (Data.Nat.Properties.suc-injective
              (Data.Nat.Properties.suc-injective x)))))
        xCONSxsEQyCONSys₁))
    xCONSxsEQyCONSys))
-}
