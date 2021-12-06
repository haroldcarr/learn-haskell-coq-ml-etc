module Main

import Data.Vect

{-
(==) : Eq ty => ty -> ty -> Bool

The above type gives NO information about the relationships between the inputs and the output.
Need to look at implementations to be sure.

Want to be more expressive when comparing values at the type level.

Like the following from P139_read_vect.idr
exactLength : (len : Nat) -> (input : Vect m a) -> Maybe (Vect len a)
-}

data EqNat : (num1 : Nat) -> (num2 : Nat) -> Type where
  Same : (num : Nat) -> EqNat num num

{-
Same 4
the (EqNat 3 4) (Same _)
-}

sameS : (k : Nat) -> (j : Nat) -> (eq : EqNat k j) -> EqNat (S k) (S j)
sameS k k (Same k) = Same (S k)

checkEqNat : (num1 : Nat) -> (num2 : Nat) -> Maybe (EqNat num1 num2)
checkEqNat  Z     Z    = Just (Same Z)
checkEqNat  Z    (S k) = Nothing
checkEqNat (S k)  Z    = Nothing
checkEqNat (S k) (S j) =
  case checkEqNat k j of
    Nothing => Nothing
    Just eq => Just (sameS _ _ eq)

exactLength : (len : Nat) -> (input : Vect m a) -> Maybe (Vect len a)
exactLength {m} len input =
  case checkEqNat m len of
    Nothing         => Nothing
    Just (Same len) => Just input

------------------------------------------------------------------------------
-- EQUALITY in general
{-
data (=) : a -> b -> Type where
  Refl : x = x
-}
{-
λΠ> the
the : (a : Type) -> a -> a

λΠ> the ("H" == "H")
id : "H" == "H" -> "H" == "H"

λΠ> the ("H" == "H") Refl
Refl : "H" == "H"

λΠ> the (True == False) Refl
... error
-}
--                                                    uses equality instead of EqNat
--                                                        v
checkEqNat' : (num1 : Nat) -> (num2 : Nat) -> Maybe (num1 = num2)
checkEqNat'  Z     Z    = Just Refl
checkEqNat'  Z    (S k) = Nothing
checkEqNat' (S k)  Z    = Nothing
checkEqNat' (S k) (S j) =
  case checkEqNat' k j of
    Nothing  => Nothing
    Just prf => Just (cong prf) -- uses 'cong' instead of 'sameS'

------------------------------------------------------------------------------
-- exercises

same_cons : {xs : List a} -> {ys : List a}
         ->      xs =      ys
         -> x :: xs = x :: ys
same_cons xsEQys = cong xsEQys

same_lists : {xs : List a} -> {ys : List a}
          -> x  = y
          -> xs = ys
          -> x :: xs = y :: ys
same_lists Refl Refl = Refl

-- 3 values must be equal (i.e., transitivity of equality)
ThreeEq : a -> b -> c -> Type
ThreeEq a b c =
  let ab = a = b
      bc = b = c
      ac = ab = bc
   in ac
{-
allSameS : (x, y, z : Nat) -> ThreeEq x y z -> ThreeEq (S x) (S y) (S z)
allSameS  Z     Z     Z    teq = ?sss
{-
allSameS  Z     Z    (S _) Refl impossible
allSameS  Z    (S _)  Z    Refl impossible
allSameS  Z    (S _) (S _) Refl impossible
allSameS (S _)  Z     Z    Refl impossible
allSameS (S _)  Z    (S _) Refl impossible
allSameS (S _) (S _)  Z    Refl impossible
-}
allSameS (S x) (S y) (S z) teq = 
TODO
- + Errors (1)
 `-- builtin line 0 col -1:
     Universe inconsistency.
-}

------------------------------------------------------------------------------
-- REWRITE

myReverse : Vect n elem -> Vect n elem
myReverse [] = []
myReverse {n = S k} (x :: xs) =
  let result = myReverse xs ++ [x]
   in rewrite plusCommutative 1 k
      in result

myReverse' : Vect n elem -> Vect n elem
myReverse' [] = []
myReverse' (x :: xs) = reverseProof (myReverse' xs ++ [x])
 where
  reverseProof : Vect (k + 1) elem -> Vect (S k) elem
  reverseProof {k} result = rewrite plusCommutative 1 k in result

------------------------------------------------------------------------------
-- can avoid rewriting in types by taking care in how function types are written

-- this version of append as n + m - which lines up with 'plus' definition
append' : Vect n elem -> Vect m elem -> Vect (n + m) elem
append'       []  ys = ys
append' (x :: xs) ys = x :: append' xs ys

-----
-- SYM

-- this version of append has m + n, which does not line up, so more work is needed
append_nil : Vect m elem -> Vect (plus m 0) elem
append_nil {m} xs = rewrite plusZeroRightNeutral m in xs

append_xs : Vect (S (m + k)) elem -> Vect (plus m (S k)) elem
append_xs {m} {k} xs = rewrite sym (plusSuccRightSucc m k) in xs -- NOTE: 'sym'

append'' : Vect n elem -> Vect m elem -> Vect (m + n) elem
append''       []  ys = append_nil ys
append'' (x :: xs) ys = append_xs (x :: append'' xs ys)

------------------------------------------------------------------------------
-- exercises

myPlusCommutes : (n : Nat) -> (m : Nat) -> n + m = m + n
myPlusCommutes  Z     m = sym (plusZeroRightNeutral m)
myPlusCommutes (S n) m =
  rewrite sym (plusSuccRightSucc m n) in
  rewrite myPlusCommutes n m
  in Refl

myReverse'' : Vect n a -> Vect n a
myReverse'' xs = reverse' [] xs
 where
  reverseProof_nil : Vect n a -> Vect (plus n 0) a
  reverseProof_nil {n} v = rewrite plusZeroRightNeutral n in v

  reverseProof_xs : Vect (S n + len) a -> Vect (plus n (S len)) a
  reverseProof_xs {n} {len} v = rewrite sym (plusSuccRightSucc n len) in v

  reverse' : Vect n a -> Vect m a -> Vect (n + m) a
  reverse' acc       []  = reverseProof_nil acc
  reverse' acc (x :: xs) = reverseProof_xs (reverse' (x::acc) xs)

------------------------------------------------------------------------------
-- VOID

-- Refl states two values are equal.

-- to show that two values are not equal must give evidence that x = y CANNOT exist

{-
-- no constructor so values of this type CANNOT exist
data Void : Type where
-}

-- the only possible input, Refl, can never be valid
twoPlusTwoNotFive : 2 + 2 = 5 -> Void
twoPlusTwoNotFive Refl impossible

valueNotSuc : (x : Nat) -> x = S x -> Void
valueNotSuc _ Refl impossible

{-
If it was possible to provide a value of the empty type,
then it would be possible to produce a value of any type.
I.E., given a proof that an impossible value has happened, anything can be proved.

void : Void -> a

The knowledge that something can’t happen, can be used to express
limitations about what can happen.
Possible to express more precisely what a function is intended to do.
-}

------------------------------------------------------------------------------
-- DECIDABILITY

{-
checkEqNat (above) gives certainty when Nats are equal,
but no useful info when they are not.

DECIDABILITY : A property of some values is decidable if it can be determined
whether the property holds or not for specific values.

data Dec : (prop : Type) -> Type where
  Yes : (prf    : prop)         -> Dec prop
  No  : (contra : prop -> Void) -> Dec prop

λΠ> the (Dec (2 + 2 = 4)) (Yes Refl)
Yes Refl : Dec (4 = 4)

λΠ> the (Dec (2 + 2 = 5)) (No twoPlusTwoNotFive)
No twoPlusTwoNotFive : Dec (4 = 5)
-}

zeroNotSuc : (0 = S k) -> Void
zeroNotSuc Refl impossible

sucNotZero : (S k = 0) -> Void
sucNotZero Refl impossible

noRec : (contra : (k = j) -> Void) -> (S k = S j) -> Void
noRec contra Refl = contra Refl

checkEqNatDec : (num1 : Nat) -> (num2 : Nat) -> Dec (num1 = num2)
checkEqNatDec  Z     Z    = Yes Refl
checkEqNatDec  Z    (S k) = No zeroNotSuc
checkEqNatDec (S k)  Z    = No sucNotZero
checkEqNatDec (S k) (S j) =
  case checkEqNatDec k j of
    Yes prf    => Yes (cong  prf)
    No  contra => No  (noRec contra)

{-
PROVING INPUTS ARE IMPOSSIBLE WITH VOID

When running checkEqNatDec,
a value of Void is NOT constructed using zeroNotSuc, sucNotZero, or noRec.

A function that produces a Void value is a proof that its args cannott all be provided at the
same time.

noRec says : given a proof that k doesn’t equal j and a proof that S k = S j,
then there’s a contradiction---therefore have a value of type Void.

The benefit of checkEqNatDec comes to the functions that use it,
because they INFO on Yes or No.

Idris has "generic" version of checkEqNatDec:

interface DecEq ty where
  decEq : (val1 : ty) -> (val2 : ty) -> Dec (val1 = val2)
-}

exactLengthDec : (len : Nat) -> (input : Vect m a) -> Maybe (Vect len a)
exactLengthDec {m} len input =
  case decEq m len of
    Yes Refl   => Just input
    No  contra => Nothing

------------------------------------------------------------------------------
-- exercises

-- Data.Vect provides a DecEq implementation, so define custom one
infixr 7 :>
data Vec : Nat -> Type -> Type where
  V0   :                              Vec  Z    a
  (:>) : (x : a) -> (xs : Vec k a) -> Vec (S k) a

-- if the first elements of two vectors are unequal, then the vectors are unequal
headUnequal
  : DecEq a
 => {xs : Vec n a} -> {ys : Vec n a}
 -> (contra : (x = y) -> Void)
 -> ((x :> xs) = (y :> ys))
 -> Void
headUnequal cntr Refl = cntr Refl

-- if there are differences in the tails of two vectors, then the vectors are unequal.
tailUnequal
  : DecEq a
 => {xs : Vec n a} -> {ys : Vec n a}
 -> (contra : (xs = ys) -> Void)
 -> ((x :> xs) = (y :> ys))
 -> Void
tailUnequal cntr Refl = cntr Refl

{-
:total headUnequal
:total tailUnequal
-}

-- use above to define:
DecEq a => DecEq (Vec n a) where
  decEq       V0        V0  = Yes Refl
  decEq (x :> xs) (y :> ys) =
    case decEq x y of
      Yes xEQy       =>
        case decEq xs ys of
          Yes xsEQys       => rewrite xEQy in rewrite xsEQys in Yes Refl
          No  xsEQysToVoid => No (\xCONSxsEQyCONSys =>
                                    void (tailUnequal xsEQysToVoid xCONSxsEQyCONSys))
      No  xEQyTOVoid =>
        case decEq xs ys of
          Yes xsEQys       => No (\xCONSxsEQyCONSys =>
                                          headUnequal xEQyTOVoid   xCONSxsEQyCONSys)
          No  xsEQysToVoid => No (\xCONSxsEQyCONSys =>
                                    void (tailUnequal xsEQysToVoid xCONSxsEQyCONSys))

{-
λΠ> decEq (the (Vec _ _) (1 :> 2 :> 3 :> V0)) (1 :> 2 :> 3 :> V0)
Yes Refl : Dec (1 :> 2 :> 3 :> V0 = 1 :> 2 :> 3 :> V0)


λΠ> decEq (the (Vec _ _) (2 :> 2 :> 3 :> V0)) (1 :> 2 :> 3 :> V0)
No (\xCONSxsEQyCONSys8 =>
      headUnequal (Decidable.Equality.Decidable.Equality.Integer implementation of Decidable.Equality.DecEq, method decEq, primitiveNotEq 2
                                                                                                                                          1)
                  xCONSxsEQyCONSys8) : Dec (2 :> 2 :> 3 :> V0 = 1 :> 2 :> 3 :> V0)


λΠ> decEq (the (Vec _ _) (1 :> 3 :> 2 :> V0)) (1 :> 2 :> 3 :> V0)
No (\xCONSxsEQyCONSys =>
      void (tailUnequal (\xCONSxsEQyCONSys9 =>
                           void (tailUnequal (\xCONSxsEQyCONSys8 =>
                                                headUnequal (Decidable.Equality.Decidable.Equality.Integer implementation of Decidable.Equality.DecEq, method decEq, primitiveNotEq 2
                                                                                                                                                                                    3)
                                                            xCONSxsEQyCONSys8)
                                             xCONSxsEQyCONSys9))
                        xCONSxsEQyCONSys)) : Dec (1 :> 3 :> 2 :> V0 = 1 :> 2 :> 3 :> V0)
-}
