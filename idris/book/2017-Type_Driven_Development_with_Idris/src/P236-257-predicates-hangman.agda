module P236-257-predicates-hangman where

open import Agda.Builtin.Equality
open import Agda.Builtin.Unit using (⊤)
open import Data.Bool
open import Data.Empty
open import Data.Nat renaming (_≟_ to _≟N_)
open import Data.List as L
open import Data.Vec as V
open import Data.Vec.Membership.Propositional
open import Data.Vec.Relation.Unary.Any
open import Relation.Binary.PropositionalEquality using (_≡_; refl; cong; cong-app; sym; subst)
open import Relation.Nullary

Nat = ℕ
_==_ = _≡_

-- ------------------------------------------------------------------------------
-- -- 'Elem' : guaranteeing a value is in a vector

removeElem : {a : Set} {n : Nat}
          -> (value : a) -> (xs : Vec a (suc n)) -> (prf : value ∈ xs)
          -> Vec a n
removeElem             value (_ ∷ ys) (here  _)     = ys
removeElem {_} {suc k} value (y ∷ ys) (there later) = y ∷ removeElem value ys later

exV : Vec Nat 3
exV = 1 ∷ 2 ∷ 3 ∷ []

twoInVector : 2 ∈ exV
twoInVector = there (here refl)

rmTwo : Vec Nat 2
rmTwo = removeElem 2 exV twoInVector

-- -- auto-implicit args - automatically constructs proofs -- TODO : NOT IN AGDA

removeElemAuto : {a : Set} {n : Nat}
              -> (value : a) -> (xs : Vec a (suc n)) -> {prf : value ∈ xs}
              -> Vec a n
removeElemAuto             value  (_ ∷ ys) {here      _} = ys
removeElemAuto {_} {suc k} value  (y ∷ ys) {there later} = y ∷ removeElemAuto value ys {later}

rmTwo' : Vec Nat 2
rmTwo' = removeElemAuto 2 exV {twoInVector}

-- ------------------------------------------------------------------------------
-- -- decidable predicates

notInNil : {A : Set} -> (value : A ∈ []) -> ⊥
notInNil ()

notInTail : {A : Set} {n : Nat} {x value : A} {xs : Vec A n}
         -> (notThere : value ∈ xs   -> ⊥)
         -> (notHere  : (value == x) -> ⊥)
         -> value ∈ (x ∷ xs)
         -> ⊥
notInTail notThere notHere (here  v==x)  = notHere  v==x
notInTail notThere notHere (there later) = notThere later

-- TODO       v - make it a Set (Generalize)
isElem : {n : Nat} (value : Nat) -> (xs : Vec Nat n) -> Dec (value ∈ xs)
isElem value      [] = no λ ()
isElem value (x ∷ xs)
  with value ≟N x
...| yes refl = yes (here refl)
...| no  notHere
  with isElem value xs
...| yes prf      = yes (there prf)
...| no  notThere = no  (notInTail notThere notHere)

elem : {n : Nat} -> Nat -> (xs : Vec Nat n) -> Bool
elem value      []  = false
elem value (x ∷ xs)
  with value ≟N x
...| no  _ = elem value xs
...| yes _ = true

exIsElem : Dec    (3 ∈ 1 ∷ 2 ∷ 3 ∷ [])
exIsElem = isElem 3   (1 ∷ 2 ∷ 3 ∷ [])

-- ------------------------------------------------------------------------------
-- -- exercises

-- data ElemL : a -> List a -> Type where
--   Here  : ElemL x (x :: xs)
--   There : (later : ElemL x xs) -> ElemL x (y :: xs)

-- notInNilL : ElemL value [] -> Void
-- notInNilL  Here     impossible
-- notInNilL (There _) impossible

-- notInTailL : (notThere : ElemL value xs -> Void) -> (notHere : (value = x) -> Void)
--           -> ElemL value (x :: xs) -> Void
-- notInTailL notThere notHere  Here         = notHere  Refl
-- notInTailL notThere notHere (There later) = notThere later

-- isElemL : DecEq a => (value : a) -> (xs : List a) -> Dec (ElemL value xs)
-- isElemL value       []  = No notInNilL
-- isElemL value (x :: xs) =
--   case decEq value x of
--     Yes Refl   => Yes Here
--     No notHere =>
--       case isElemL value xs of
--         Yes prf     => Yes (There prf)
--         No notThere => No (notInTailL notThere notHere)

-- elemL : Eq ty => (value : ty) -> (xs : List ty) -> Bool
-- elemL value       []  = False
-- elemL value (x :: xs) =
--   case value == x of
--     False => elemL value xs
--     True  => True

-- exIsElemL : Dec (ElemL 3 [1,2,3])
-- exIsElemL = isElemL 3 [1,2,3]

-- -----

-- data Last : List a -> a -> Type where
--   LastOne  : Last [value] value
--   LastCons : (prf : Last xs value) -> Last (x :: xs) value

-- exLast123 : Last [1,2,3] 3
-- exLast123 = LastCons (LastCons LastOne)

-- notLastInNil : Last [] value -> Void
-- notLastInNil  LastOne     impossible
-- notLastInNil (LastCons _) impossible

-- lastOneImpliesEq : Last [x] a -> x = a
-- lastOneImpliesEq  LastOne       = Refl
-- lastOneImpliesEq (LastCons prf) impossible

-- tailStillLast : Last (x :: y :: xs) a -> Last (y :: xs) a
-- tailStillLast (LastCons yxs) = yxs

-- isLast : DecEq a => (xs : List a) -> (value : a) -> Dec (Last xs value)
-- isLast            []  a = No notLastInNil
-- isLast      (x :: []) a =
--   case decEq x a of
--     No  xEQaToVoid => No (\LastPxPa => xEQaToVoid (lastOneImpliesEq LastPxPa))
--     Yes xEQa       => doYes xEQa
--  where
--   doYes : (x = a) -> Dec (Last [x] a)
--   doYes prf = rewrite prf in (Yes LastOne)
-- isLast (x :: y :: xs) a =
--   case isLast (y :: xs) a of
--     No  lastYXSaToVoid => No  (\LastXYXSa => void (lastYXSaToVoid (tailStillLast LastXYXSa)))
--     Yes why            => Yes (LastCons why)

-- exIsLast : Dec (Last [1, 2, 3] 3)
-- exIsLast = isLast [1,2,3] 3

-- ------------------------------------------------------------------------------
-- -- hangman

-- data WordState : (guesses_remaining : Nat) -> (letters : Nat) -> Type where
--   MkWordState : (word : String) -> (missing : Vect letters Char)
--              -> WordState guesses_remaining letters

-- data Finished : Type where
--   Lost : (game : WordState 0 (S letters)) -> Finished
--   Won  : (game : WordState (S guesses) 0) -> Finished

-- data ValidInput : List Char -> Type where
--   Letter : (c : Char) -> ValidInput [c]

-- isValidNil : ValidInput [] -> Void
-- isValidNil (Letter _) impossible

-- isValidTwo : ValidInput (x :: y :: xs) -> Void
-- isValidTwo (Letter _) impossible

-- isValidInput : (cs : List Char) -> Dec (ValidInput cs)
-- isValidInput             []   = No  isValidNil
-- isValidInput (x       :: [])  = Yes (Letter x)
-- isValidInput (x :: (y :: xs)) = No  isValidTwo

-- isValidString : (s : String) -> Dec (ValidInput (unpack s))
-- isValidString s = isValidInput (unpack s)

-- readGuess : IO (x ** ValidInput x)
-- readGuess = do
--   putStr "Guess:"
--   x <- getLine
--   case isValidString (toUpper x) of
--     Yes prf    => pure (_ ** prf)
--     No  contra => do putStrLn "Invalid guess"
--                      readGuess
-- {-
-- :exec readGuess >>= \_ => putStrLn "YES"
-- -}

-- processGuess : (letter : Char)
--             ->         WordState (S guesses) (S letters)
--             -> Either (WordState    guesses  (S letters))
--                       (WordState (S guesses)    letters)
-- processGuess letter (MkWordState word missing) =
--   case V.isElem letter missing of
--     Yes prf    => Right (MkWordState word (removeElemAuto letter missing))
--     No  contra => Left  (MkWordState word missing)

-- game : WordState (S guesses) (S letters) -> IO Finished
-- game {guesses} {letters} st = do
--    (_ ** Letter letter) <- readGuess
--    case processGuess letter st of
--      Left l => do
--        putStrLn "Wrong!"
--        case guesses of
--          Z   => pure (Lost l)
--          S k => game l
--      Right r => do
--        putStrLn "Right!"
--        case letters of
--          Z   => pure (Won r)
--          S k => game r

-- main : IO ()
-- main = do
--   result <- game {guesses=2} (MkWordState "Test" ['T', 'E', 'S'])
--   case result of
--     Lost (MkWordState word missing) => putStrLn ("You lose. The word was " ++ word)
--     Won game                        => putStrLn "You win!"

-- {-
-- :exec main
-- -}
