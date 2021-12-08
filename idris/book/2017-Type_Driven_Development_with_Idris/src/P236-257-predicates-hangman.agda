module P236-257-predicates-hangman where

open import Agda.Builtin.Equality
open import Agda.Builtin.Unit                     using (⊤)
open import Data.Bool
open import Data.Char                             as C hiding (_==_) renaming (_≟_ to _≟C_)
open import Data.Empty
open import Data.Nat                              renaming (_≟_ to _≟N_)
open import Data.List                             as L
open import Data.Product
open import Data.Vec                              as V hiding (_>>=_)
open import Data.String                           as S hiding (_==_)
open import Data.Sum
open import Data.Vec.Membership.Propositional
open import Data.Vec.Relation.Unary.Any
open import Function.Base
open import hcio                                  as HCIO
open import IO                                    as IO hiding (_>>=_; _>>_)
import      IO.Primitive                          as Primitive
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

data Last {A : Set} : (List A) -> A -> Set where
  LastOne  : {a   : A}                                    -> Last (a ∷ []) a
  LastCons : {a x : A} {xs : List A} -> (prf : Last xs a) -> Last (x ∷ xs) a

exLast123 : Last (1 ∷ 2 ∷ 3 ∷ []) 3
exLast123 = LastCons (LastCons LastOne)

exLastTTTF : Last (true ∷ true ∷ true ∷ false ∷ []) false
exLastTTTF = LastCons (LastCons (LastCons LastOne))

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

data WordState : (guesses_remaining : Nat) -> (letters : Nat) -> Set where
  MkWordState  : {guesses_remaining letters : Nat}
              -> (word    : String)
              -> (missing : Vec Char letters)
              -> WordState guesses_remaining letters

data Finished : Set where
  Lost : {letters : Nat} (game : WordState 0 (suc letters)) -> Finished
  Won  : {guesses : Nat} (game : WordState (suc guesses) 0) -> Finished

data ValidInput : List Char -> Set where
  Letter : (c : Char) -> ValidInput (c ∷ [])

isValidNil : ValidInput [] -> ⊥
isValidNil ()

isValidTwo : {x y : Char} {xs : List Char} -> ValidInput (x ∷ y ∷ xs) -> ⊥
isValidTwo ()

isValidInput : (cs : List Char) -> Dec (ValidInput cs)
isValidInput           []   = no  isValidNil
isValidInput (x      ∷ [])  = yes (Letter x)
isValidInput (x ∷ (y ∷ xs)) = no  isValidTwo

isValidString : (s : String) -> Dec (ValidInput (S.toList s))
isValidString s = isValidInput (S.toList s)

{-# TERMINATING #-}
readGuess : IO (Σ (List Char) (λ lc -> ValidInput lc))
readGuess = do
  putStr "Guess:"
  x <- HCIO.getLine
  case isValidString (S.fromList (L.map C.toUpper (S.toList x))) of λ where
    (yes prf)    -> return (_ , prf)
    (no  contra) -> do putStrLn "Invalid guess"
                       readGuess

Either = _⊎_

isElemV : {n : Nat} (value : Char) -> (xs : Vec Char n) -> Dec (value ∈ xs)
isElemV value      [] = no λ ()
isElemV value (x ∷ xs)
  with value ≟C x
...| yes refl = yes (here refl)
...| no  notHere
  with isElemV value xs
...| yes prf      = yes (there prf)
...| no  notThere = no  (notInTail notThere notHere)

processGuess : {guesses letters : Nat}
            -> (letter : Char)
            ->         WordState (suc guesses) (suc letters)
            -> Either (WordState      guesses  (suc letters))
                      (WordState (suc guesses)      letters)
processGuess letter (MkWordState word missing) =
  case isElemV letter missing of λ where
    (yes prf)    -> inj₂ (MkWordState word (removeElem letter missing prf))
    (no  contra) -> inj₁ (MkWordState word missing)

mutual
  game : {guesses letters : Nat} -> WordState (suc guesses) (suc letters) -> IO Finished
  game st = do
    (_ , Letter letter) <- readGuess
    let pg = processGuess letter st
    game' pg

  game' : {guesses letters : Nat}
       -> Either (WordState      guesses  (suc letters))
                 (WordState (suc guesses)      letters)
       -> IO Finished
  game' {zero}       {_} (inj₁ l@(MkWordState {.0}             {_} word missing)) = return (Lost l)
  game' {suc guesses}{_} (inj₁ l@(MkWordState {.(suc guesses)} {_} word missing)) = game l
  game' {_}{zero}        (inj₂ r@(MkWordState {_} {.0}             word missing)) = return (Won r)
  game' {_}{suc letters} (inj₂ r@(MkWordState {_} {.(suc letters)} word missing)) = game r

main : Primitive.IO ⊤
main = run do
  result <- game {guesses = 2} (MkWordState "Test" ('T' ∷ 'E' ∷ 'S' ∷ []))
  case result of λ where
    (Lost (MkWordState word missing)) -> putStrLn ("You lose.") -- The word was " V.++ word)
    (Won game)                        -> putStrLn "You win!"
