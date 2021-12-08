module Main

import Data.List as L
import Data.Vect as V

{-
Dependent types like EqNat and ==, are used for describing relationships between data
- these types are often referred to as predicates
  data types that exist to describe a property of some data.
- If a value can be constructed for a predicate, it guarantees the property.
-}

------------------------------------------------------------------------------
-- 'Elem' : guaranteeing a value is in a vector
{-
-- a correct implementation of this is impossible if the element is not in the vector
removeElem : (value : a) -> (xs : Vect (S n) a) -> Vect n a

data Elem : a -> Vect k a -> Type where
  Here  : Elem x (x :: xs)
  There : (later : Elem x xs) -> Elem x (y :: xs)
-}

{-
Uses 'absurd'
interface Uninhabited t where
  uninhabited : t -> Void

absurd : Uninhabited t => (h : t) -> a
absurd h = void (uninhabited h)
-}

removeElem : (value : a) -> (xs : Vect (S n) a) -> (prf : Elem value xs) -> Vect n a
removeElem             value (value :: ys)  Here         = ys
removeElem {n =  Z}    value (    y :: []) (There later) = absurd later -- later : Elem value []
removeElem {n = (S k)} value (    y :: ys) (There later) = y :: removeElem value ys later

exV : Vect 3 Int
exV = [1,2,3]

twoInVector : Elem 2 Main.exV
twoInVector = There Here

rmTwo : Vect 2 Int
rmTwo = removeElem 2 Main.exV twoInVector

-- auto-implicit args - automatically constructs proofs

removeElemAuto : (value : a) -> (xs : Vect (S n) a) -> {auto prf : Elem value xs} -> Vect n a
removeElemAuto             value  (value :: ys) {prf = Here       } = ys
removeElemAuto {n =  Z}    value  (    y :: []) {prf = There later} = absurd later
removeElemAuto {n = (S k)} value  (    y :: ys) {prf = There later} = y :: removeElemAuto value ys

rmTwo' : Vect 2 Int
rmTwo' = removeElemAuto 2 Main.exV

------------------------------------------------------------------------------
-- decidable predicates
-- a property is decidable if it can be said that the holds or NOT for some specific value(s)

notInNil : V.Elem value [] -> Void
notInNil  Here     impossible
notInNil (There _) impossible

notInTail : (notThere : V.Elem value xs -> Void)
         -> (notHere  : (value = x)     -> Void)
         -> Elem value (x :: xs)
         -> Void
notInTail notThere notHere  Here         = notHere  Refl
notInTail notThere notHere (There later) = notThere later

isElem : DecEq a => (value : a) -> (xs : Vect n a) -> Dec (Elem value xs)
isElem value       [] = No notInNil
isElem value (x :: xs) =
  case decEq value x of
    Yes Refl   => Yes Here
    No notHere =>
      case Main.isElem value xs of
        Yes prf     => Yes (There prf)
        No notThere => No (notInTail notThere notHere)

elem : Eq ty => (value : ty) -> (xs : Vect n ty) -> Bool
elem value       []  = False
elem value (x :: xs) =
  case value == x of
    False => Main.elem value xs
    True  => True

exIsElem : Dec (V.Elem 3 [1,2,3])
exIsElem = Main.isElem 3 [1,2,3]

------------------------------------------------------------------------------
-- exercises

data ElemL : a -> List a -> Type where
  Here  : ElemL x (x :: xs)
  There : (later : ElemL x xs) -> ElemL x (y :: xs)

notInNilL : ElemL value [] -> Void
notInNilL  Here     impossible
notInNilL (There _) impossible

notInTailL : (notThere : ElemL value xs -> Void) -> (notHere : (value = x) -> Void)
          -> ElemL value (x :: xs) -> Void
notInTailL notThere notHere  Here         = notHere  Refl
notInTailL notThere notHere (There later) = notThere later

isElemL : DecEq a => (value : a) -> (xs : List a) -> Dec (ElemL value xs)
isElemL value       []  = No notInNilL
isElemL value (x :: xs) =
  case decEq value x of
    Yes Refl   => Yes Here
    No notHere =>
      case isElemL value xs of
        Yes prf     => Yes (There prf)
        No notThere => No (notInTailL notThere notHere)

elemL : Eq ty => (value : ty) -> (xs : List ty) -> Bool
elemL value       []  = False
elemL value (x :: xs) =
  case value == x of
    False => elemL value xs
    True  => True

exIsElemL : Dec (ElemL 3 [1,2,3])
exIsElemL = isElemL 3 [1,2,3]

-----

data Last : List a -> a -> Type where
  LastOne  : Last [value] value
  LastCons : (prf : Last xs value) -> Last (x :: xs) value

exLast123 : Last [1,2,3] 3
exLast123 = LastCons (LastCons LastOne)

notLastInNil : Last [] value -> Void
notLastInNil  LastOne     impossible
notLastInNil (LastCons _) impossible

lastOneImpliesEq : Last [x] a -> x = a
lastOneImpliesEq  LastOne       = Refl
lastOneImpliesEq (LastCons prf) impossible

tailStillLast : Last (x :: y :: xs) a -> Last (y :: xs) a
tailStillLast (LastCons yxs) = yxs

isLast : DecEq a => (xs : List a) -> (value : a) -> Dec (Last xs value)
isLast            []  a = No notLastInNil
isLast      (x :: []) a =
  case decEq x a of
    No  xEQaToVoid => No (\LastPxPa => xEQaToVoid (lastOneImpliesEq LastPxPa))
    Yes xEQa       => doYes xEQa
 where
  doYes : (x = a) -> Dec (Last [x] a)
  doYes prf = rewrite prf in (Yes LastOne)
isLast (x :: y :: xs) a =
  case isLast (y :: xs) a of
    No  lastYXSaToVoid => No  (\LastXYXSa => void (lastYXSaToVoid (tailStillLast LastXYXSa)))
    Yes why            => Yes (LastCons why)

exIsLast : Dec (Last [1, 2, 3] 3)
exIsLast = isLast [1,2,3] 3

------------------------------------------------------------------------------
-- hangman

data WordState : (guesses_remaining : Nat) -> (letters : Nat) -> Type where
  MkWordState : (word : String) -> (missing : Vect letters Char)
             -> WordState guesses_remaining letters

data Finished : Type where
  Lost : (game : WordState 0 (S letters)) -> Finished
  Won  : (game : WordState (S guesses) 0) -> Finished

data ValidInput : List Char -> Type where
  Letter : (c : Char) -> ValidInput [c]

isValidNil : ValidInput [] -> Void
isValidNil (Letter _) impossible

isValidTwo : ValidInput (x :: y :: xs) -> Void
isValidTwo (Letter _) impossible

isValidInput : (cs : List Char) -> Dec (ValidInput cs)
isValidInput             []   = No  isValidNil
isValidInput (x       :: [])  = Yes (Letter x)
isValidInput (x :: (y :: xs)) = No  isValidTwo

isValidString : (s : String) -> Dec (ValidInput (unpack s))
isValidString s = isValidInput (unpack s)

readGuess : IO (x ** ValidInput x)
readGuess = do
  putStr "Guess:"
  x <- getLine
  case isValidString (toUpper x) of
    Yes prf    => pure (_ ** prf)
    No  contra => do putStrLn "Invalid guess"
                     readGuess
{-
:exec readGuess >>= \_ => putStrLn "YES"
-}

processGuess : (letter : Char)
            ->         WordState (S guesses) (S letters)
            -> Either (WordState    guesses  (S letters))
                      (WordState (S guesses)    letters)
processGuess letter (MkWordState word missing) =
  case V.isElem letter missing of
    Yes prf    => Right (MkWordState word (removeElemAuto letter missing))
    No  contra => Left  (MkWordState word missing)

game : WordState (S guesses) (S letters) -> IO Finished
game {guesses} {letters} st = do
   (_ ** Letter letter) <- readGuess
   case processGuess letter st of
     Left l => do
       putStrLn "Wrong!"
       case guesses of
         Z   => pure (Lost l)
         S k => game l
     Right r => do
       putStrLn "Right!"
       case letters of
         Z   => pure (Won r)
         S k => game r

main : IO ()
main = do
  result <- game {guesses=2} (MkWordState "Test" ['T', 'E', 'S'])
  case result of
    Lost (MkWordState word missing) => putStrLn ("You lose. The word was " ++ word)
    Won game                        => putStrLn "You win!"

{-
:exec main
-}
