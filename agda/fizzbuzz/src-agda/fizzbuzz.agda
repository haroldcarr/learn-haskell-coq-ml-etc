module fizzbuzz where

import Data.Nat        as N
import Data.Nat.DivMod as N
import Data.Nat.Show   as N
import Data.Bool       as B
import Data.Fin        as F
import Data.Unit       as U
import Data.String     as S
open import Data.Product using (_,_ ; _×_)
open import IO
open import Agda.Builtin.Coinduction
open import Relation.Nullary
open import Function

congruent : N.ℕ → N.ℕ → B.Bool
congruent n N.zero    = B.false
congruent n (N.suc m) with N._≟_ 0 $ F.toℕ (N._mod_ n (N.suc m) {U.tt})
... | yes _ = B.true
... | no  _ = B.false

_and_ : {A B : Set} → A → B → A × B
_and_ = _,_

fizzbuzz : N.ℕ → S.String
fizzbuzz N.zero    = "fizzbuzz"
fizzbuzz n with congruent n 3 and congruent n 5
... | B.true  , B.true   = "fizzbuzz"
... | B.true  , B.false  = "fizz"
... | B.false , B.true   = "buzz"
... | B.false , B.false  = N.show n

worker : N.ℕ → IO U.⊤
worker N.zero    = putStrLn $ fizzbuzz N.zero
worker (N.suc n) = ♯ worker n >> ♯ putStrLn (fizzbuzz $ N.suc n)

main = run $ worker 100
