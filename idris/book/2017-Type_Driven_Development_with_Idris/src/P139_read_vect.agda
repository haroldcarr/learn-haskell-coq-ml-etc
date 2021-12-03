module P139_read_vect where

open import Agda.Builtin.Unit using (⊤)

import      Relation.Nullary
import      Relation.Binary.PropositionalEquality as Eq
open        Eq                using (_≡_; refl; cong; cong-app)

open import Data.Bool
open import Eq-Ord
open import Data.Fin
open import Data.Maybe        hiding (_>>=_)
open import Data.Nat
open import Data.Product
open import Data.String
open import Data.Vec          hiding (_>>=_)
open import hcio              as HCIO
open import IO                as IO hiding (_>>=_; _>>_)
import      IO.Primitive      as Primitive
open import Function.Base
open import Relation.Nullary

Nat = ℕ


------------------------------------------------------------------------------
-- read a vector of known length

readVecLen : (len : Nat) -> IO.IO (Vec String len)
readVecLen  zero   = return []
readVecLen (suc k) = do
  x  <- HCIO.getLine
  xs <- readVecLen k
  return (x ∷ xs)

printVecString : {len : Nat} -> Vec String len -> String
printVecString      [] = "[]"
printVecString (x ∷ xs) = x Data.String.++ " ∷ " Data.String.++ printVecString xs

-- ------------------------------------------------------------------------------
-- -- read a vector of unknown length

data VecUnknown (a : Set) : Set where
   MkVect : (len : Nat) -> Vec a len -> VecUnknown a

{-# TERMINATING #-}
readVec : IO (VecUnknown String)
readVec = do
  x <- getLine
  if (x == "")
    then return (MkVect _ [])
    else do
      MkVect _ xs <- readVec
      return (MkVect _ (x ∷ xs))

printVecU : {a : Set} -> VecUnknown a -> String
printVecU (MkVect     .0       [])  = "[]"
printVecU (MkVect (suc i) (x ∷ xs)) = "x ∷ " Data.String.++ printVecU (MkVect i xs)

-- ------------------------------------------------------------------------------
-- dependent pairs : general solution to above
-- when needing unknown input to define a dependent type

-- type of 2nd element computed from value of 1st
anyVect : Σ Nat (λ n -> Vec String n)
anyVect = (3 , "Rod" ∷ "Jane" ∷ "Freddy" ∷ [])

sndAnyVect : Vec String 3
sndAnyVect = proj₂ anyVect

-- ------------------------------------------------------------------------------

{-# TERMINATING #-}
readVec' : IO (Σ Nat (λ n -> Vec String n))
readVec' = do
  x <- getLine
  if (x == "")
    then return (_ , [])
    else do
      (_ , xs) <- readVec'
      return (_ , x ∷ xs)

------------------------------------------------------------------------------

exactLength : {a : Set} {m : Nat} -> (len : Nat) -> (input : Vec a m) -> Maybe (Vec a len)
exactLength {_} {m} len input
  with m Data.Nat.≟ len
... | yes p rewrite p = just input
... | no  _           = nothing

printVecStringString : {len : Nat} -> Vec (String × String) len -> String
printVecStringString      [] = "[]"
printVecStringString ((l , r) ∷ xs) =
 "(" Data.String.++ l Data.String.++ " , " Data.String.++ r Data.String.++ ")"
 Data.String.++ " ∷ " Data.String.++ printVecStringString xs

zipInputs : IO ⊤
zipInputs = do
  IO.putStrLn "Enter 1st vector (blank line to end):"
  (len1 , vec1) <- readVec'
  IO.putStrLn "Enter 2nd vector (blank line to end):"
  (len2 , vec2) <- readVec'
  case exactLength len1 vec2 of λ where
    nothing  -> IO.putStrLn "nothing"
    (just v) ->
      let vz = Data.Vec.zip vec1 v
       in IO.putStrLn (printVecStringString vz)

------------------------------------------------------------------------------

main : Primitive.IO ⊤
main = run do
  IO.putStrLn "--------------------------------------------------"
  IO.putStrLn "readVecLen 4"
  IO.putStrLn "enter 4 lines:"
  vl <- readVecLen 4
  IO.putStrLn "result:"
  IO.putStrLn (printVecString vl)
  IO.putStrLn "--------------------------------------------------"
  IO.putStrLn "readVec"
  IO.putStrLn "enter lines, then a blank line:"
  vu <- readVec
  IO.putStrLn "result:"
  IO.putStrLn (printVecU vu)
  IO.putStrLn "--------------------------------------------------"
  IO.putStrLn "readVec'"
  IO.putStrLn "enter lines, then a blank line:"
  (_ , v) <- readVec'
  IO.putStrLn "result:"
  IO.putStrLn (printVecString v)
  IO.putStrLn "--------------------------------------------------"
  IO.putStrLn "zipInputs"
  zipInputs
