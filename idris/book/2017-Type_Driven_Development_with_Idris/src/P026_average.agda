module P026_average where

open import Agda.Builtin.Equality
open import Agda.Builtin.Sigma
open import Agda.Builtin.Unit
open import Data.Bool
open import Data.Char
open import Data.Empty
open import Data.Float
open import Level using (Level)
open import Data.List
open import Data.Nat
open import Data.Nat.DivMod
import      Data.Nat.Show as DNS
open import Data.String
open import Function.Base
open import Relation.Nullary
open import Relation.Unary

Nat = ℕ

IsSpace : Char -> Set
IsSpace c = if isSpace c then ⊤ else ⊥

isSpaceDec : (c : Char) -> Dec (IsSpace c)
isSpaceDec c
  with isSpace c
... | false = no  λ z → z
... | true  = yes tt

{-# TERMINATING #-}
words : String -> List String
words s = case dropWhile isSpaceDec (toList s) of λ where
  [] -> []
  s' -> case break isSpaceDec s' of λ where
          (w , s'') → fromList w ∷ words (fromList s'')

average : (str : String) -> Nat
average str =
    let numWords    = wordCount str
        totalLength = sum (allLengths (words str))
    in case numWords of λ where
         0 -> 0
         x -> totalLength / x

  where
    wordCount : String -> Nat
    wordCount str = Data.List.length (words str)

    allLengths : List String -> List Nat
    allLengths strs = map (λ x -> Data.List.length (toList x)) strs

showAverage : String -> String
showAverage str = "The average word length is: "
                  Data.String.++ DNS.show (average str)
                  Data.String.++ "\n"

-- main : IO ()
-- main = repl "Enter a string: " showAverage

{-
average "one two three four five six seven eight nine ten qwertyui"
showAverage "one two three four five six seven eight nine ten qwertyui"
-}
