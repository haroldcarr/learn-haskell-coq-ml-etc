module P149 where

open import Data.Bool
open import Data.Char
open import Data.Float
open import Data.List
open import Data.Nat
import      Data.Nat.Show as DNS
open import Data.Product
open import Data.String
open import Data.Vec
open import Function.Base

Nat = ℕ

-- type synonyms

Position : Set
Position = (Float × Float)

Polygon : Nat -> Set
Polygon = Vec Position

tri : Polygon 3
tri = (0.0 , 0.0) ∷ (3.0 , 0.0) ∷ (0.0 , 4.0) ∷ []

-- -- type level functions with pattern matching

StringOrInt : Bool -> Set
StringOrInt false = String
StringOrInt true  = Nat

-- -- dependent pattern matching : inspecting value of one arg determines type of another

getStringOrInt : (isInt : Bool) -> StringOrInt isInt
getStringOrInt false = "Ninety four"
getStringOrInt true  = 94

valToString : (isInt : Bool) -> StringOrInt isInt -> String
valToString false y = {-trim-} y
valToString true  y = {-cast-} DNS.show y

-- -- using case expressions in types

valToString' : (isInt : Bool)
            -> (case isInt of λ where
                 false -> String
                 true  -> Nat)
            -> String
valToString' false y = {-trim-} y
valToString' true  y = {-cast-} DNS.show y

-- -- functions with variable number of args

AdderType : (numargs : Nat) -> Set -> Set
AdderType  zero   numType = numType
AdderType (suc k) numType = (next : numType) -> AdderType k numType

adder : {-Num numType
     => -}
        (numargs : Nat)
     -> {-numType-} Nat
     -> AdderType numargs {-numType-} Nat
adder  zero   acc = acc
adder (suc k) acc = λ next -> adder k (next Data.Nat.+ acc)

{-
adder 0 1
adder 2 2 2 2
adder 3 10 10 10 10
-}

-- type-safe printf

data Format : Set where
  Number :           Format -> Format
  Str    :           Format -> Format
  Lit    : String -> Format -> Format
  End    :                     Format

PrintfType : Format -> Set
PrintfType (Number  fmt) = (i   : Nat)    -> PrintfType fmt
PrintfType (Str     fmt) = (str : String) -> PrintfType fmt
PrintfType (Lit str fmt) =                   PrintfType fmt
PrintfType End           = String

printfFmt : (fmt : Format) -> (acc : String) -> PrintfType fmt
printfFmt (Number  fmt) acc = λ i   -> printfFmt fmt (acc Data.String.++ DNS.show i)
printfFmt (Str     fmt) acc = λ str -> printfFmt fmt (acc Data.String.++ str)
printfFmt (Lit lit fmt) acc =          printfFmt fmt (acc Data.String.++ lit)
printfFmt End           acc = acc

toFormat : (xs : List Char) -> Format
toFormat                 []  = End
toFormat ('%' ∷ 'd' ∷ chars) = Number  (toFormat chars)
toFormat ('%' ∷ 's' ∷ chars) = Str     (toFormat chars)
toFormat ('%'       ∷ chars) = Lit "%" (toFormat chars)
toFormat (c         ∷ chars) =
  case toFormat chars of λ where
    (Lit lit chars') -> Lit (Data.String.fromList (c ∷ (Data.String.toList lit))) chars'
    fmt              -> Lit (Data.String.fromList (c ∷ (Data.String.toList "")))  fmt

printf : (fmt : String) -> PrintfType (toFormat (Data.String.toList fmt))
printf fmt = printfFmt _ ""
{-
———— Errors ————————————————————————————————————————————————
Failed to solve the following constraints:
  PrintfType _fmt_55
    =< PrintfType (toFormat (Data.String.toList fmt))
-}

{-
           toFormat (Data.String.toList "%s = %d")
printfFmt            (Str (Lit " = " (Number End))) "" "this" 45
printfFmt (toFormat (Data.String.toList "%s = %d")) "" "this" 45

printf "%s = %d" "this" 45
-}
