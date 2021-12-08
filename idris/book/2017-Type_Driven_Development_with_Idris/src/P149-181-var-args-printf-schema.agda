module P149-181-var-args-printf-schema where

open import Data.Bool
open import Data.Char
open import Data.Fin
open import Data.Float
open import Data.List
open import Data.Maybe
open import Data.Nat
import      Data.Nat.Show as DNS
open import Data.Product
open import Data.String
open import Data.Vec
open import Function.Base
open import Relation.Nullary

Nat = ℕ

------------------------------------------------------------------------------

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

------------------------------------------------------------------------------
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

------------------------------------------------------------------------------
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

printf : (fmtS : String) -> PrintfType (toFormat (Data.String.toList fmtS))
printf fmtS = printfFmt (toFormat (Data.String.toList fmtS)) ""

{-
           toFormat (Data.String.toList "%s = %d")
printfFmt            (Str (Lit " = " (Number End))) "" "this" 45
printfFmt (toFormat (Data.String.toList "%s = %d")) "" "this" 45

printf "%s = %d" "this" 45
-}

------------------------------------------------------------------------------
-- determine schema from user input, then use type-level functions to compute correct type for data

infixr 5 _|+|_

data Schema : Set where
  SString : Schema
  SInt    : Schema
  _|+|_   : Schema -> Schema -> Schema

SchemaType : Schema -> Set
SchemaType SString   = String
SchemaType SInt      = Nat
SchemaType (x |+| y) = (SchemaType x × SchemaType y)

{-
SchemaType (SInt |+| SString)
-}

record DataStore : Set where
  constructor MkData
  field
    schema : Schema
    size   : Nat
    items  : Vec (SchemaType schema) size
open DataStore

addToStore : (store : DataStore) -> SchemaType (schema store) -> DataStore
addToStore (MkData schema size store) newitem = MkData schema _ (addToData store)
 where
  addToData : {oldsize : Nat}
           -> Vec (SchemaType schema)      oldsize
           -> Vec (SchemaType schema) (suc oldsize)
  addToData            []  = newitem ∷ []
  addToData (item ∷ items) = item ∷ addToData items

getEntry : (pos : Nat) -> (store : DataStore)
        -> Maybe (SchemaType (schema store))
getEntry pos store
  with pos Data.Nat.<? size store
... | no  _ = nothing
... | yes p = just (Data.Vec.lookup (items store) (Data.Fin.fromℕ< {pos} {size store} p))

display : {schema : Schema} -> (SchemaType schema) -> String
display {SString   }  item           = item
display {SInt      }  item           = DNS.show item
display {(x |+| x₁)} (iteml , itemr) =
  display iteml Data.String.++ ", " Data.String.++ display itemr

displayM : {schema : Schema} -> Maybe (SchemaType schema) -> String
displayM nothing     = "nothing"
displayM (just item) = display item

{-
                        MkData (SString |+| SInt) 1 (("Answer" , 42) ∷ [])
            addToStore (MkData (SString |+| SInt) 0 []) ("Answer", 45)
getEntry 0 (addToStore (MkData (SString |+| SInt) 0 []) ("Answer", 45))
displayM (getEntry 0 (addToStore (MkData (SString |+| SInt) 0 []) ("Answer", 45)))
-}

-- TODO : I quit, because I was not learning anything from this point on.

