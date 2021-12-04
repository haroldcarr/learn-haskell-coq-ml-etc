module Main

import Data.Vect

-- type synonyms

Position : Type
Position = (Double, Double)

Polygon : Nat -> Type
Polygon n = Vect n Position

tri : Polygon 3
tri = [(0.0, 0.0), (3.0, 0.0), (0.0, 4.0)]

-- type level functions with pattern matching

StringOrInt : Bool -> Type
StringOrInt False = String
StringOrInt True  = Int

-- dependent pattern matching : inspecting value of one arg determines type of another

getStringOrInt : (isInt : Bool) -> StringOrInt isInt
getStringOrInt False = "Ninety four"
getStringOrInt True  = 94

valToString : (isInt : Bool) -> StringOrInt isInt -> String
valToString False y = trim y
valToString True y  = cast y

-- using case expressions in types

valToString' : (isInt : Bool)
            -> (case isInt of
                 False => String
                 True => Int)
            -> String
valToString' False y = trim y
valToString' True y  = cast y

-- functions with variable number of args

AdderType : (numargs : Nat) -> Type -> Type
AdderType  Z    numType = numType
AdderType (S k) numType = (next : numType) -> AdderType k numType

adder : Num numType
     => (numargs : Nat)
     -> numType
     -> AdderType numargs numType
adder  Z    acc = acc
adder (S k) acc = \next => adder k (next + acc)

{-
adder 0 1
adder 2 2 2 2
adder 3 10 10 10 10
-}

-- type-safe printf

data Format
  = Number     Format
  | Str        Format
  | Lit String Format
  | End

PrintfType : Format -> Type
PrintfType (Number  fmt) = (i   : Int)    -> PrintfType fmt
PrintfType (Str     fmt) = (str : String) -> PrintfType fmt
PrintfType (Lit str fmt) =                   PrintfType fmt
PrintfType End           = String

printfFmt : (fmt : Format) -> (acc : String) -> PrintfType fmt
printfFmt (Number  fmt) acc = \i   => printfFmt fmt (acc ++ show i)
printfFmt (Str     fmt) acc = \str => printfFmt fmt (acc ++ str)
printfFmt (Lit lit fmt) acc =         printfFmt fmt (acc ++ lit)
printfFmt End           acc = acc

toFormat : (xs : List Char) -> Format
toFormat                   []  = End
toFormat ('%' :: 'd' :: chars) = Number  (toFormat chars)
toFormat ('%' :: 's' :: chars) = Str     (toFormat chars)
toFormat ('%'        :: chars) = Lit "%" (toFormat chars)
toFormat (c          :: chars) =
  case toFormat chars of
    Lit lit chars' => Lit (strCons c lit) chars'
    fmt            => Lit (strCons c "")  fmt

printf : (fmt : String) -> PrintfType (toFormat (unpack fmt))
printf _ = printfFmt _ ""

{-
printf "%s = %d" "this" 45
-}








