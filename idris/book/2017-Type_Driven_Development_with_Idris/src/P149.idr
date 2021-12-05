module Main

import Data.Vect

------------------------------------------------------------------------------
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

------------------------------------------------------------------------------
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

------------------------------------------------------------------------------
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

------------------------------------------------------------------------------
-- determine schema from user input, then use type-level functions to compute correct type for data

infixr 5 .+.

data Schema
  = SString
  | SInt
  | (.+.) Schema Schema

SchemaType : Schema -> Type
SchemaType SString   = String
SchemaType SInt      = Int
SchemaType (x .+. y) = (SchemaType x, SchemaType y)

{-
SchemaType (SInt .+. SString)
-}

{- Instead of manually defining type and projections, use a record
data DataStore : Type where
  MkData : (schema : Schema) -> (size : Nat) -> (items : Vect size (SchemaType schema))
        -> DataStore

size : DataStore -> Nat
size (MkData _ size' _) = size'

schema : DataStore -> Schema
schema (MkData schema' _ _) = schema'

items : (store : DataStore) -> Vect (size store) (SchemaType (schema store))
items (MkData _ _ items') = items'
-}

record DataStore where
  constructor MkData
  schema : Schema
  size   : Nat
  items  : Vect size (SchemaType schema)

addToStore : (store : DataStore) -> SchemaType (schema store) -> DataStore
addToStore (MkData schema size store) newitem = MkData schema _ (addToData store)
 where
  addToData : Vect    oldsize  (SchemaType schema)
           -> Vect (S oldsize) (SchemaType schema)
  addToData             []  = [newitem]
  addToData (item :: items) = item :: addToData items

getEntry : (pos : Integer) -> (store : DataStore)
        -> Maybe (String, DataStore)
getEntry pos store =
  case integerToFin pos (size store) of
    Nothing => Just ("Out of range\n", store)
    Just id => Just (display (index id (items store)) ++ "\n", store)
 where
   display : SchemaType schema -> String
   display {schema = SString  } item           = show item
   display {schema = SInt     } item           = show item
   display {schema = (x .+. y)} (iteml, itemr) = display iteml ++ ", " ++ display itemr

{-
:let teststore = (MkData (SString .+. SInt) 1 [("Answer", 42)])
:let teststore = (MkData (SString .+. SInt) 0 [])
:let teststore' = addToStore teststore ("Answer", 45)
schema teststore'
size teststore'
items teststore'
getEntry 0 teststore'
-}

-- TODO : I quit, because I was not learning anything from this point on.

data Command : Schema -> Type where
  SetSchema : (newschema : Schema) -> Command schema
  Add       : SchemaType schema    -> Command schema
  Get       : Integer              -> Command schema
  Quit      :                         Command schema

parsePrefix : (schema : Schema) -> String -> Maybe (SchemaType schema, String)
parsePrefix SString input = getQuoted (unpack input)
 where
  getQuoted : List Char -> Maybe (String, String)
  getQuoted ('"' :: xs) =
    case span (/= '"') xs of
      (quoted, '"' :: rest) => Just (pack quoted, ltrim (pack rest))
      _                     => Nothing
  getQuoted _ = Nothing
parsePrefix SInt input =
  case span isDigit input of
    ("" , rest) => Nothing
    (num, rest) => Just (cast num, ltrim rest)

parseBySchema : (schema : Schema) -> String -> Maybe (SchemaType schema)
parseBySchema schema input =
  case parsePrefix schema input of
    Just (res, "") => Just res
    Just        _  => Nothing
    Nothing        => Nothing

parseSchema : List String -> Maybe Schema
parseSchema ("String" :: xs) =
  case xs of
    [] => Just SString
    _  => case parseSchema xs of
            Nothing => Nothing
            Just xs_sch => Just (SString .+. xs_sch)
parseSchema ("Int" :: xs) =
  case xs of
    [] => Just SInt
    _  => case parseSchema xs of
            Nothing     => Nothing
            Just xs_sch => Just (SInt .+. xs_sch)
parseSchema _ = Nothing

parseCommand : (schema : Schema) -> String -> String -> Maybe (Command schema)
parseCommand schema "add" rest = Just (Add (?parseBySchema rest))
parseCommand schema "get" val  =
   case all isDigit (unpack val) of
     False => Nothing
     True  => Just (Get (cast val))
parseCommand schema "quit" ""  = Just Quit
parseCommand schema "schema" rest =
  case parseSchema (words rest) of
    Nothing       => Nothing
    Just schemaok => Just (SetSchema schemaok)
parseCommand _ _ _             = Nothing

parse : (schema : Schema) -> (input : String) -> Maybe (Command schema)
parse schema input =
  case span (/= ' ') input of
    (cmd, args) => parseCommand schema cmd (ltrim args)

setSchema : (store : DataStore) -> Schema -> Maybe DataStore
setSchema store schema =
  case size store of
    Z   => Just (MkData schema _ [])
    S k => Nothing

processInput : DataStore -> String -> Maybe (String, DataStore)
processInput store input =
  case parse (schema store) input of
    Nothing                  => Just ("Invalid command\n", store)
    Just (Add item)          => Just ("ID " ++ show (size store) ++ "\n", addToStore store item)
    Just (SetSchema schema') =>
      case setSchema store schema' of
        Nothing              => Just ("Can't update schema\n", store)
        Just store'          => Just ("OK\n", store')
    Just (Get pos)           => getEntry pos store
    Just Quit                => Nothing

 
