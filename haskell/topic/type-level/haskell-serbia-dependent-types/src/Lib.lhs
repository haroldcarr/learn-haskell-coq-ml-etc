> {-# LANGUAGE DataKinds              #-}
> {-# LANGUAGE FlexibleInstances      #-}
> {-# LANGUAGE GADTs                  #-}
> {-# LANGUAGE KindSignatures         #-}
> {-# LANGUAGE MultiParamTypeClasses  #-}
> {-# LANGUAGE PolyKinds              #-}
> {-# LANGUAGE StandaloneDeriving     #-}
> {-# LANGUAGE TypeFamilies           #-}
> {-# LANGUAGE TypeOperators          #-}
> {-# LANGUAGE UndecidableInstances   #-}
>
> module Lib where
>
> import Data.Kind
> import GHC.TypeLits

https://haskell-serbia.com/tutorial/4

Dependent types help form a compile time proof that critical features work correctly.

Set of types that ensures invariants met.

Dependent types use type level functions that are reduced using term level data.

Outline:
- Terms vs. Types
- Type Level Data
- Lambda Cube
- Local Assumptions
- Generic Programming
- Sigma and Pi

------------------------------------------------------------------------------
Terms vs. Types

term :: type :: kind
- term is of type
- type is of kind (the "type" of a type)

https://wiki.haskell.org/Kind

standard Haskell (no extensions)
- terms present at runtime
- types erased at runtime

------------------------------------------------------------------------------
* Type level data

A type has a set of possible term-level values.

> -- | type with zero inhabitants
> data Void
> instance Show Void where -- there will not be any instances, so this is useless, but used below
>   show x = "Void"

-- type with one inhabitant
data () = ()

-- type with two inhabitants
data Bool = True | False

Types can also contain type level data.
- data that lives on type level
- does not have term-level inhabitants

Example: a type level String (Symbol)

> type Greetings = ("Hello" :: Symbol)
>
> -- | (phantom) type param must be of type 'Symbol'
> data Label (greetings :: Symbol) = Get deriving (Show)

Type level data lives on level of types but is not a type.

:k Greetings
Greetings :: Symbol

:t Get :: Label Greetings
Get :: Label Greetings :: Label Greetings

:t Get :: Label String
    - Expected kind ‘Symbol’, but ‘String’ has kind ‘*’

------------------------------------------------------------------------------
Lambda Cube

https://en.wikipedia.org/wiki/Lambda_cube

Framework for exploring forms of abstraction.

1. Values depending on values (functions)

> -- | b depends on a
> a :: Integer
> a = 2
> b :: Bool
> b = even a

2.. Values depending on Types (classes)

> mbi =   (maxBound :: Int) == 9223372036854775807
> mbb = if maxBound :: Bool then "yes" else "no"

Type determines the values maxBound will have.

3. Types depending on Types (type functions)

uses TypeFamilies extension
- provides pattern matching on a type parameter

> -- | list-like data family
> data family XList a
>
> -- | list-like instance for Char
> data instance XList Char
>   = XCons !Char !(XList Char)
>   | XNil
>
> -- number-like instance for ()
> data instance XList ()
>   = XListUnit !Int

above: two instances of 'Xlist': 'Char' and '()'

https://hackage.haskell.org/package/type-list-0.5.0.0/docs/src/Data-Type-List.html#Reverse

> type family ReverseAcc xs acc where
>   ReverseAcc '[] acc = acc
>   ReverseAcc (x ': xs) acc = ReverseAcc xs (x ': acc)
>
> -- | type level funcion on type level data
> type family Reverse (xs :: [a]) :: [a] where
>   Reverse xs = ReverseAcc xs '[]

with TypeFamilies
- can say that type used as param to type constructor determines type that can be constructed
- type depending on another type

4. Types depending on Values (dependent types)

The rest focuses on #4.

------------------------------------------------------------------------------
Local Assumptions

> data Term a where
>   Lit    :: Int                           -> Term Int
>   Succ   :: Term Int                      -> Term Int
>   IsZero :: Term Int                      -> Term Bool
>   If     :: Term Bool -> Term a -> Term a -> Term a
>   Pair   :: Term a    -> Term b           -> Term (a,b)

With  ADT return type is always 'Term a'.
With GADT return types are more specific.

Can pattern match on more specific constructors:

> eval :: Term a -> a
> eval (Lit a) = a

:t Lit
Lit :: Int -> Term Int

:t :t eval (Lit 3)
eval (Lit 3) :: Int

Pattern matching on GADT type constructors
- can assume type was constructed with certain type param(s)
- also get with data families
  - but difference is getting "local assumptions"
    since able to use pattern matching on polymorphic type variables
  - therefore all possible results defined in GADt since fixed set of constructors

Two ways of saying same thing:

> data IntOrStringX a where
>  IntConstructorX    :: a ~ Int    => IntOrStringX a      -- a has a type constraint to Int
>  StringConstructorX :: a ~ String => IntOrStringX a      -- a has a type constraint to String

> data IntOrStringY a where
>  IntConstructorY    ::     Int    -> IntOrStringY Int    -- a has a type constraint to Int
>  StringConstructorY ::     String -> IntOrStringY String -- a has a type constraint to String

> wasItStringOrInt :: IntOrStringY a -> IntOrStringY b -> String
> wasItStringOrInt x0 y0 =
>   -- Local      : because limited to only one case branch
>   -- Assumption : because can assume type of type parameter
>   case x0 of
>     IntConstructorY x -> case y0 of
>       IntConstructorY    y -> show $ x +   y -- x ~ Int, y ~ Int
>       StringConstructorY y -> show   x ++  y -- x ~ Int, y ~ String
>     StringConstructorY x -> case y0 of
>       IntConstructorY    y -> x ++ show y    -- x ~ String, y ~ Int
>       StringConstructorY y -> x ++      y    -- x ~ String, y ~ String

let xx = IntConstructorY 42
let yy = StringConstructorY "Haskell rocks!"
wasItStringOrInt  yy xx
"Haskell rocks!42"

------------------------------------------------------------------------------
Generic programming

Haskell Algebraic Data Type

> data T
>   = A Int    Int
>   | B String String String
>   | C ()     Void
>   | D
>   deriving Show

T is a sum of products A B C D.

all ADTs can be encoded using
- Either    : provides sums
- (,)       : tuples provide products
- ()        : represents empty constructor
- ((->) r ) : represents function type : can take type/value and yield new type/value

T can be encoded:

> type T' = (Int, Int) `Either` (String, String, String) `Either` ((), Void) `Either` ()

The above is essence of ADTs.

To understand dependent types, need to understand Σ (sigma) and Π (pi).

https://en.wikipedia.org/wiki/Intuitionistic_type_theory

------------------------------------------------------------------------------
Σ and Π

pseudo code

Sigma type : a tuple with caveats

  (A     , B)
Σ (x :: A) B(x)

- x is of type A
- x used as args to type level function B(x)

example sigma type:

Σ (x :: Bool) (if x then Int else String)

possible values:

(True , 42)
(False, "abc")

Used in function

f :: Σ (x :: Bool) (if x then Int else String) -> String
f (x,y) = case x of
  True  -> show y -- if x is True  then y :: Int
  False ->      y -- if x is False then y :: String

After pattern match, can use local assumptions about y which is the term level.
- only need to handle Int and String : compiler ruled out everything else.

Σ : type level generalization of a sum types
- sum of all possible first components of a tuple (True + False above)

Π : type level generalization of product types

   A    -> B
Π (x :: A) B(x)

type of result of B depend on its input

f :: Π (x :: Bool) (if x then Int else String) -> String
f x = case x of
  True  -> 42
  False -> "abc"

Result of Π type is a function.

Why product type?

able to get back values from a Π type
- not the case for ∑ type
- ∑ type can have as a return type one of the case branches
- with Π, can extract values (similar to a product type in Haskell)

f True  == 42
f False == "abc"

Example using dependent types to detect wrong behavior at compile time.

> type Body = String
>
> data Method
>   = GET
>   | POST
>   deriving (Show)
>
> data SMethod m where
>   SGET  :: m ~ 'GET  => SMethod m
>   SPOST :: m ~ 'POST => SMethod m
>
> deriving instance Show (SMethod m)
>
> type family IfGetThenUnitElseMaybeBody (m :: Method) :: Type where
>   IfGetThenUnitElseMaybeBody 'GET  = ()         -- cannot have args
>   IfGetThenUnitElseMaybeBody 'POST = Maybe Body -- single arg is Maybe Body
>
> -- this type is similar to: Σ (x :: Bool)    (if x        then Int else String)
> -- specifically           : Σ (x :: SMethod) (if x == GET then ()  else Maybe Body)
> data Request m =
>   Req (SMethod m)
>       (IfGetThenUnitElseMaybeBody m)
>
> mkSMethod :: Method -> Either (SMethod 'GET) (SMethod 'POST)
> mkSMethod m = case m of
>   GET  -> Left  SGET
>   POST -> Right SPOST
>
> mkValidRequest :: Method -> Either (Request 'GET) (Request 'POST)
> mkValidRequest m = do
>   let requestBody = Just "POST BODY" :: Maybe Body
>   let sm = mkSMethod m
>   case sm of
>     Left  SGET  -> Left  $ Req SGET  ()
>     Right SPOST -> Right $ Req SPOST requestBody

import Data.Either
let x = mkValidRequest GET
:t x
isLeft x
isRight x

:t Req SGET ()
:t Req SPOST (Just "foo")
:t Req SPOST Nothing
-- these will get compiler errors
:t Req SGET "get"
:t Req SPOST "get"
:t Req SPOST (Just 'a')
