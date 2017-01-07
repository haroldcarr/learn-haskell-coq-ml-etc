> module Ch07_01_higher_order_kinds where

Patterns of Kind Abstraction (i.e., type-level programming)

Beginnings of type-level programming
- functional dependencies
- GADTs

type-level == kind-level

Types classify values

–- Primitive types
"a string" :: String
3          :: Int

-- higher-order, parameterized types
Just 10  :: Maybe  Int
Left 10  :: Either Int b

-- functions are values
(* 2) :: Num a => a -> a

-- type-constructors are functions
Just :: a -> Maybe a
Left :: a -> Either a b

Kinds classify types

For monomorphic types (i.e, not polymorphic) the kind signature is `*`:

-- TYPE         KIND
[Char]      ::   *
Maybe Int   ::   *

Parametric types express higher-order kinds, for example:

Maybe  ::  * -> *
--         a -> Maybe a

where `* -> *` is a placeholder for `a -> Maybe a`

Either       * -> * -> *
 --          a -> b -> Either a b

kinds distinguish
- "lifted" types    : `*`
- type constructors : e.g., `* -> * -> *`

kind signatures signify
- arity of parameterization of a type
- i.e., the number and position of type parameters in a type

arity says nothing about type
- kind system is untyped
