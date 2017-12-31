> {-# LANGUAGE DataKinds #-}
> {-# LANGUAGE GADTs #-}
> {-# LANGUAGE KindSignatures #-}
> {-# LANGUAGE PolyKinds #-}
> {-# LANGUAGE RankNTypes #-}
> {-# LANGUAGE ScopedTypeVariables #-}
> {-# LANGUAGE TemplateHaskell #-}
> {-# LANGUAGE TypeFamilies #-}
> {-# LANGUAGE TypeOperators #-}
> {-# LANGUAGE UndecidableInstances #-}
>
> module S1 where
>
> import Data.Singletons.Prelude
> import Data.Singletons.TH

https://github.com/goldfirere/singletons
https://cs.brynmawr.edu/~rae/papers/2014/promotion/promotion.pdf

abstract

type-level missing features present at the term level
- case expressions
- anonymous functions
- partially-applied functions
- let expressions

This paper encodes these term-level constructs at the type level.

also discuss term-level features that are not promotable

implemented in singletons library


2 types and kinds

2.1 datakinds

term is classified by type
type is classified by kind

  data Bool = True | False                -- DataKinds

> --                   kind
> data OS (unixLike :: Bool) where        -- GADTs, KindSignatures
>   MacOS   :: OS 'True   -- type 'True
>   Linux   :: OS 'True   -- type 'True
>   Windows :: OS 'False  -- type 'False

2.2 Type families

open type families : 2005
closed type families : 2014

type family : function at type level

> data Nat1 = Zero | Succ Nat1
> type family IsZero (n :: Nat1) :: Bool where -- TypeFamilies
>   IsZero  'Zero    = 'True
>   IsZero ('Succ n) = 'False

closed type families have all equations written in one place
- GHC can use equations to infer kinds of the family arguments and result

2.3 Kind polymorphism

enables a definition to be abstract in its kinds

> type family Length (list :: [a]) :: Nat1 where -- PolyKinds
>   Length '[ ] = 'Zero
>   Length (x ': xs) = 'Succ (Length xs)  -- TypeOperators

'a' is a kind variable : classifies the type list

2.4 Type-level literals

Iavor Diatchki : http://www.haskell.org/ghc/docs/7.8.2/html/users_guide/

two kinds of type-level literals are supported
- natural numbers (Nat)
  - numeric literal in a type will produce a type of kind 'Nat' (NOT the one above)
  - type families: +, *, ...
- strings (Symbol)

3. Promoting functions

> $(promote [d|                       -- TemplateHaskell, Data.Singletons.Prelude, UndecidableInstances
>   hcmap :: (a -> b) -> [a] -> [b]
>   hcmap _      []  = []
>   hcmap f (x : xs) = f x : hcmap f xs
>  |])

3.1 A longer example – reordering of type-level lists

normal term-level code promoted automatically to type level

reorders xs1 to match the ordering in xs2
- all elements in xs1 that are equivalent to elements in xs2 are brought to the front of the result list
  -  placed in the same order as those elements in xs2
- Elements in xs1 not equivalent to anything in xs2 are left in the same order and moved to the end of the result list
- Extra elements in xs2 ignored

> $(promote [d|
>   reorderBy :: forall a. (a -> a -> Bool) -> [a] -> [a] -> [a] -- RankNTypes
>   reorderBy _ x [] = x
>   reorderBy eq x (h : t) = case extract h x of
>     (lst, Nothing)  -> reorderBy eq lst t
>     (lst, Just elt) -> elt : reorderBy eq lst t
>    where
>     extract :: a -> [a] -> ([a], Maybe a)                      -- ScopedTypeVariables
>     extract _     [] = ([], Nothing)
>     extract s (h : t)
>       | s `eq` h  = (t, Just s)
>       | otherwise = let (resList, resVal) = extract s t
>                     in (h : resList, resVal)
>   |])

This promoted function used in units library

Muranushi and R. A. Eisenberg.
Experience report: Type-checking polymorphic units for astrophysics research in Haskell.
In ACM SIGPLAN Haskell Symposium, 2014.

enables type-checking code with respect to units-of-measure.

type-check multiplication of two dimensioned quantities

units stores the dimensions of a quantity as a type-level list where order is insignificant

When type-checking multiplication, must combine two such lists, reordering one to match the other
- to avoid duplicating a dimension factor.

Reordering also used to ensure that addition happens between two quantities of the same dimension
- again neglecting the order of the type-level lists

3.2 Promoted Prelude

convenience: library modules containing promoted functions from
- Prelude
- Data.Bool
- Data.Either
- Data.List
- Data.Maybe
- Data.Tuple

5. Limitations

Eisenberg and Weirich 2012 listed features that were either not yet supported by singletons

Can now state that almost all features are implemented.

Exceptions

Infinite terms
- infinite terms possible via laziness
- not possible to construct infinite types
- e.g., : iterate :: (a → a) → a → [a]
          iterate f x = x : iterate f (f x)

promotion algorithm operates in an untyped setting and only reports errors when the algorithm gets stuck.
can generate definitions that are unusable

Literals
- rely on GHC’s built-in promotion of literals, so limited by GHC’s capabilities
- promotion of integer literals to type-level Nats is supported
  - drawbacks
    - negative integer literals do not promote
    - types do not work out – the type Int does not promote to the kind Nat.
- String literals problematic
  - after GHC promotes them to type level, no longer considered lists of characters
  - means impossible to promote code that concatenates two string literals using (++)

Datatypes storing functions
- do not support promotion of datatypes that store functions

do-notation
- not promotable

List comprehensions
- syntactic sugar for monadic notation, so do not promote

Arithmetic sequences
- rely on Enum type class, implemented using integers and infinite lists
- infinite, so NO

Show and Read type classes
- rely on string manipulation, not available on type-level Symbols.

Fixity declarations for datatypes
- Template Haskell bug: do not work

