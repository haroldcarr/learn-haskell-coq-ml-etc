> {-# LANGUAGE GADTs #-}
>
> module Ch05_07_typecase where

-- http://www.cs.ox.ac.uk/jeremy.gibbons/publications/typecase.pdf

TypeCase : design pattern for defining closed type-indexed functions,
in which index family is fixed but collection of functions is extensible.

Use-case for GADTs: generic programming.

example: type representation that unifies types Int, Char, and List:

> data Rep t where
>   RInt  :: Rep Int
>   RChar :: Rep Char
>   RList :: Show a => Rep a -> Rep [a]

RList is existentially qualified (a does not appear on the left-hand side).

Phantom type t serves as type metadata.

> showT :: Show t => Rep t -> t -> String
>
> showT RInt i  = (show i) ++ " :: INT"
> showT RChar i = (show i) ++ " :: Char"
>
> showT (RList rep) [] = "THE END"
> showT (RList rep) (x:xs)
>    = (showT rep x) ++ ", " ++
>      (showT (RList rep) xs)

`showT` is a CLOSED type-indexed function because the type index family (Rep t) is fixed.
- defined for each member of the family of types Rep t:

> ch_05_07_e1 = showT RInt 3
> ch_05_07_e2 = showT (RList RInt) [12,13,14]
> ch_05_07_e3 = showT (RList RChar) ['2','3','5']

Whereas `show` of `Show` type-class is an open type-indexed function.
- type-indexed by instances of Show
- "open" because new types can be added to the type index (i.e., new instances of type-class `Show`).
