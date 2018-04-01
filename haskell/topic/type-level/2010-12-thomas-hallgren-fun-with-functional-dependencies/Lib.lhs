> module Lib where

http://www.cse.chalmers.se/~hallgren/Papers/hallgren.pdf

Fun with FuntionalDependenies : Types as Values in StatiComputations
Thomas Hallgren - Deember 29,2000

types as a way to organize values, and
?     as a way to organize types

relation between types  and classes is similar to the
relation between values and types

types can belong to classes


three levels on which things are described
- values  : belong to types
- types   : belong to classes (an typeclass instance)
- classes :

two relations
- between values and types
- between types and classes

since only interested in types and classes
define data types with constructors

> data Zero
> data Succ n

> type Three = Succ (Succ (Succ Zero))

and type classes without operations

> class Even n
> class Odd n

predicaes:

> instance Even Zero
> instance Odd  n => Even (Succ n)
> instance Even n => Odd  (Succ n)
