> module Ch05_03_universal_quantification where

type_abstraction
datq_type_abstraction
universal_quantification

Universal quantification expresses parametric polymorphism in functions and datatypes.

Use `forall` in Rank-n function types to indicate nested parametric polymorphism.

Universal quantification is implicit default pattern when parameterizing types with types.

> data MB a = {- forall a. -} N  | J  a

Example: ADT with value and two functions on that operate on that value

> data U' a = U' a
>             (a -> Bool)
>             (a -> String)
>
> uf1 :: U' a -> Bool
> uf1 (U' v f1 _) = f1 v
>
> uf2 :: U' a -> String
> uf2 (U' v _ f2) = f2 v
>
> u   = U' 3 even show
> eu3 = uf1 u -- even 3
> su3 = uf2 u -- show 3

Above: packaged value with functions.
But did not encapsulate the value.
Existential quantification enables encapsulation (next).

