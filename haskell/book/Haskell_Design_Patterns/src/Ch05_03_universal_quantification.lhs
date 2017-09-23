type_abstraction
datq_type_abstraction
universal_quantification

> module Ch05_03_universal_quantification where
>
> import Test.HUnit      as U
> import Test.HUnit.Util as U

Universal quantification
- expresses parametric polymorphism in functions and datatypes.
- is implicit default pattern when parameterizing types with types.

> data MB a = {- forall a. -} N  | J  a

Example: ADT with value and two functions that operate on that value

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
> eu3 = U.t "eu3" (uf1 u) False -- even 3
> su3 = U.t "su3" (uf2 u) "3"   -- show 3

Above: packaged value with functions.
But did not encapsulate the value:

> uv :: U' a -> a
> uv (U' v _ _) = v
> gu3 = U.t "gu3" (uv u) 3

Existential quantification enables encapsulation (next).

> runTests_Ch05_03 = runTestTT $ TestList $ eu3 ++ su3 ++ gu3

