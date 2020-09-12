type_abstraction
data_type_abstraction
gadt

> {-# LANGUAGE GADTs #-}
>
> module Ch05_06_gadt where
>
> import Test.HUnit      as U
> import Test.HUnit.Util as U

Generalized Algebraic Datatypes

Emerged independently in ML and Haskell in 2003.
Were a part of GHC by 2005
see
- History of Haskell, Hudak et al.            -- http://haskell.cs.yale.edu/wp-content/uploads/2011/02/history.pdf
- First-class Phantom Types, Cheney and Hinze -- https://ecommons.cornell.edu/handle/1813/5614

GADTs bring together
- phantom types
- smart constructors
- refined pattern matching

> data Expr t where  -- phantom 't'
>   -- built-in smart constructors
>   I   :: Int  -> Expr Int
>   B   :: Bool -> Expr Bool
>   Add :: Expr Int -> Expr Int -> Expr Int

GADT constructors describe constrained instances of Expr t (provide increased type safety for data construction).

GADT smart constructors built into type so pattern matching possible.  Now can define eval:

> eval :: Expr t -> t
> eval (I v) = v
> eval (B v) = v
> eval (Add x y) = eval x + eval y
>
> eg = U.t "eq"
>   (eval (Add (I 1) (I 2)))
>   3

GADTs not expressed by syntax
- rather by relationship between type parameters and constructor return types.

Phantom types are not expressed by syntax
- implied by lack of appearance of a type parameter in the type constructors

Difference in meaning of type parameter:
- phantom: signify type of embedded value
- gadt: expressing type metadata

> runTests_Ch05_06 = runTestTT $ TestList {- $ -} eg
