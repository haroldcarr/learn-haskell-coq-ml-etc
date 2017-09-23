type_abstraction
data_type_abstraction
phantom_type

> module Ch05_05_phantom_types where
>
> import Test.HUnit      as U
> import Test.HUnit.Util as U

Phantom Types

Introduced in 1999 as solution for embedding type-safe domain specific language (DSL) in Haskell.

See Fun with Phantom Types, Hinze http://www.cs.ox.ac.uk/ralf.hinze/publications/With.pdf

Motivating example:

> data Expr = I   Int
>           | B   Bool
>           | Add Expr Expr
>           deriving (Eq, Show)

two problems:

`Add` can be given booleans:

> ch05_05_e1 = U.t "ch05_05_e1"
>   (Add (I 3) (B True))
>   (Add (I 3) (B True))

`eval` cannot pass type-checking

> -- eval (B v) = v
> -- eval (I v) = v
> -- eval (Add x y) = (eval x) + (eval y)
>
> -- Couldn't match expected type ‘Bool’ with actual type ‘Int’

phantom type solves the "boolean to `Add`" problem by adding the "phantom" type t:

> data ExprP t = IP   Int
>              | BP   Bool
>              | AddP (ExprP Int) (ExprP Int)
>              deriving (Eq, Show)

`ExprP` is parametrized by type `t`, but `t` does not appear in any of the constructors, hence the term "phantom type".

`t` serves as a placeholder that can be used by constructors to describe its particular type.

However, all the constructors still return the same type:

> -- :t IP
> -- IP :: Int -> ExprP t
> -- :t BP
> -- BP :: Bool -> ExprP t
> -- :t AddP
> -- AddP :: ExprP Int -> ExprP Int -> ExprP t

therefore invalid values can still be constructed:

> -- :t (IP 1)
> -- (IP 1) :: ExprP t
> -- :t (BP True)
> -- (BP True) :: ExprP t
> ap = U.t "ap"
>   (AddP (IP 1) (BP True))
>   (AddP (IP 1) (BP True))

solution: use phantom type info to create type-safe smart constructors:

> iP :: Int -> ExprP Int
> iP = IP
>
> bP :: Bool -> ExprP Bool
> bP = BP
>
> addP :: ExprP Int -> ExprP Int -> ExprP Int
> addP = AddP

Now type-checker prevents invalid values:

> -- addP (iP 1) (bP True)
> -- Couldn't match type ‘Bool’ with ‘Int’
> -- Expected type: ExprP Int
> --  Actual type: ExprP Bool
> -- In the second argument of ‘addP’, namely ‘(bP True)’

But type inference remains a problem:

> -- :t (IP 1)
> -- (IP 1) :: ExprP t -- need ExprP Int

But, same as above, def of `eval` will not type-check.

Despite limitations, phantom types useful.
E.g., Lens library uses the const phantom type: https://github.com/ekmett/lens/wiki/Derivation

Summary:
- phantom types enable type-safe construction
- to solve problem defining `eval` GADTs (next)

> runTests_Ch05_05 = runTestTT $ TestList $ ch05_05_e1 ++ ap
