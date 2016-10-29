> module Ch05_05_phantom_types where

type_abstraction
data_type_abstraction
phantom_type

Phantom Types

Introduced in 1999 as a solution for embedding a type-safe domain specific language (DSL) in Haskell.

See Fun with Phantom Types, Hinze http://www.cs.ox.ac.uk/ralf.hinze/publications/With.pdf

Motivating example:

Consider this trivial expression language and evaluator:

> data Expr = I Int
>           | B Bool
>           | Add Expr Expr
>           deriving Show

two problems:

`Add` can be given booleans: `(Add (I2 11) (B2 True))`

`eval` cannot pass type-checking

> -- eval (B v) = v
> -- eval (I v) = v
> -- eval (Add x y) = (eval x) + (eval y)
>
> -- Couldn't match expected type ‘Bool’ with actual type ‘Int’

Phantom type solve the "boolean to `Add`" problem by adding the type t:

> data ExprP t = IP Int
>              | BP Bool
>              | AddP (ExprP Int) (ExprP Int)
>              deriving Show

`Expr3` is parametrized by type `t`, but `t` does not appear in any of the constructors, hence the term phantom type.

`t` serves as a placeholder that can be used by constructors to describe its particular type.

However, all the constructors still return the same type:

> -- :t IP
> -- IP :: Int -> ExprP t
> -- :t BP
> -- BP :: Bool -> ExprP t
> -- :t AddP
> -- AddP :: ExprP Int -> ExprP Int -> ExprP t

But invalid values can still be constructed:

> -- :t (IP 1)
> -- (IP 1) :: ExprP t
> -- :t (BP True)
> -- (BP True) :: ExprP t
> ap = AddP (IP 1) (BP True)

Solution: use phantom type info to create type-safe smart constructors:

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
- Phantom types enable type-safe construction.

- To solve problem defining `eval` GADTs

