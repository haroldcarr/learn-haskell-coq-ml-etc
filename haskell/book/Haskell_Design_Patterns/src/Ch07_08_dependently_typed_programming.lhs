> {-# LANGUAGE GADTs        #-}
> {-# LANGUAGE TypeFamilies #-}
>
> module Ch07_08_dependently_typed_programming where

Dependently-typed programming

type-level programming where prior data types determine the types of subsequent values.

enables more nuanced type definitions
- e.g., "list of numbers of size n"
        "list of distinct strings"

sprintf : type-safe implementation : return type depends on value of format string

 sprintf "%s"  :: String -> String
 sprintf "%d"  :: Int -> String

ref : Fun with Types, Kiselyov et al

embedded language for format strings:

> data L      -- literals e.g. "hello"
> data V val  -- values e.g. (V Int) or (V String)

> -- F: format
> -- unifies L and V into a type to express print formats
> data F t where
>   -- takes string, returns format for literal strings F L
>   Lit :: String -> F L
>   -- takes polymorphic "to string" function, returns format for typed values F (V val).
>   Val ::  (val -> String) -> F (V val)

type-family to generate appropriate type signatures for sprintf

> type family   SPrintf f
> type instance SPrintf L       = String
> type instance SPrintf (V val) = val -> String

sprintf with return types computed by type function SPrintf

> sprintf :: F f -> SPrintf f
> sprintf (Lit str)   = str
> sprintf (Val show') = \x -> (show' x)


> ch07_08_e1 = sprintf (Lit "hello")

> -- sprintf :: SPrintf (V Float)
> ch07_08_e2 = sprintf (Val (show::Float -> String)) 1.2

> -- sprintf :: SPrintf (V String)
> ch07_08_e3 = sprintf (Val (show::String -> String)) "hello"

For Val values
- sprintf returns function of appropriate type
For Lit values
- returns what is given

above is generic programming in the dependently-typed style

Haskell and dependently-typed programming

division between term-level and type-level in Haskell
- Conor McBride : "the barrier represented by :: has not been broken"

Many language extensions make it harder to do type inference.
- need to add type signatures

