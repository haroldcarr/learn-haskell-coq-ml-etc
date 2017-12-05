> {-# OPTIONS_GHC -fno-warn-missing-signatures            #-}
> {-# OPTIONS_GHC -Wno-missing-pattern-synonym-signatures #-}
>
> {-# LANGUAGE DeriveFunctor             #-}
> {-# LANGUAGE ExistentialQuantification #-}
> {-# LANGUAGE GADTs                     #-}
> {-# LANGUAGE LambdaCase                #-}
> {-# LANGUAGE OverloadedStrings         #-}
> {-# LANGUAGE PatternSynonyms           #-}
> {-# LANGUAGE TypeOperators             #-}
>
> module PF where
>
> import Control.Applicative
> import Control.Monad.State
> import Control.Monad.Free
> import Data.Attoparsec.ByteString.Char8

https://stackoverflow.com/questions/34631446/parsing-to-free-monads/34643032#34643032

> data ExampleF a
>   = GL String (Int -> a)
>   | PL Int            a
>   deriving Functor
>
> type Example = Free ExampleF
>
> gl :: String -> Example Int
> gl s = liftF $ GL s id
>
> pl :: Int -> Example ()
> pl i = liftF $ PL i ()

Note: each example returns a different type depending on last statement.

> plglUnit :: Example ()
> plglUnit  = do
>   i1 <- gl "one"
>   pl i1
>
> plglInt :: Example Int
> plglInt  =
>   gl "one"
>
> plGl :: Example ()
> plGl = do
>   pl 10
>   i1 <- gl "nice"
>   i2 <- gl ("nicer: " ++ show i1)
>   pl i2

Note
- 1st arg to analyze is type-specific
- must change type declaration to handle another type (but code the same)

> analyze :: Example () -> (Int, Int, [String], [Int], [Int])
> analyze t =
>   let (_, ng, np, go1, go2, po) = execState
>         (a t) ( [1::Int ..] -- input to GL 2nd arg fun
>               , 0           -- num GL
>               , 0           -- num PL
>               , []          -- output of GL - 1st arg
>               , []          -- input of GL - 2nd arg
>               , []          -- output of PL
>               )
>   in (ng, np, go1, go2, po)
>  where
>   a = foldFree $ \case
>     GL s next -> do
>       ( gi:gis, ng,   np,     go1,    go2,   po) <- get
>       put (gis, ng+1, np  , s:go1, gi:go2,   po)
>       return (next gi)
>     PL i next -> do
>       (    gis, ng,   np,     go1,    go2,   po) <- get
>       put (gis, ng  , np+1,   go1,    go2, i:po)
>       return  next

Want parser for things like:

foo 12
bar nice
foo 11
foo 42

not every Example value can be represented  without reimplementing some portion of Haskell
e.g., : 'return putStrLn' has type 'Example (String -> IO ())'

restrict input to calls to foo and bar sequenced with >> (i.e., no var bindings, no arbitrary computations)
- note: if binding is not a requirement, consider using free applicative instead of free monad

BNF:

<program> ::= ""
            | <expr> "\n" <program>
<expr>    ::= "gl " <string>
            | "pl " <integer>

> int :: Parser Int
> int = fmap read (many1 digit)
>
> parseGL :: Parser (Example Int)
> parseGL = string "gl " *> fmap gl (many1 (letter_ascii <|> digit))
>
> parsePL :: Parser (Example ())
> parsePL = string "pl " *> fmap pl int
>

how to give a type to composition of these two parsers?

parseExpr :: Parser (Example ???)
parseExpr = parseGL <|> parsePL

They have different types, so can't compose with '<|> :: Alternative f => f a -> f a -> f a'

type of parsed program depends on the value of input string. "Types depending on values" : dependent types

Force expressions on both sides of '<|>' in 'parseExpr' to have same type via erasing Example's type parameter using existential quantification.
Note: 'Ex' and ':*:' are machinery from Hasochism paper : http://homepages.inf.ed.ac.uk/slindley/papers/hasochism.pdf

> data Ex a = forall i. Wrap (a i)
>
> parseExpr0 :: Parser (Ex Example)
> parseExpr0 = fmap Wrap parseGL <|> fmap Wrap parsePL

now typechecks

but returns 'Example' containing value of unknown type

but Example's parameter is known to be either () or Int

common technique
- erase a type parameter by existential quantification
- make recoverable by bundling a runtime rep

wrap Example value with GADT evidence

> data Ty a where
>   IntTy  :: Ty Int
>   UnitTy :: Ty ()
>
> -- | functor product
> -- pairs up two type constructors, ensuring that their parameters are equal; thus, pattern matching on the Ty tells you about its accompanying Example.
> data (a :*: b) i = a i :&: b i
>
> -- | Sig(ma) type (i.e., dependent pair)
> -- type of second component depends on value of first
> type    Sig a b = Ex   (a :*: b)
> pattern Sig x y = Wrap (x :&: y)
>
> parseExpr :: Parser (Sig Ty Example)
> parseExpr =  fmap (Sig IntTy)  parseGL
>          <|> fmap (Sig UnitTy) parsePL

Ty is like a runtime "singleton" rep of Example's type parameter.
Pattern matching on IntTy say a ~ Int.

this use of Sig equivalent to Either (Example Int) (Example ())
- a sigma type is a sum

repeatedly apply the expression parser, then manipulate the dependent pairs in the list.

> parseProgram :: Parser (Sig Ty Example)
> parseProgram = fmap (foldr1 combine) $ parseExpr `sepBy1` char '\n'
>  where combine (Sig _ val) (Sig ty acc) = Sig ty (val >> acc)
>        combine          _            _  = error "parseProgram"

TODO: separate parsing and typechecking: first parsing into untyped syntax tree; then type-check/transform into typed version
- dependent pair technique still necessary to give type to output of type-checker, but don't tangle in parser

> exunit :: Result (Sig Ty Example)
> exunit = parse parseProgram "pl 12\ngl nice\npl 11\npl 42"
>
> exint :: Result (Sig Ty Example)
> exint = parse parseProgram "pl 12\ngl nice"
>
> (Fail fun fcon fmsg) = exunit
> (Partial pf) = exunit
> final = pf ""
> (Done di dr) = final

> parseFully :: Result r -> r
> parseFully r0 = case handlePartial r0 of
>   Fail _u _ctxs msg -> error  msg
>   Done _u r         -> r
>   _                 -> error "impossible"
>  where
>   handlePartial r = case r of
>     Partial f  -> f ""       -- tell the parser there is no more input
>     failOrDone -> failOrDone

> interpretF :: Sig Ty Example -> IO ()
> interpretF x = case x of
>   Wrap (UnitTy :&: Free (GL s _next)) -> putStrLn ("UTGL " ++ s)
>   Wrap (IntTy  :&: Free (GL i _next)) -> putStrLn ("ITGL " ++ show i)
>   Wrap (UnitTy :&: Free (PL i _next)) -> putStrLn ("UTPL " ++ show i)
>   Wrap (IntTy  :&: Free (PL i _next)) -> putStrLn ("ITPL " ++ show i)
>   Wrap (UnitTy :&: Pure s)            -> print s
>   Wrap (IntTy  :&: Pure i)            -> print i

> xxx :: Sig Ty Example -> (Int, Int, [String], [Int], [Int])
> xxx x = case x of
>   Wrap (UnitTy :&: f) -> analyze f
>   Wrap (IntTy  :&: _) -> error "IntTy not implemented"

xxx (parseFully exunit)
xxx (parseFully exint)


