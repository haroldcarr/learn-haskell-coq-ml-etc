> {-# LANGUAGE GADTs #-}
>
> module P05_gadt where

Created       : 2013 Aug 18 (Sun) 11:14:50 by carr.
Last Modified : 2017 Apr 29 (Sat) 20:17:38 by Harold Carr.

Generalized Algebraic Data Types in Haskell by Anton Dergunov

Monad Reader Issue 22
- http://themonadreader.files.wordpress.com/2013/08/issue221.pdf
- http://themonadreader.wordpress.com/

page numbers refer to Anton's tutorial

------------------------------------------------------------------------------
* Intro (p. 5)

GADTs enable value constructors to return specific types (instance of more general type being defined).

use-cases
- DSLs
- generic programming
- ensuring correctness

> -- ADT
> -- Test   : type constructor
> -- a      : type parameter
> -- TI, TS : value constructors
> -- each value constructor can specify zero or more components
> data TestA a
>   = TI Int
>   | TS String a

:t TI
-- TI :: Int -> TestA a

:t TI 10
-- TI 10 :: TestA a

:t TS
-- TS :: String -> a -> TestA a

:t TS "test" 'c'
-- TS "test" 'c' :: TestA Char

Equivalent using GADT (p. 7):

> data TestG1 a where
>   TIG1 :: Int         -> TestG1 a
>   TSG1 :: String -> a -> TestG1 a

GADT version that specifies return type:

> data TestG2 a where
>   TIG2 :: Int         -> TestG2 Int  -- locks down return type
>   TSG2 :: String -> a -> TestG2 a

:t TIG2
-- TIG2 :: Int -> TestG2 Int
:t TIG2 10
-- TIG2 10 :: TestG2 Int

-- TSG2 same as TS
:t TSG2 "foo" 'c'
-- TSG2 "foo" 'c' :: TestG2 Char


Key GADT feature: pattern matching causes type refinement, so guaranteed =Int=:

f :: TestG2 a -> a
f (TIG2 i) = i + 10

f (TIG2 3)
-- 13
