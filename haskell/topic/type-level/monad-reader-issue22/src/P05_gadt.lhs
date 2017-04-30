Created       : 2013 Aug 18 (Sun) 11:14:50 by carr.
Last Modified : 2017 Apr 29 (Sat) 20:02:44 by Harold Carr.

Generalized Algebraic Data Types in Haskell by Anton Dergunov

Monad Reader Issue 22
- http://themonadreader.files.wordpress.com/2013/08/issue221.pdf
- http://themonadreader.wordpress.com/

page numbers refer to Anton's tutorial

> -- {-# LANGUAGE DataKinds #-}
> -- {-# LANGUAGE ExistentialQuantification #-}
> -- {-# LANGUAGE FlexibleInstances #-}
> -- {-# LANGUAGE MultiParamTypeClasses #-}
> {-# LANGUAGE GADTs #-}
> -- {-# LANGUAGE TypeFamilies #-}
> -- {-# LANGUAGE TypeOperators #-}
>
> module P05_gadt where

import Data.Char
import Debug.Trace

debug = flip trace

------------------------------------------------------------------------------
* Intro (p. 5)

GADTs enable value constructors to return specific types (instance of more general type being defined).

Use-cases:
- DSLs
- generic programming
- ensuring correctness.

> -- ADT
> -- Test   : type constructor
> -- TI, TS : value constructors
> -- each value constructor can specify zero or more components
> data Test a
>   = TI Int
>   | TS String a

:t TI
-- TI :: Int -> Test a

:t TI 10
-- TI 10 :: Test a

:t TS
-- TS :: String -> a -> Test a

:t TS "test" 'c'
-- TS "test" 'c' :: Test Char

Equivalent using GADT (p. 7):

> data Test' a where
>   TI' :: Int         -> Test' a
>   TS' :: String -> a -> Test' a

GADT version that specifies return type:

> data Test'' a where
>   TI'' :: Int         -> Test'' Int  -- locks down return type HERE
>   TS'' :: String -> a -> Test'' a

:t TI''
-- TI'' :: Int -> Test'' Int
:t TI'' 10
-- TI'' 10 :: Test'' Int

-- TS'' same as TS
:t TS'' "foo" 'c'
-- TS'' "foo" 'c' :: Test'' Char


Key GADT feature: pattern matching causes type refinement, so guaranteed =Int=:

f :: Test'' a -> a
f (TI'' i) = i + 10

f (TI'' 3)
-- 13
