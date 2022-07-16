
-- for Typeable
{-# LANGUAGE GADTs            #-}
{-# LANGUAGE TypeApplications #-}
-- for Data
{-# LANGUAGE DeriveDataTypeable #-}
-- for Generic
{-# LANGUAGE DeriveGeneric #-}

module Lib where

{-
https://www.youtube.com/watch?v=Zj8KXD9MRA0&t=2s

3 different ways GHC supports generic programming, use when
- Typeable : only need to know is the type of something           : at runtime
- Data     : the type AND how it was defined (i.e., constructors) : at runtime
- Generic  : make decisions                                       : at compiletime
-}

------------------------------------------------------------------------------
-- for Typeable example
import qualified Type.Reflection as T
------------------------------------------------------------------------------
-- for Data.Data example
import qualified          Data.Data     as D
import qualified          Data.Generics as D

------------------------------------------------------------------------------
-- for GHC.Generics
import qualified          GHC.Generics as G
------------------------------------------------------------------------------
-- Typeable : runtime type identification
-- use when you need to know what type something has at runtime

doSomethingOnInts :: T.Typeable a => a -> a
doSomethingOnInts x
  -- only looks at type, not value
  --                     v
  | Just T.HRefl <- T.typeOf x `T.eqTypeRep` T.typeRep @Int = x + 1
  | otherwise = x

-- >>> doSomethingOnInts 5
-- >>> doSomethingOnInts (5 :: Int)
-- >>> :t eqTypeRep
-- >>> :t typeOf
-- >>> :t typeRep
-- >>> :i :~~:

------------------------------------------------------------------------------
-- Data.Data : runtime type structure identification
-- note : Typeable is a superclass of Data

-- Typeable : tells us about the left hand side
-- do I know its type?  that would be do I know it is 'Wurble'
data Wurble
  -- Data.Data
  -- Tells us about the left and right hand sides.
  -- do I know it is Wurble and has this specific constructor?
  = MkW1 Int Bool Int Double
  | MkW2 Char Int
  deriving (D.Data, Show)

add1 :: D.Data a => a -> a
add1  = D.everywhere (D.mkT ((+1) :: Int -> Int))

-- >>> add1 (MkW1 2 True 30 0.0)
-- >>> :t everywhere
-- >>> add1 [MkW1 2 True 30 0.0, MkW1 0 True 100 0.1]
-- >>> : mkT
-- >>> add1 [MkW2 '2' 2, MkW2 '3' 3]

------------------------------------------------------------------------------
-- GHC.Generics
-- like 'Data' but also enable access at compiletime (besides runtime)

-- enables traversals that pick out particular element of record and optimize at compiletime

-- libraries that work with generics
-- - generic-lens
-- - generics-sop

data MyRecord = MkR { field1 :: Int, field2 :: Bool }
  deriving G.Generic

-- >>> :kind! (G.Rep MyRecord)
