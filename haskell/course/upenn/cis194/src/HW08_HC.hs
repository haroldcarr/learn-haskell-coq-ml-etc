{-
Created       : 2014 Jun 17 (Tue) 15:48:54 by Harold Carr.
Last Modified : 2014 Jun 17 (Tue) 17:00:11 by Harold Carr.
-}

-- for exercise 1.2
{-# OPTIONS_GHC -fno-warn-orphans #-}

module HW08_HC where

import           HW08_Employee

import           Data.List       (union)
import           Data.Monoid

import qualified Test.HUnit      as T
import qualified Test.HUnit.Util as U

------------------------------------------------------------------------------
-- Exercise 1

-- 1

emptyGL :: GuestList
emptyGL = (GL [] 0)

glCons :: Employee -> GuestList -> GuestList
glCons e@(Emp _ ef) (GL gl glf) = GL (e:gl) (ef + glf)

-- 2

-- this does not take "boss" into account - same as glCons above
instance Monoid GuestList where
    mempty   = emptyGL
    mappend (GL lgl _) (GL rgl _) =
        let u = lgl `union` rgl
            f = foldr ((+) . empFun) 0 u
        in (GL u f)

-- 3

moreFun :: GuestList -> GuestList -> GuestList
moreFun l@(GL _ lf) r@(GL _ rf) | lf > rf   = l
                                | otherwise = r

-- test

you  :: Employee
you   = (Emp "you" 5)
me   :: Employee
me    = (Emp "me" 10)
them :: Employee
them  = (Emp "them" 15)

youAndMe        :: GuestList
youAndMe         = glCons you (glCons me   emptyGL)
meAndThem       :: GuestList
meAndThem        = glCons  me (glCons them emptyGL)
youAndMeAndThem :: GuestList
youAndMeAndThem  = youAndMe `mappend` meAndThem

glGuests :: GuestList -> [Employee]
glGuests (GL l _) = l
glFun :: GuestList -> Fun
glFun (GL _ f) = f

ex1 :: T.Test
ex1 = T.TestList
    [
      U.teq "e110" (glCons me emptyGL)                 (GL [me] 10)
    , U.teq "e120" (glFun youAndMeAndThem)             30
    , U.teq "e121" (length (glGuests youAndMeAndThem))  3
    , U.teq "e130" (moreFun youAndMe meAndThem)        meAndThem
    ]

------------------------------------------------------------------------------
-- Exercise 2

ex2 :: T.Test
ex2 = T.TestList
    [
    ]

------------------------------------------------------------------------------
-- Exercise 3

ex3 :: T.Test
ex3 = T.TestList
    [
    ]

------------------------------------------------------------------------------
-- Exercise 4

ex4 :: T.Test
ex4 = T.TestList
    [
    ]

------------------------------------------------------------------------------
-- Exercise 5

ex5 :: T.Test
ex5 = T.TestList
    [
    ]

------------------------------------------------------------------------------
-- Exercise 6

ex6 :: T.Test
ex6 = T.TestList
    [
    ]

------------------------------------------------------------------------------

-- TODO: remove this when final
t :: IO T.Counts
t = hw08

hw08 :: IO T.Counts
hw08 = do
    T.runTestTT ex1
    T.runTestTT ex2
    T.runTestTT ex3
    T.runTestTT ex4
    T.runTestTT ex5
    T.runTestTT ex6

-- End of file.
