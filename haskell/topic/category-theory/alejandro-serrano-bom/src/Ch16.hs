module Ch16 where

import           Test.HUnit      (Counts, Test (TestList), runTestTT)
import qualified Test.HUnit.Util as U (t)

------------------------------------------------------------------------------
-- ch 16

{-
2nd : composition of two functors which are related via an adjoint relation
- e.g., newtype State s a = State { runState :: s -> (a, s) }
- instead of monolithic block, see State as composition of
  - functor (, s) : pairs any value a with an output value of type s
  - functor (s ->) : adds an input of type s
- since two functors are adjoint, their composition is a monad

adjointness : topic of Chapter 18
- introduce notion decoupled from monads
- then describe construction which builds monad out of adjunction
- then comonads : also built from an adjunction by similar mechanism

Part IV : concept of free monad : build a monad "for free"
- can sharpen concept via adjoint functor
- construction of free monad is adjoint to forgetful functor

Category Theory for Programmers [Milewski, 2014]
Categories for the Working Mathematician [Mac Lane, 1998]
Category Theory in Context [Riehl, 2016]
-}

ts16 :: [Test]
ts16 = U.t "t05" 'a' 'a'

------------------------------------------------------------------------------
t16 :: IO Counts
t16  =
  runTestTT $ TestList {-$-}
  ts16
