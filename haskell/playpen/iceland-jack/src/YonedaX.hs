{-# OPTIONS_GHC -fno-warn-type-defaults #-}

{-# LANGUAGE InstanceSigs   #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RankNTypes     #-}

module YonedaX where

-- https://gist.github.com/Icelandjack/aecf49b75afcfcee9ead29d27cc234d5

import           Data.Char
import           Data.Functor.Yoneda

{-# ANN module ("HLint: ignore Use const" :: String) #-}

infixr 5
  ·

class List f where
  nil :: f a
  (·) :: a -> f a -> f a

-- A type class that allows constructing lists

abc :: List f => f Char
abc  = 'a'·'b'·'c'·nil

-- abc can be a String if we provide instance List [].
-- But what happens if we want to map over abc as is?
-- You might think that this requires a Functor constraint.

ordAbc :: Functor f => List f => f Int
ordAbc = fmap ord abc

-- But there is a way to get around that: Data.Functor.Yoneda
-- Think of Yoneda F as F in the process of being mapped over.
-- This means Yoneda F A has access to cont
--
--        vvvv
--   fmap cont (as :: F A)
--        ^^^^
--
instance List f => List (Yoneda f) where

  -- No need to map in the empty case
  --   fmap _cont [] = []
  nil :: Yoneda f a
  nil  = Yoneda (\_cont -> nil)

  -- As we cons, we apply 'cont' to the head
  --   fmap cont (a:as) = cont(a) : fmap cont as
  (·) :: a -> Yoneda f a -> Yoneda f a
  a ·         Yoneda as  =  Yoneda $ \cont -> cont a · as cont

-- Now we have the ability to map over forall f. List f => f a
-- without explicitly using fmap or assuming a Functor f instance at any point.
-- The mapping is built into the List (Yoneda f) instance.
-- We map over abc :: forall f. List f => f Char by instantiating f as Yoneda g.
-- Ignoring the newtype, this turns 'a'·'b'·'c'·nil into \cont -> cont 'a'·cont 'b'·cont 'c'·nil.

{-
import Data.Char
import Data.Functor.Yoneda
:set -XTypeApplications

>> :t abc @(Yoneda _)
.. :: List g => Yoneda g Char

then using runYoneda

>> :t runYoneda abc
.. :: List f => (Char -> b) -> f b

Now we just pass ord as an argument and we have successfully mapped over abc
-}
ordAbc' :: List g => g Int
ordAbc' = runYoneda abc ord

-- as if we had written abc already mapped, and then passed it ord

runYonedaAbc :: List f => (Char -> xx) -> f xx
runYonedaAbc f = f 'a'· f 'b'· f 'c'·nil

------------------------------------------------------------------------------

-- fmap :: Functor f => (a -> b) -> f a -> f b
myFmap  :: Functor f => (a -> b) -> f a -> f b
myFmap   = fmap

-- flip fmap  :: Functor f => f a -> (a -> b) -> f b

type Yo (f :: * -> *) a = (forall b. (a -> b) -> f b)

myFlippedFmap :: Functor f => f a -> Yo f a
myFlippedFmap  = flip myFmap

{-
myFlippedFmap [1] (+1)
myFlippedFmap [1] (+1)
myFlippedFmap [1] show
-}
