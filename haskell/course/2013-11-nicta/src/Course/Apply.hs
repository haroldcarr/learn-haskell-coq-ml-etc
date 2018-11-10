{-
Created       : 2014 Jul 15 (Tue) 05:01:37 by Harold Carr.
Last Modified : 2014 Jul 15 (Tue) 05:12:37 by Harold Carr.
-}

{-# LANGUAGE InstanceSigs        #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Course.Apply where

import           Course.Core
import           Course.Functor
import           Course.Id
import           Course.List
import           Course.Optional
import qualified Prelude         as P

import qualified Test.HUnit      as T
import qualified Test.HUnit.Util as U

class Functor f => Apply f where
  -- Pronounced apply.
  (<*>) ::
    f (a -> b)
    -> f a
    -> f b

infixl 4 <*>

-- | Implement @Apply@ instance for @Id@.
--
-- >>> Id (+10) <*> Id 8
-- Id 18
instance Apply Id where
  (<*>) ::
    Id (a -> b)
    -> Id a
    -> Id b
  (<*>) = mapId . runId
--  Id f <*> Id x = Id (f x)

-- | Implement @Apply@ instance for @List@.
--
-- >>> (+1) :. (*2) :. Nil <*> 1 :. 2 :. 3 :. Nil
-- [2,3,4,2,4,6]
instance Apply List where
  (<*>) ::
    List (a -> b)
    -> List a
    -> List b
  f <*> a = flatMap (`map` a) f
-- HC: (<*>) fs xs = foldRight (\f acc -> map f xs ++ acc) Nil fs

-- | Implement @Apply@ instance for @Optional@.
--
-- >>> Full (+8) <*> Full 7
-- Full 15
--
-- >>> Empty <*> Full 7
-- Empty
--
-- >>> Full (+8) <*> Empty
-- Empty
instance Apply Optional where
  (<*>) ::
    Optional (a -> b)
    -> Optional a
    -> Optional b
  f <*> a = bindOptional (`mapOptional` a) f
{- HC:
    (<*>)  Empty    _       = Empty
    (<*>)  _        Empty   = Empty
    (<*>) (Full f) (Full x) = Full (f x)
-}

tao :: [T.Test]
tao = U.tt "tao"
      [ Full (+8) <*> Full 7
      , bindOptional (`mapOptional` (Full 7)) (Full (+8)) -- def of <*> for Optional
      ,              (`mapOptional` (Full 7))       (+8)  -- def of bindOptional (removes "container" from second arg)
      ,         (+8)  `mapOptional` (Full 7)              -- partial "left" application
      ,                             (Full ((+8) 7))       -- definition of mapOptional
      ]
      (Full 15)

-- | Implement @Apply@ instance for reader.
--
-- (<*>) ::  f       (a -> b)  ->  f      a ->   f      b
-- (<*>) :: ((->) t) (a -> b)) -> ((->) t a) -> ((->) t b)
-- (<*>) :: (t ->     a -> b)  -> (t  ->  a) -> (t  ->  b)
--
-- returns a function : t -> b
--
-- >>> ((+) <*> (+10)) 3
-- 16
--
-- >>> ((+) <*> (+5)) 3
-- 11
--
-- >>> ((+) <*> (+5)) 1
-- 7
--
-- >>> ((*) <*> (+10)) 3
-- 39
--
-- >>> ((*) <*> (+2)) 3
-- 15
instance Apply ((->) t) where
  (<*>) ::
    ((->) t (a -> b))
    -> ((->) t a)
    -> ((->) t b)
  f1 <*> f2 = \x -> f1 x (f2 x)

-- | Apply a binary function in the environment.
--
-- >>> lift2 (+) (Id 7) (Id 8)
-- Id 15
--
-- >>> lift2 (+) (1 :. 2 :. 3 :. Nil) (4 :. 5 :. Nil)
-- [5,6,6,7,7,8]
--
-- >>> lift2 (+) (Full 7) (Full 8)
-- Full 15
--
-- >>> lift2 (+) (Full 7) Empty
-- Empty
--
-- >>> lift2 (+) Empty (Full 8)
-- Empty
--
-- >>> lift2 (+) length sum (listh [4,5,6])
-- 18
lift2 ::
  Apply f =>
  (a -> b -> c)
  -> f a
  -> f b
  -> f c
lift2 f a b = f <$> a <*> b

-- | Apply a ternary function in the Monad environment.
--
-- >>> lift3 (\a b c -> a + b + c) (Id 7) (Id 8) (Id 9)
-- Id 24
--
-- >>> lift3 (\a b c -> a + b + c) (1 :. 2 :. 3 :. Nil) (4 :. 5 :. Nil) (6 :. 7 :. 8 :. Nil)
-- [11,12,13,12,13,14,12,13,14,13,14,15,13,14,15,14,15,16]
--
-- >>> lift3 (\a b c -> a + b + c) (Full 7) (Full 8) (Full 9)
-- Full 24
--
-- >>> lift3 (\a b c -> a + b + c) (Full 7) (Full 8) Empty
-- Empty
--
-- >>> lift3 (\a b c -> a + b + c) Empty (Full 8) (Full 9)
-- Empty
--
-- >>> lift3 (\a b c -> a + b + c) Empty Empty (Full 9)
-- Empty
--
-- >>> lift3 (\a b c -> a + b + c) length sum product (listh [4,5,6])
-- 138
lift3 ::
  Apply f =>
  (a -> b -> c -> d)
  -> f a
  -> f b
  -> f c
  -> f d
lift3 f a b c = f <$> a <*> b <*> c

lift3C :: Apply f => (a -> b -> c -> d) -> f a -> f b -> f c -> f d
lift3C f a b c = lift2 f a b <*> c

-- | Apply a quaternary function in the environment.
--
-- >>> lift4 (\a b c d -> a + b + c + d) (Id 7) (Id 8) (Id 9) (Id 10)
-- Id 34
--
-- >>> lift4 (\a b c d -> a + b + c + d) (1 :. 2 :. 3 :. Nil) (4 :. 5 :. Nil) (6 :. 7 :. 8 :. Nil) (9 :. 10 :. Nil)
-- [20,21,21,22,22,23,21,22,22,23,23,24,21,22,22,23,23,24,22,23,23,24,24,25,22,23,23,24,24,25,23,24,24,25,25,26]
--
-- >>> lift4 (\a b c d -> a + b + c + d) (Full 7) (Full 8) (Full 9) (Full 10)
-- Full 34
--
-- >>> lift4 (\a b c d -> a + b + c + d) (Full 7) (Full 8) Empty  (Full 10)
-- Empty
--
-- >>> lift4 (\a b c d -> a + b + c + d) Empty (Full 8) (Full 9) (Full 10)
-- Empty
--
-- >>> lift4 (\a b c d -> a + b + c + d) Empty Empty (Full 9) (Full 10)
-- Empty
--
-- >>> lift4 (\a b c d -> a + b + c + d) length sum product (sum . filter even) (listh [4,5,6])
-- 148
lift4 ::
  Apply f =>
  (a -> b -> c -> d -> e)
  -> f a
  -> f b
  -> f c
  -> f d
  -> f e
lift4 f a b c d = f <$> a <*> b <*> c <*> d

lift4C :: Apply f => (a -> b -> c -> d -> e) -> f a -> f b -> f c -> f d -> f e
lift4C f a b c d = lift3 f a b c <*> d

-- | Sequence, discarding the value of the first argument.
-- Pronounced, right apply.
--
-- >>> [1,2,3] *> [4,5,6]
-- [4,5,6,4,5,6,4,5,6]
--
-- >>> [1,2] *> [4,5,6]
-- [4,5,6,4,5,6]
--
-- >>> [1,2,3] *> [4,5]
-- [4,5,4,5,4,5]
--
-- >>> Full 7 *> Full 8
-- Full 8
--
-- prop> [a,b,c] *> [x,y,z] == [x,y,z,x,y,z,x,y,z]
--
-- prop> Full x *> Full y == Full y
(*>) ::
  Apply f =>
  f a
  -> f b
  -> f b
(*>) = lift2 (const id) -- TODO: understand (especially applied to lists)

tsl :: [T.Test]
tsl = U.tt "tsl"
      [                      (11:.22:.Nil)  *> (100:.200:.Nil)
      , lift2 (const id)     (11:.22:.Nil)     (100:.200:.Nil) -- def of *>
      ,       (const id) <$> (11:.22:.Nil) <*> (100:.200:.Nil) -- def of lift2
      ,                      (id:.id:.Nil) <*> (100:.200:.Nil) -- def of instance Functor List
      ,             foldRight (\f acc -> map f (100:.200:.Nil) ++ acc) Nil (id:.id:.Nil) -- def of instance Apply List
      ]
      (100:.200:.100:.200:.Nil)

-- | Sequence, discarding the value of the second argument.
-- Pronounced, left apply.
--
-- >>> [1,2,3] <* [4,5,6]
-- [1,1,1,2,2,2,3,3,3]
--
-- >>> [1,2] <* [4,5,6]
-- [1,1,1,2,2,2]
--
-- >>> [1,2,3] <* [4,5]
-- [1,1,2,2,3,3]
--
-- >>> Full 7 <* Full 8
-- Full 7
--
-- prop> [x,y,z] <* [a,b,c] == [x,x,x,y,y,y,z,z,z]
--
-- prop> Full x <* Full y == Full x
(<*) ::
  Apply f =>
  f b
  -> f a
  -> f b
(<*) = lift2 const -- TODO: understand (especially applied to lists)

tsr :: [T.Test]
tsr = U.tt "tsr"
      [                 (11:.22:.Nil) <*  (100:.200:.Nil)
      , lift2 const     (11:.22:.Nil)     (100:.200:.Nil) -- def of <*
      ,       const <$> (11:.22:.Nil) <*> (100:.200:.Nil) -- def of lift2
      , ((const 11):.(const 22):.Nil) <*> (100:.200:.Nil) -- def of instance Functor List
      ,        foldRight (\f acc -> map f (100:.200:.Nil) ++ acc) Nil ((const 11):.(const 22):.Nil) -- def of instance Apply List
      ]
      (11:.11:.22:.22:.Nil)

-----------------------
-- SUPPORT LIBRARIES --
-----------------------

instance Apply IO where
  f <*> a =
    f P.>>= \f' -> P.fmap f' a

instance Apply [] where
  f <*> a =
    f P.>>= \f' -> P.fmap f' a

instance Apply P.Maybe where
  f <*> a =
    f P.>>= \f' -> P.fmap f' a

(>>) ::
  Apply f =>
  f a
  -> f b
  -> f b
(>>) =
  (*>)

------------------------------------------------------------------------------

testApply :: IO T.Counts
testApply =
    T.runTestTT P.$ T.TestList P.$ tao P.++ tsl P.++ tsr

-- End of file.
