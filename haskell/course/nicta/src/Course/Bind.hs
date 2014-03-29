{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE RebindableSyntax    #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Course.Bind(
  Bind(..)
, (>>=)
, join
, (<=<)
, testBind
) where

import           Course.Apply    (Apply)
import           Course.Core
import           Course.Functor
import           Course.Id
import           Course.List
import           Course.Optional
import qualified Prelude         as P

import qualified Test.HUnit      as T
import qualified Test.HUnit.Util as U

class Apply f => Bind f where
  -- Pronounced, bind.
  (=<<) ::
    (a -> f b)
    -> f a
    -> f b

infixr 1 =<<

-- | Witness that all things with (=<<) and (<$>) also have (<*>).
--
{- HC: TODO: this causes ./test.sh Bind to hang (works in ghci)
--
-- >>> Id (+10) <*> Id 8
-- Id 18
--
-}
--
-- >>> (+1) :. (*2) :. Nil <*> 1 :. 2 :. 3 :. Nil
-- [2,3,4,2,4,6]
--
-- >>> Full (+8) <*> Full 7
-- Full 15
--
-- >>> Empty <*> Full 7
-- Empty
--
-- >>> Full (+8) <*> Empty
-- Empty
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
(<*>) ::
  Bind f =>
  f (a -> b)
  -> f a
  -> f b
(<*>) f fa =  f <*> fa

infixl 4 <*>

-- | Binds a function on the Id monad.
--
-- >>> (\x -> Id(x+1)) =<< Id 2
-- Id 3
instance Bind Id where
  (=<<) f = f . runId

-- | Binds a function on a List.
--
-- >>> (\n -> n :. n :. Nil) =<< (1 :. 2 :. 3 :. Nil)
-- [1,1,2,2,3,3]
instance Bind List where
  (=<<) = flatMap

-- | Binds a function on an Optional.
--
-- >>> (\n -> Full (n + n)) =<< Full 7
-- Full 14
instance Bind Optional where
  (=<<) _  Empty   = Empty
  (=<<) f (Full x) = (f x)

-- | Binds a function on the reader ((->) t).
--
-- >>> ((*) =<< (+10)) 7
-- 119
instance Bind ((->) t) where
  f =<< g = \x -> f (g x) x

-- | Flattens a combined structure to a single structure.
--
-- >>> join ((1 :. 2 :. 3 :. Nil) :. (1 :. 2 :. Nil) :. Nil)
-- [1,2,3,1,2]
--
-- >>> join (Full Empty)
-- Empty
--
-- >>> join (Full (Full 7))
-- Full 7
--
-- >>> join (+) 7
-- 14
join ::
  Bind f =>
  f (f a)
  -> f a
join = (id =<<)

-- | Implement a flipped version of @(=<<)@, however, use only
-- @join@ and @(<$>)@.
-- Pronounced, bind flipped.
--
-- >>> ((+10) >>= (*)) 7
-- 119
--
-- >>> (Id 2) >>= (\x -> Id(x+1))
-- Id 3
(>>=) ::
  Bind f =>
  f a
  -> (a -> f b)
  -> f b
(>>=) fa a2fb = join (a2fb <$> fa)

infixl 1 >>=

tbf :: [T.Test]
tbf = U.tt "tbf"
      [        (Id 2) >>= (\x -> Id(x+1))
      ,             join ((\x -> Id(x+1)) <$> (Id 2))
      ,         (id =<<) ((\x -> Id(x+1)) <$> (Id 2))
      , (id . runId)     ((\x -> Id(x+1)) <$> (Id 2))
      , (id . runId) (Id ((\x -> Id(x+1))         2))
      , (id . runId) (Id (Id 3))
      ,  id              (Id 3)
      ]
      (Id 3)

-- | Implement composition within the @Bind@ environment.
-- Pronounced, kleisli composition.
--
-- >>> ((\n -> n :. n :. Nil) <=< (\n -> n+1 :. n+2 :. Nil)) 1
-- [2,2,3,3]
--
-- >>> (<=<) (\b -> Full (b+1)) (\a -> Full a) 99
-- Full 100
(<=<) ::
  Bind f =>
  (b -> f c)
  -> (a -> f b)
  -> a
  -> f c
(<=<) b2fc a2fb a = (a2fb a) >>= b2fc

infixr 1 <=<

-----------------------
-- SUPPORT LIBRARIES --
-----------------------

instance Bind IO where
  (=<<) =
    (P.=<<)

instance Bind [] where
  (=<<) =
    (P.=<<)

instance Bind P.Maybe where
  (=<<) =
    (P.=<<)

------------------------------------------------------------------------------

testBind :: IO T.Counts
testBind =
    T.runTestTT P.$ T.TestList P.$ tbf

-- End of file.
