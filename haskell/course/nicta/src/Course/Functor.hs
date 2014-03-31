{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Course.Functor where

import           Course.Core
import           Course.Id
import           Course.List
import           Course.Optional
import qualified Prelude         as P

import qualified Test.HUnit      as T
import qualified Test.HUnit.Util as U

class Functor f where
  -- Pronounced, eff-map.
  (<$>) ::
    (a -> b)
    -> f a
    -> f b

infixl 4 <$>

-- $setup
-- >>> :set -XOverloadedStrings
-- >>> import Course.Core
-- >>> import qualified Prelude as P(return, (>>))

-- | Maps a function on the Id functor.
--
-- >>> (+1) <$> Id 2
-- Id 3
instance Functor Id where
    (<$>) f (Id x) = Id (f x)
--C (<$>) = mapId

-- | Maps a function on the List functor.
--
-- >>> (+1) <$> Nil
-- []
--
-- >>> (+1) <$> (1 :. 2 :. 3 :. Nil)
-- [2,3,4]
instance Functor List where
  (<$>) = map

-- | Maps a function on the Optional functor.
--
-- >>> (+1) <$> Empty
-- Empty
--
-- >>> (+1) <$> Full 2
-- Full 3
instance Functor Optional where
    (<$>) _  Empty   = Empty
    (<$>) f (Full x) = Full (f x)
--C (<$>) = mapOptional

data HCEither a b = Bad a | Good b deriving Show

-- | Maps a function on the HCEither fuctor.
--
-- >>> (+1) <$> Bad "x"
-- Bad "x"
--
-- >>> (+1) <$> Good 2
-- Good 3
--
-- Note the partially applied type constructor.
instance Functor (HCEither a) where
  (<$>) _ (Bad  x) = Bad x
  (<$>) f (Good x) = Good (f x)

-- | Maps a function on the "reader" ((->) t) functor.
-- Note: a type constructor `t -> a` can be written `(->) t a`
-- (just like any "normal" infix function).
-- An `instance Functor` declaration requires a type constructor of one argument.
-- So this instance is given  `(->) t` : a partially applied type constructor.
--
-- (<$>) :: (a -> b) ->  f      a  ->  f      b
-- (<$>) :: (a -> b) -> ((-> t) a) -> ((-> t) b)
-- (<$>) :: (a -> b) -> (t  ->  a) -> (t  ->  b)
--
-- >>> ((+1) <$> (*2)) 8
-- 17
instance Functor ((->) t) where
    (<$>) f1 f2 = f1 . f2
--C (<$>) = (.)

-- | Anonymous map. Maps a constant value on a functor.
--
-- >>> 7 <$ [1,2,3]
-- [7,7,7]
--
-- prop> x <$ [a,b,c] == [x,x,x]
--
-- prop> x <$ Full q == Full x
(<$) ::
  Functor f =>
  a
  -> f b
  -> f a
a <$ fb = const a <$> fb

tcl :: [T.Test]
tcl = U.tt "tcl"
      [                             7      <$        (1:.2:.Nil)
      ,                       const 7      <$>       (1:.2:.Nil)
      , map                  (const 7)               (1:.2:.Nil)
      , foldRight (\x acc -> (const 7) x :. acc) Nil (1:.2:.Nil)
      ]
      (7:.7:.Nil)

tco :: [T.Test]
tco = U.tt "tco"
      [             7 <$  Full 2
      ,       const 7 <$> Full 2
      , Full (const 7 2)
      , Full        7
      ]
      (Full 7)

-- | Anonymous map producing unit value.
--
-- >>> void [1,2,3]
-- [(),(),()]
--
-- >>> void (Full 7)
-- Full ()
--
-- >>> void Empty
-- Empty
--
-- >>> void (+10) 5
-- ()
void ::
  Functor f =>
  f a
  -> f ()
void =
  error "todo"

-----------------------
-- SUPPORT LIBRARIES --
-----------------------

-- | Maps a function on an IO program.
--
-- >>> reverse <$> (putStr "hi" P.>> P.return ("abc" :: List Char))
-- hi"cba"
instance Functor IO where
  (<$>) =
    P.fmap

instance Functor [] where
  (<$>) =
    P.fmap

instance Functor P.Maybe where
  (<$>) =
    P.fmap

--------------------------------------------------

testFunctor :: IO T.Counts
testFunctor =
    T.runTestTT P.$ T.TestList P.$ tcl P.++ tco

-- End of file.
