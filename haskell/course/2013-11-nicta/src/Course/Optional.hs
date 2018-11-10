{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Course.Optional where

import           Course.Core
import qualified Prelude         as P

import qualified Test.HUnit      as T
import qualified Test.HUnit.Util as U

--  class Optional<A> {
--    Optional(A a) {} // Full
--    Optional() {} // Empty
--  }
data Optional a = Full a | Empty deriving (Eq, Show)

-- | HC: this is `lift1`
mapOptional :: (a -> b) -> Optional a -> Optional b
mapOptional _ Empty    = Empty
mapOptional f (Full a) = Full (f a)

bindOptional :: (a -> Optional b) -> Optional a -> Optional b
bindOptional _ Empty    = Empty
bindOptional f (Full a) = f a

(??) :: Optional a -> a -> a
Empty ?? d  = d
Full a ?? _ = a

(<+>) :: Optional a -> Optional a -> Optional a
Empty <+> o = o
k <+> _     = k

applyOptional :: Optional (a -> b) -> Optional a -> Optional b
applyOptional f a = bindOptional (\f' -> mapOptional (\a' -> f' a') a) f

-- | HC: this is `lift2`
twiceOptional :: (a -> b -> c) -> Optional a -> Optional b -> Optional c
twiceOptional f = applyOptional . mapOptional f

-- Note: this reasoning is from an older but similar definition of twiceOptional
tto :: [T.Test]
tto = U.tt "tto"
      [ twiceOptional (+) (Full 2) (Full (3::Int))
      , bindOptional (\aa -> mapOptional ((+) aa) (Full 3)) (Full 2)
      ,              (\aa -> mapOptional ((+) aa) (Full 3))       2
      ,                      mapOptional ((+)  2) (Full 3)
      ,                             Full(((+)  2)       3)
      ]
      (Full 5)

-- |
-- >>> contains   3 (Full 3)
-- True
--
-- >>> contains   3 Empty
-- False
--
-- >>> containsHC 3 (Full 3)
-- True
--
-- >>> containsHC 3 Empty
-- False
--
contains :: Eq a => a -> Optional a -> Bool
contains _ Empty = False
contains a (Full z) = a == z

containsHC :: Eq a => a -> Optional a -> Bool
containsHC a o = case mapOptional (a ==) o of
                     Empty    -> False
                     (Full x) -> x

instance P.Monad Optional where
  (>>=) =
    flip bindOptional
  return =
    Full

------------------------------------------------------------------------------

testOptional :: IO T.Counts
testOptional =
   T.runTestTT P.$ T.TestList P.$ tto

-- End of file.

