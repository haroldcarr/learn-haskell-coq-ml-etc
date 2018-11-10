{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Course.Comonad
(
 Comonad(..)
,testComonad
) where

import           Course.Core
import           Course.Extend
import           Course.Id

import qualified Prelude         as P
import qualified Test.HUnit      as T
import qualified Test.HUnit.Util as U

class Extend f => Comonad f where
  copure ::
    f a
    -> a

-- | Implement the @Comonad@ instance for @Id@.
--
-- >>> copure (Id 7)
-- 7
instance Comonad Id where
  copure = runId

-- | Witness that all things with (<<=) and copure also have (<$>).
--
-- >>> (+10) <$> Id 7
-- Id 17
(<$>) ::
  Comonad f =>
  (a -> b)
  -> f a
  -> f b
f <$> a = f . copure <<= a

t1 :: [T.Test]
t1 = U.tt "t1"
     [      (+10)          <$> Id 7
     ,      (+10) . copure <<= Id 7
     , Id (((+10) . copure)   (Id 7))
     , Id  ((+10)                 7)
     ]
     (Id 17)

------------------------------------------------------------------------------

testComonad :: IO T.Counts
testComonad =
    T.runTestTT P.$ T.TestList P.$ t1

-- End of file.


