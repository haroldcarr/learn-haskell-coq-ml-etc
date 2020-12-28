module XSpec where

------------------------------------------------------------------------------
import           L1
------------------------------------------------------------------------------
import           Prelude    hiding (replicate)
import           Test.Hspec
------------------------------------------------------------------------------

spec :: Spec
spec  =
  l1

l1 :: Spec
l1  = do
  reflection
  reification
  equality
  mkvec
  replicate
  withvec
  getthird
