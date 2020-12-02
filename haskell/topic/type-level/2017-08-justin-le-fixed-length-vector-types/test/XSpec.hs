module XSpec where

------------------------------------------------------------------------------
import           Lib
------------------------------------------------------------------------------
import           Prelude    hiding (replicate)
import           Test.Hspec
------------------------------------------------------------------------------

spec :: Spec
spec  = do
  reflection
  reification
  equality
  mkvec
  replicate
  withvec
  getthird
