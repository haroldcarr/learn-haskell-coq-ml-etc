module XSpec where

import           HC
import           Lib
------------------------------------------------------------------------------
import           Test.Hspec

spec :: Spec
spec  = do
  Lib.test
  HC.test
