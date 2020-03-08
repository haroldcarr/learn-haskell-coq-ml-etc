module TestSpec where

------------------------------------------------------------------------------
import           HC
------------------------------------------------------------------------------
import           Test.Hspec
import           Test.Hspec.Contrib.HUnit
import           Test.HUnit               (Test (TestList))
------------------------------------------------------------------------------

spec :: Spec
spec  =
  fromHUnitTest $ TestList $
  txsum ++ tv3 ++ tvr3 ++ tva ++ tvfffvabc ++
  gnpi ++ ghl ++ gchar ++ g2 ++ exj ++ hpn ++ hpk ++ hpt ++ hmi ++ htos ++ htos' ++ sgnpm ++
  hcp1 ++ hcp2t

