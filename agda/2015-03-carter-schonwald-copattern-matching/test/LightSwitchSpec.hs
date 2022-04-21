module LightSwitchSpec where

------------------------------------------------------------------------------
import           LightSwitch
------------------------------------------------------------------------------
import           Test.Hspec
------------------------------------------------------------------------------

spec :: Spec
spec  = do
  let initial = alternates True
      flip1   = switchFlip initial
      flip2   = switchFlip flip1
      flip3   = switchFlip flip2
  it "initial"      $ isLit initial `shouldBe`  True
  it "flip1"        $ isLit flip1   `shouldBe` False
  it "flip2"        $ isLit flip2   `shouldBe`  True
  it "flip3"        $ isLit flip3   `shouldBe` False
  it "currentState" $ currentState  `shouldBe` False


