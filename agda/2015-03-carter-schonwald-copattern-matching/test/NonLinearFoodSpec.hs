module NonLinearFoodSpec where

------------------------------------------------------------------------------
import           NonLinearFood
------------------------------------------------------------------------------
import           Test.Hspec
------------------------------------------------------------------------------

spec :: Spec
spec  = do
  let initial  = mkEdible 100
      bite1    = takeABite 25 initial
      bite2    = takeABite 30 bite1
      bite3    = takeABite 40 bite2
      bite4    = takeABite 33 bite3
      bite5    = takeABite 66 bite4
      lastBite = finishIt     bite4
  it "initial"  $ whatsLeft initial `shouldBe`  100
  it "bite1"    $ whatsLeft bite1   `shouldBe`   75
  it "bite2"    $ whatsLeft bite2   `shouldBe`   45
  it "bite3"    $ whatsLeft bite3   `shouldBe`    5
  it "bite4"    $ whatsLeft bite4   `shouldBe`    0
  it "bite5"    $ whatsLeft bite5   `shouldBe`    0
  it "lastBite" $ lastBite          `shouldBe` Done
  it "eatItAll" $ eatItAll          `shouldBe` Done


