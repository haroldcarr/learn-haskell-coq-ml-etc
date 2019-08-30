module HDPSpec where

import           Ch00_Tests
import           Test.Hspec

spec :: Spec
spec = do
  _ <- runIO main
  describe "HDP" $  it "main" $ True `shouldBe` True
