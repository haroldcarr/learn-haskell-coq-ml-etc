module Lib2Spec where

------------------------------------------------------------------------------
import           Lib2
------------------------------------------------------------------------------
import           Test.Hspec
------------------------------------------------------------------------------

spec :: Spec
spec  = do
  it "cycle" $
    Lib2.head (Lib2.tail (Lib2.tail (Lib2.tail (Lib2.cycle [1,2,3::Int]))))
    `shouldBe` 1
  it "numFrom" $
    Lib2.head (Lib2.tail (Lib2.tail (Lib2.tail (Lib2.numFrom (1::Int)))))
    `shouldBe` 4
