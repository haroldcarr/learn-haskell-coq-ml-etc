module LibSpec where

------------------------------------------------------------------------------
import           Lib
------------------------------------------------------------------------------
import           Test.Hspec
------------------------------------------------------------------------------

spec :: Spec
spec  =
  it "zipWithStream" $
    headStream (tailStream (tailStream (zipWithStream (,)
                                        (numFrom (1::Int))
                                        (numFrom (1.0::Float)))))
    `shouldBe` (3, 3.0)
