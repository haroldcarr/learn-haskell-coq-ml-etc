{-# LANGUAGE Strict     #-}
{-# LANGUAGE StrictData #-}

module XSpec where

------------------------------------------------------------------------------
import           DataForTest
import           UseRWSIO
import           UseRWSTIO
import           UseRWST
------------------------------------------------------------------------------
import           Test.Hspec
------------------------------------------------------------------------------

spec :: Spec
spec = do
  mrwsi   <- runIO (runMonadRWSInts  False)
  rwstioi <- runIO (runRWSTIOInts    False)
  rwsti   <- runIO (runRWSTInts      False)
  mrwsd   <- runIO (runMonadRWSData  False)
  rwstd   <- runIO (runRWSTData      False)
  let a = 20000000
      w = [11000001,12000001,13000001,14000001,15000001,16000001,17000001,18000001,19000001,20000001
          , 1000001, 2000001, 3000001, 4000001, 5000001, 6000001, 7000001, 8000001, 9000001,10000001]
      s = RoundManager 200000010000100 (BlockStore 20000000 3000)
  it "runMonadRWSInts"  $ mrwsi   `shouldBe` (a, w, a)
  it "runRWSTIOInts"    $ rwstioi `shouldBe` (a, w, a)
  it "runRWSTInts"      $ rwsti   `shouldBe` (a, w, a)
  it "runMonadRWSData"  $ mrwsd   `shouldBe` (a, w, s)
  it "runRWSTData"      $ rwstd   `shouldBe` (a, w, s)

