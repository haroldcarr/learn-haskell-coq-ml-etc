module HDPSpec where

import           Ch00_Tests
import           Ch06_01_generics_sum_of_products
import           Test.Hspec

spec :: Spec
spec = do
  _ <- runIO main
  describe "HDP"
    ch06_01_Test
