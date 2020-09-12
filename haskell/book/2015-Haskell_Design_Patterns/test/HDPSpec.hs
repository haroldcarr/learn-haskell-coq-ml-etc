module HDPSpec where

import           Ch00_Tests
import           Ch06_01_generics_sum_of_products
import           Ch06_02_generics_origami_recursion_schemes_fix
import           Ch06_03_generics_scrap_your_boilerplate
import           Test.Hspec

spec :: Spec
spec = do
  _ <- runIO main
  describe "HDP" $ do
    ch06_01_Test
    ch06_02_Test
    ch06_03_Test
