import           Test.Tasty
import           Test.Tasty.HUnit           hiding (assert)

main :: IO ()
main = do
  defaultMain $
    testGroup
      "op-monad"
      [
        testCase "left-pad" $ True @?= True
      ]

