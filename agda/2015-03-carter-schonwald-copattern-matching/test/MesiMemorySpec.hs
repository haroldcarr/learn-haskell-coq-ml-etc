module MesiMemorySpec where

------------------------------------------------------------------------------
import           MesiMemory
------------------------------------------------------------------------------
import           Test.Hspec
------------------------------------------------------------------------------

spec :: Spec
spec  = do
  let initial   = mkMesiCommandI Invalid
      invalid   = currentState initial
      exclusive = currentState (localReadI initial)
      shared    = currentState (remoteReadI (localReadI initial))
      modified  = currentState (localWriteI initial)
  it "invalid"   $ invalid   `shouldBe` Invalid
  it "exclusive" $ exclusive `shouldBe` Exclusive
  it "shared"    $ shared    `shouldBe` Shared
  it "modified"  $ modified  `shouldBe` Modified





