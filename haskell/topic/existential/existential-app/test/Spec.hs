{-# LANGUAGE OverloadedStrings #-}

import           Lib        as L
import           Test.Hspec as S
import           TheSpec    as TS

main :: IO ()
main = S.hspec $ do
  describe "test" test

test :: S.Spec
test = do
  it "works" $
     {-
     run appSpec appState (Command (CommandEntry "hello"))
     `shouldBe` ("hello", CommandResult "world")
     -}
     do
       r <- TS.run appSpec appState (Command (CommandEntry "hello"))
       r `shouldBe` (AppState "hello", CommandResult "world")
