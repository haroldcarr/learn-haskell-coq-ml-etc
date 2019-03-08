{-# LANGUAGE OverloadedStrings #-}

module TestSpec where

import           Lib
------------------------------------------------------------------------------
import           Dhall
import           Test.Hspec

spec :: Spec
spec = do
  x <- runIO (input auto "./config/top.dhall")
  it "[User]" $
    (x :: [User])
    `shouldBe`
    [ User { homeDirectory  = "/home/jenkins"
           , privateKeyFile = "/etc/jenkins/id_rsa"
           , publicKeyFile  = "/etc/jenkins/id_rsa.pub"
           }
    , User { homeDirectory  = "/home/alice"
           , privateKeyFile = "/home/alice/id_rsa"
           , publicKeyFile  = "/home/alice/id_rsa.pub"
           }
    , User { homeDirectory  = "/home/bob"
           , privateKeyFile = "/home/bob/id_rsa"
           , publicKeyFile  = "/home/bob/id_rsa.pub"
           }
    , User { homeDirectory  = "/home/carr"
           , privateKeyFile = "/home/carr/id_rsa"
           , publicKeyFile  = "/home/carr/id_rsa.pub"
           }
    ]

