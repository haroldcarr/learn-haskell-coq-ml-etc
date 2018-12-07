{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module Lib where

import Dhall

data User = User
  { homeDirectory  :: Text
  , privateKeyFile :: Text
  , publicKeyFile  :: Text
  } deriving (Eq, Generic, Show)
instance Interpret User

dmain :: IO ()
dmain = do
  x <- input auto "./config/top.dhall"
  print (x :: [User])

{-
:set -XOverloadedStrings
import Dhall
import Data.Text
x <- input auto "./config/makeUser.dhall" :: IO (Dhall.Text -> User)
-}

