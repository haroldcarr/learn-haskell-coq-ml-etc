{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module Lib
( scottyMain
)
where

import           Data.Aeson   (FromJSON, ToJSON)
import           Data.Monoid  (mconcat)
import           GHC.Generics
import           Web.Scotty

data User = User { userId :: Int, userName :: String } deriving (Eq, Generic, Show)
bob       = User 1 "bob"
jenny     = User 2 "jenny"
allUsers  = [bob, jenny]

instance ToJSON   User
instance FromJSON User

scottyMain :: IO ()
scottyMain = scotty 3000 $ do
    get "/users" $ do
        json allUsers
    get "/users/:id" $ do
        id <- param "id"
        json $ filter ((==id) . userId) allUsers
    post "/reg" $ do
        e <- param "email" `rescue` (const next)
        html $ mconcat [ "ok ", e ]
    get "/:word" $ do
        beam <- param "word"
        html $ mconcat ["<h1>Scotty, ", beam, " me up!</h1>"]

{-
curl http://127.0.0.1:3000/users
curl http://127.0.0.1:3000/users/1
curl -X POST http://127.0.0.1:3000/reg?email=foo
curl http://127.0.0.1:3000/JUNK
-}

