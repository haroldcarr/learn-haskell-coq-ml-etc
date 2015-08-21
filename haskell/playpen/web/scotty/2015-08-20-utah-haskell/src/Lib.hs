{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module Lib
    ( scottyMain
    ) where

import           Data.Aeson   (FromJSON, ToJSON)
import           Data.Monoid  (mconcat)
import           GHC.Generics
import           Web.Scotty

data User = User { userId :: Int, userName :: String } deriving (Generic, Show)
bob       = User { userId = 1, userName = "bob" }
jenny     = User { userId = 2, userName = "jenny" }
allUsers  = [bob, jenny]

instance ToJSON User
instance FromJSON User

matchesId :: Int -> User -> Bool
matchesId id user = userId user == id

scottyMain :: IO ()
scottyMain = scotty 3000 $ do
    get "/users" $ do
        json allUsers
    get "/users/:id" $ do
        id <- param "id"
        json $ filter (matchesId id) allUsers
    post "/reg" reg
    get "/:word" $ do
        beam <- param "word"
        html $ mconcat ["<h1>Scotty, ", beam, " me up!</h1>"]

reg :: ActionM ()
reg = do
    e <- param "email" `rescue` (const next)
    html $ mconcat [ "ok ", e ]

{-
curl http://127.0.0.1:3000/users
curl -X POST http://127.0.0.1:3000/reg?email=foo
-}

