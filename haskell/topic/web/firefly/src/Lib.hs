{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module Lib where

import           Control.Monad.Trans         (liftIO)
import           Data.Aeson                  (FromJSON, ToJSON)
import           Data.Maybe                  (fromMaybe)
import qualified Data.Text                   as T
import qualified Data.Text.IO                as TIO
import           GHC.Generics                (Generic)
import qualified Network.Wai                 as W
import qualified Text.Blaze.Html5            as H
import           Web.Firefly

main :: IO ()
main  = run 3000 (loggerMiddleware app)

app :: App ()
app  = do
  route "/hello"      helloHandler
  route "/goodbye/.*" goodbyeHandler
  route "/users"      getUser

-- | Get the 'name' query param from the url,
-- if it doesn't exist use 'Stranger'.
helloHandler :: Handler T.Text
helloHandler  = do
  name <- fromMaybe "Stranger" <$> getQuery "name"
  -- returning Text
  -- response status will be 200
  -- Content-Type    will be "text/plain"
  return $ "Hello " <> name

goodbyeHandler :: Handler H.Html
goodbyeHandler  = do
  pathInfo <- getPathInfo
  return . byeHtml $
    case pathInfo of
      [_, name] -> if name == "" then "Stranger" else name
      _         -> "Stranger"

byeHtml :: T.Text -> H.Html
byeHtml name = H.docTypeHtml $ do
  H.head $
    H.title "Goodbye!"
  H.body $ do
    H.h1 "Goodbye!"
    H.p ("Bye " >> H.b (H.toHtml name) >> " thanks for coming!")

data User = User
  { username :: T.Text
  , age      :: Int
  } deriving (Generic, ToJSON, FromJSON)

steve :: User
steve  = User{username="Steve", age=26}

getUser :: Handler W.Response
getUser  = do
  uname <- getQuery "username"
  return $ case uname of
    -- 'toResponse' converts differing response types
    -- to a common Wai Response.
    Just "steve" -> toResponse $ Json steve -- body only : status is 200
    Just name    -> toResponse ("Couldn't find user: " <> name, notFound404)
    Nothing      -> toResponse ("Please provide a 'username' parameter" :: T.Text, badRequest400)

-- | middleware before/after processing a request.
loggerMiddleware :: App () -> App ()
loggerMiddleware  = addMiddleware before after
 where
  before = do
    path   <- getPath
    method <- getMethod
    liftIO . TIO.putStrLn $ "INFO: " <> method <> " to " <> path
    -- Return the request to give to the app with.
    -- In this case no changes were made.
    waiRequest

  after resp = do
    liftIO $ print (W.responseStatus resp)
    -- Return the response with any changes.
    -- In this case no changes were made.
    return resp

