{-# LANGUAGE OverloadedStrings #-}

module Lib where

import           Data.Monoid
import qualified Network.Wai.Middleware.Cors          as Cors
import           Network.Wai.Middleware.RequestLogger
import           Prelude
import           Web.Scotty

main :: IO ()
main = scotty 3001 $ do
  -- Add any WAI middleware, they are run top-down.
  middleware $ Cors.cors (const $ Just (Cors.simpleCorsResourcePolicy
                                        { Cors.corsRequestHeaders =
                                            [ "Accept"
                                            , "Accept-Language"
                                            , "Content-Language"
                                            , "Content-Type"
                                            , "hx-current-url"
                                            , "hx-request"
                                            ] } ))
  middleware logStdoutDev

  -- Parameter in query string.
  -- If not been given, 500 page generated.
  get "/foo" $ do
      v <- param "fooparam"
      html $ mconcat ["<h1>", v, "</h1>"]

