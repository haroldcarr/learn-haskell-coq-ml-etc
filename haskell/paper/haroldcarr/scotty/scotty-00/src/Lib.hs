{-# LANGUAGE OverloadedStrings #-}

module Lib
    ( scottyMain
    ) where

import Web.Scotty
import Data.Monoid (mconcat)

scottyMain :: IO ()
scottyMain = scotty 3000 $ do
    get "/:word" $ do
        beam <- param "word"
        html $ mconcat ["<h1>Scotty, ", beam, " me up!</h1>"]
