{-# LANGUAGE OverloadedStrings #-}

module BlockchainIO where

import           Data.Thyme.Time

getDate :: IO String
getDate = do
  now    <- getCurrentTime
  myzone <- getCurrentTimeZone
  let x   = show (utcToZonedTime myzone now :: ZonedTime)
  return x
