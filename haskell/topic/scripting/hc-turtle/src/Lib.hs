module Lib where

import Turtle

datePwd :: IO UTCTime
datePwd  = do
  dir    <- pwd
  datefile dir

getTime :: IO ()
getTime  = do                   --
  t <- datePwd
  print t
