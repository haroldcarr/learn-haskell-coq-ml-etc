{-# LANGUAGE OverloadedStrings #-}

module XTurtle where

{-
Created       : 2015 Sep 03 (Thu) 09:03:52 by Harold Carr.
Last Modified : 2015 Sep 03 (Thu) 10:09:45 by Harold Carr.
-}

import           Turtle

datePwd = do
    dir <- pwd
    time <- datefile dir
    return time

t = do
    time <- datePwd
    print time
