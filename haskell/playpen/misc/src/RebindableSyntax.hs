{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}

{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE RebindableSyntax          #-}

module RebindableSyntax where

import           Prelude       hiding (return, (>>), (>>=))

concatNums :: String
concatNums = do
  "80"
  "60"
  "10"
 where
  (>>) = (++)
  return x = x

addNums :: Num a => a
addNums = do
  80
  60
  10
 where
  (>>) = (+)
  return x = x
