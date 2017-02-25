{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE RebindableSyntax          #-}

module RebindableSyntax where

import           Control.Monad ((<=<))
import           Data.Monoid
import           Prelude       hiding (return, (>>), (>>=))

concatNums = do
  "80"
  "60"
  "10"
 where
  (>>) = (++)
  return = \x -> x

addNums = do
  80
  60
  10
 where
  (>>) = (+)
  return = \x -> x
