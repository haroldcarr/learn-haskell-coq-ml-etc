{-# LANGUAGE DataKinds  #-}
{-# LANGUAGE LambdaCase #-}

module Main where

------------------------------------------------------------------------------
import qualified Example
import           Types
------------------------------------------------------------------------------
import           Data.Proxy
import           System.Environment
------------------------------------------------------------------------------

main :: IO ()
main  = getArgs >>= \case
  ("b" :_) -> Example.main (Proxy :: Proxy 'BlockingChannel)
  ("nb":_) -> Example.main (Proxy :: Proxy 'NonBlockingChannel)
  _        -> error "wrong args"
