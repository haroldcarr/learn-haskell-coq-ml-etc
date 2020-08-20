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
  ("b" :i:_) -> Example.main (Proxy :: Proxy 'BlockingChannel)    (read i)
  ("nb":i:_) -> Example.main (Proxy :: Proxy 'NonBlockingChannel) (read i)
  _          -> Example.main (Proxy :: Proxy 'BlockingChannel)    25000
