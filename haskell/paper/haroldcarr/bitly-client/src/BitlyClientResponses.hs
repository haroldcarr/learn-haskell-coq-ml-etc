{-# LANGUAGE OverloadedStrings #-}

{-
Created       : 2014 Mar 03 (Mon) 20:39:50 by Harold Carr.
Last Modified : 2014 Mar 17 (Mon) 20:13:58 by Harold Carr.
-}

module BitlyClientResponses where

import           Control.Applicative
import           Control.Monad
import           Data.Aeson
import qualified Data.ByteString.Lazy as B
import           GHC.Generics
import           Network.HTTP.Conduit (simpleHttp)

data ExpandResponseWrapper
  = ExpandResponseWrapper { status_code :: Int
                          , status_txt  :: String
                          }

data Response
  = ExpandResponse { short_url   :: [String] -- [URI]
                   , hash        :: [String]
                   , user_hash   :: String
                   , global_hash :: String
                   , error       :: String
                   , long_url    :: [String] -- [URI]
                   }
    deriving (Show)

instance FromJSON ExpandResponse
instance ToJSON ExpandResponse

-- End of file.
