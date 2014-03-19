{-
Created       : 2014 Mar 03 (Mon) 20:39:50 by Harold Carr.
Last Modified : 2014 Mar 19 (Wed) 16:10:54 by Harold Carr.
-}

{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module BitlyClientResponses where

import           Control.Applicative
import           Data.Aeson
import qualified Data.ByteString.Lazy.Char8 as L (pack)

data DataStatusCodeStatusTxt =
    DSCST { ddata       :: ResponseData
          , status_code :: Integer
          , status_txt  :: String
          }
    deriving (Eq, Show)

data ResponseData
  = ExpandResponseData { expand :: [Response]
                       }
    deriving (Eq, Show)

data Response = ExpandResponse { short_url   :: Maybe String -- [URI]
                               , long_url    :: Maybe String -- [URI]
                               , user_hash   :: Maybe String
                               , global_hash :: Maybe String
                               , hash        :: Maybe String
                               , error       :: Maybe String
                               }
              | J String -- for development/debugging
              | N String -- for development/debugging
    deriving (Eq, Show)

instance FromJSON DataStatusCodeStatusTxt where
    parseJSON = withObject "DataStatusCodeStatusTxt" $ \o ->
        DSCST <$> o .: "data"
              <*> o .: "status_code"
              <*> o .: "status_txt"

instance FromJSON ResponseData where
    parseJSON = withObject "ResponseData" $ \o ->
        ExpandResponseData <$> o .: "expand"

instance FromJSON Response where
    parseJSON = withObject "Response" $ \o ->
        ExpandResponse <$> o .:? "short_url"
                       <*> o .:? "long_url"
                       <*> o .:? "user_hash"
                       <*> o .:? "global_hash"
                       <*> o .:? "hash"
                       <*> o .:? "error"

parseResponse :: String -> Either String DataStatusCodeStatusTxt
parseResponse x = eitherDecode $ L.pack x

-- End of file.
