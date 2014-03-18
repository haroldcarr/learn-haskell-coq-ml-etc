{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

{-
Created       : 2014 Mar 03 (Mon) 20:39:50 by Harold Carr.
Last Modified : 2014 Mar 18 (Tue) 14:47:25 by Harold Carr.
-}

module BitlyClientResponses where

import           Control.Applicative
import           Data.Aeson
import           Data.Aeson.Types
import qualified Data.ByteString.Lazy.Char8 as L (pack)
import           Data.Text

data DataStatusCodeStatusTxt =
    DSCST { ddata       :: [ResponseData] -- TODO: not really an array
          , status_code :: Integer
          , status_txt  :: String
          }
    deriving (Eq, Show)

data ResponseData
  = ExpandResponseData { expand :: [Response]
                       }
    deriving (Eq, Show)

data Response =
    ExpandResponse { short_url   :: [String] -- [URI]
                   , hash        :: [String]
                   , user_hash   :: String
                   , global_hash :: String
                   , error       :: String
                   , long_url    :: [String] -- [URI]
                   }
    deriving (Eq, Show)

toDSCST        :: Value -> Parser DataStatusCodeStatusTxt
toDSCST (Object o) = DSCST [] <$> {- o .: "data" <*> -} o .: "status_code" <*> o .: "status_txt"
toDSCST _ = Control.Applicative.empty

instance FromJSON DataStatusCodeStatusTxt where
    parseJSON = toDSCST
{-
toResponseData :: Value -> Parser ResponseData
toResponseData (Object o) = ExpandResponseData <$> o .: "expand"
toResponseData _ = Control.Applicative.empty

instance FromJSON ResponseData where
    parseJSON = toResponseData

toResponse     :: Value -> Parser Response
toResponse (Object o) = ExpandResponse <$> o .: ([]::String) <*> o .: ([]::String) "" "" "" ([]::String)
toResponse _ = Control.Applicative.empty

instance FromJSON Response where
    parseJSON = toResponse
-}
parseResponse :: String -> Maybe DataStatusCodeStatusTxt
parseResponse x = decode $ L.pack x

-- End of file.
