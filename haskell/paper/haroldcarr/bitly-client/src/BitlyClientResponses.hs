{-
Created       : 2014 Mar 03 (Mon) 20:39:50 by Harold Carr.
Last Modified : 2014 Mar 20 (Thu) 09:17:38 by Harold Carr.
-}

{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module BitlyClientResponses where

import           Control.Applicative
import           Data.Aeson
import           Data.Aeson.Types
import qualified Data.ByteString.Lazy.Char8 as L (pack)

data DataStatusCodeStatusTxt =
    DSCST { ddata       :: ResponseData
          , status_code :: Integer
          , status_txt  :: String
          }
    deriving (Eq, Show)

data ResponseData
    = ExpandResponseData { expand :: [Response] }
    | InfoResponseData   { info :: [Response] }
    deriving (Eq, Show)

data Response = ExpandResponse { short_url   :: Maybe String -- [URI]
                               , long_url    :: Maybe String -- [URI]
                               , user_hash   :: Maybe String
                               , global_hash :: Maybe String
                               , hash        :: Maybe String
                               , eerror      :: Maybe String
                               }
              | InfoResponse { short_url   :: Maybe String
                             , hash        :: Maybe String
                             , user_hash   :: Maybe String
                             , global_hash :: Maybe String
                             , eerror      :: Maybe String
                             , title       :: Maybe String
                             , created_by  :: Maybe String
                             , created_at  :: Maybe String
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
        <|> InfoResponseData   <$> o .: "info"

instance FromJSON Response where
    parseJSON v =     expandParse v
                  <|> infoParse   v

expandParse :: Value -> Parser Response
expandParse = withObject "expand" (\o -> ExpandResponse <$> o .:? "short_url"
                                                        <*> o .:? "long_url"
                                                        <*> o .:? "user_hash"
                                                        <*> o .:? "global_hash"
                                                        <*> o .:? "hash"
                                                        <*> o .:? "error")

infoParse :: Value -> Parser Response
infoParse = withObject "info" (\o ->  InfoResponse <$> o .:? "short_url"
                                                   <*> o .:? "hash"
                                                   <*> o .:? "user_hash"
                                                   <*> o .:? "global_hash"
                                                   <*> o .:? "error"
                                                   <*> o .:? "title"
                                                   <*> o .:? "created_by"
                                                   <*> o .:? "created_at")

parseResponse :: String -> Either String DataStatusCodeStatusTxt
parseResponse x = eitherDecode $ L.pack x

-- End of file.
