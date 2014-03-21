{-
Created       : 2014 Mar 03 (Mon) 20:39:50 by Harold Carr.
Last Modified : 2014 Mar 21 (Fri) 16:33:51 by Harold Carr.
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
    = ExpandResponseData     { expand :: [Response] }
    | InfoResponseData       { info :: [Response] }
    | LinkLookupResponseData { link_lookup :: [Response] }
    | ShortenResponseData    { shorten :: Response }
    deriving (Eq, Show)

data Response = ExpandResponse { er_short_url   :: Maybe String -- URI
                               , er_long_url    :: String -- URI
                               , er_user_hash   :: String
                               , er_global_hash :: String
                               , er_hash        :: Maybe String
                               , er_error       :: Maybe String
                               }
              | InfoResponse { ir_created_by  :: String
                             , ir_created_at  :: Integer
                             , ir_short_url   :: Maybe String
                             , ir_hash        :: Maybe String
                             , ir_user_hash   :: Maybe String
                             , ir_global_hash :: Maybe String
                             , ir_error       :: Maybe String
                             , ir_title       :: Maybe String
                             }
              | LinkLookupResponse { llr_aggregate_link :: String
                                   , llr_url            :: String
                                     }
              | ShortenResponse { sr_new_hash    :: Integer
                                , sr_url         :: String
                                , sr_hash        :: String
                                , sr_global_hash :: String
                                , sr_long_url    :: String
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
            ExpandResponseData     <$> o .: "expand"
        <|> InfoResponseData       <$> o .: "info"
        <|> LinkLookupResponseData <$> o .: "link_lookup"
--        <|> ShortenResponseData

instance FromJSON Response where
    parseJSON v =     expandParse     v
                  <|> infoParse       v
                  <|> linkLookupParse v
                  <|> shortenParse    v

expandParse :: Value -> Parser Response
expandParse = withObject "expandParse" (\o -> ExpandResponse <$> o .:? "short_url"
                                                             <*> o .:  "long_url"
                                                             <*> o .:  "user_hash"
                                                             <*> o .:  "global_hash"
                                                             <*> o .:? "hash"
                                                             <*> o .:? "error")

infoParse :: Value -> Parser Response
infoParse = withObject "infoParse" (\o ->  InfoResponse <$> o .:  "created_by"
                                                        <*> o .:  "created_at"
                                                        <*> o .:? "short_url"
                                                        <*> o .:? "hash"
                                                        <*> o .:? "user_hash"
                                                        <*> o .:? "global_hash"
                                                        <*> o .:? "error"
                                                        <*> o .:? "title")

linkLookupParse :: Value -> Parser Response
linkLookupParse = withObject "infoParse" (\o ->  LinkLookupResponse <$> o .: "aggregate_link"
                                                                    <*> o .: "url")

shortenParse :: Value -> Parser Response
shortenParse = withObject "shortenParse" (\o -> ShortenResponse <$> o .: "new_hash"
                                                                <*> o .: "url"
                                                                <*> o .: "hash"
                                                                <*> o .: "global_hash"
                                                                <*> o .: "long_url")

parseResponse :: String -> Either String DataStatusCodeStatusTxt
parseResponse x = eitherDecode $ L.pack x

-- End of file.
