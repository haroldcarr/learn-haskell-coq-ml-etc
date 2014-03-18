{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

{-
Created       : 2014 Mar 03 (Mon) 20:39:50 by Harold Carr.
Last Modified : 2014 Mar 18 (Tue) 15:33:08 by Harold Carr.
-}

module BitlyClientResponses where

import           Control.Applicative
import           Control.Monad
import           Data.Aeson
import           Data.Aeson.Types
import qualified Data.ByteString.Lazy.Char8 as L (pack)
import qualified Data.HashMap.Strict        as M
import           Data.Text
import qualified Data.Text                  as T

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

data Response = ExpandResponse { short_url   :: [String] -- [URI]
                               , hash        :: [String]
                               , user_hash   :: String
                               , global_hash :: String
                               , error       :: String
                               , long_url    :: [String] -- [URI]
                               }
              | J String
              | N String
    deriving (Eq, Show)

instance FromJSON DataStatusCodeStatusTxt where
    parseJSON (Object o) = do
        dat <- parseJSON =<< (o .: "data")
        sc  <- (o .: "status_code")
        st  <- (o .: "status_txt")
        return $ DSCST dat sc st
    parseJSON _ = mzero

toResponseData :: Value -> Parser ResponseData
toResponseData (Object o) = do
  return $
    case M.lookup "expand" o of
        Just e  -> parseExpand e -- ExpandResponseData [J "J"]
        Nothing -> ExpandResponseData [N "N"]
toResponseData _ = Control.Applicative.empty

instance FromJSON ResponseData where
    parseJSON = toResponseData

parseExpand :: Value -> ResponseData
parseExpand v = ExpandResponseData [J (show v)]

{-
toResponse     :: Value -> Parser Response
toResponse (Object o) = ExpandResponse <$> o .: ([]::String) <*> o .: ([]::String) "" "" "" ([]::String)
toResponse _ = Control.Applicative.empty

instance FromJSON Response where
    parseJSON = toResponse
-}

parseResponse :: String -> Maybe DataStatusCodeStatusTxt
parseResponse x = decode $ L.pack x

-- End of file.
