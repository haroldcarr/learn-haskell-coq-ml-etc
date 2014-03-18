{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

{-
Created       : 2014 Mar 03 (Mon) 20:39:50 by Harold Carr.
Last Modified : 2014 Mar 18 (Tue) 16:03:10 by Harold Carr.
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

instance FromJSON ResponseData where
    parseJSON (Object o) =
      return $
        case M.lookup "expand" o of
            Just e  -> ExpandResponseData [J (show e)]
            Nothing -> ExpandResponseData [N "N"]
    parseJSON _ = mzero

parseResponse :: String -> Maybe DataStatusCodeStatusTxt
parseResponse x = decode $ L.pack x

-- End of file.
