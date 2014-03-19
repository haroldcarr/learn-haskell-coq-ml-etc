{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

{-
Created       : 2014 Mar 03 (Mon) 20:39:50 by Harold Carr.
Last Modified : 2014 Mar 19 (Wed) 09:53:22 by Harold Carr.
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

data Response = ExpandResponse { long_url    :: [String] -- [URI]
                               , global_hash :: String
                               , short_url   :: [String] -- [URI]
                               , user_hash   :: String
                               -- , hash        :: [String]
                               -- , error       :: String
                               }
              | J String
              | N String
    deriving (Eq, Show)

instance FromJSON DataStatusCodeStatusTxt where
    parseJSON (Object o) = DSCST <$>
                               o .: "data" <*>
                               o .: "status_code" <*>
                               o .: "status_txt"
    parseJSON x = fail $ "FAIL: DataStatusCodeStatusTxt: " ++ (show x)

instance FromJSON ResponseData where
    parseJSON (Object o) =
        case M.lookup "expand" o of
            -- Just v  -> return $ ExpandResponseData [J ((show o) ++ " $$$ " ++ (show v))] -- parseJSON v -- ((o .: "data") >>= (.: "expand"))
            -- BEST YET - but doesn't parse arrays
            Just v  -> do { d <- (parseJSON v) :: Parser Response; return $ ExpandResponseData [d] }

            -- Right (Left "when expecting a [a], encountered String instead")
            -- Just _  -> do { d <- parseJSON =<< (o .: "expand"); return $ ExpandResponseData d}
            -- Just _  -> do { d <- parseJSON o; return $ ExpandResponseData d}

            Nothing -> return $ ExpandResponseData [N "N"]
    parseJSON x =  fail $ "FAIL: ResponseData: " ++ (show x)

instance FromJSON Response where
    parseJSON (Object o) = ExpandResponse         <$>
                               o .: "long_url"    <*>
                               o .: "global_hash" <*>
                               o .: "short_url"   <*>
                               o .: "user_hash"
                               -- o .: "hash"        <*>
                               -- o .: "error"       <*>
    parseJSON x =  fail $ "FAIL: Response: " ++ (show x)

parseResponse :: String -> Either String DataStatusCodeStatusTxt
parseResponse x = eitherDecode $ L.pack x

-- End of file.
