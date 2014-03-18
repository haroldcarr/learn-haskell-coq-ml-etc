{-# LANGUAGE TemplateHaskell #-}

{-
Created       : 2014 Mar 03 (Mon) 20:39:50 by Harold Carr.
Last Modified : 2014 Mar 17 (Mon) 16:38:30 by Harold Carr.
-}

module BitlyClientRequests
       ( Request
       , makeRequestUrl
       , mkExpandRequest
       , mkShortenRequest
       , mkLinkEditRequest
       )
       where

import           BitlyClientTH as BCTH

data Request
  = ExpandRequest { shortUrl :: [String] -- [URI]
                  , hash     :: [String]
                  }
  | ShortenRequest { longUrl :: String
                   , domain  :: String
                   }
  | LinkEditRequest { link     :: String
                    , title    :: Maybe String
                    , note     :: Maybe String
                    , private  :: Maybe Bool
                    , user_ts  :: Maybe Int
                    , archived :: Maybe Bool
                    , edit     :: [String]
                    }
  deriving (Eq, Show)

BCTH.mk ''Request

mkExpandRequest :: [String] -> [String] -> Request
mkExpandRequest shortUrl hash = ExpandRequest shortUrl hash

mkShortenRequest :: String -> String -> Request
mkShortenRequest longUrl  domain = ShortenRequest longUrl domain

mkLinkEditRequest :: String -> Maybe String -> Maybe String -> Maybe Bool -> Maybe Int -> Maybe Bool -> [String] -> Request
mkLinkEditRequest link title note private user_ts archived edit = LinkEditRequest link title note private user_ts archived edit

-- End of file.
