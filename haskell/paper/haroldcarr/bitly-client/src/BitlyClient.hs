{-# LANGUAGE OverloadedStrings #-}

{-
Created       : 2014 Mar 03 (Mon) 20:39:50 by Harold Carr.
Last Modified : 2014 Mar 10 (Mon) 00:06:25 by Harold Carr.
-}

module BitlyClient where

import           Control.Monad.IO.Class (MonadIO, liftIO)
import qualified Data.ByteString.Lazy   as L
import           Data.List              (intercalate)
import qualified Network.HTTP           as H (urlEncode)
import qualified Network.HTTP.Conduit   as C
import           System.IO

bitlyApiV3 :: String
bitlyApiV3  = "https://api-ssl.bitly.com/v3/"

accessTokenFile :: String
accessTokenFile = ".access_token" -- single line, no newline

data Request = ExpandRequest { shortUrl :: [String]
                             , hash     :: [String]
                             }
               | ShortenRequest { longUrl :: String
                                , domain  :: String
                                }

mkReqUrl :: Request -> String
mkReqUrl (ExpandRequest  s h) = mru "expand"  (zr "shortUrl"  s  ++ zr "hash"    h)
mkReqUrl (ShortenRequest l d) = mru "shorten" (zr "longUrl"  [l] ++ zr "domain" [d])

zr :: String -> [a] -> [(String,a)]
zr = zip . repeat

mru :: String -> [(String,String)] -> String
mru op p = bitlyApiV3 ++ op ++ "?" ++ (urlEncodeVars p)

addAccessToken :: String -> IO String
addAccessToken x = do
    token <- withFile accessTokenFile ReadMode hGetLine
    return $ x ++ "&" ++ urlEncodeVars [("access_token", token)]

doRequest :: (MonadIO m) => Request -> m L.ByteString
doRequest r = do
    url <- liftIO (addAccessToken (mkReqUrl r))
    C.simpleHttp url

-- the version in Network.HTTP does not do the right thing
urlEncodeVars :: [(String,String)] -> String
urlEncodeVars = intercalate "&" . uev
  where
    uev ((n,v):t) = (H.urlEncode n ++ "=" ++ H.urlEncode v) : uev t
    uev        [] = []

{-
mk "Request" [ ("expand" , ["shortUrl", "hash"], [])
             , ("shorten", []                  , ["longUrl", "domain"])
             ]
-}

-- End of file.
