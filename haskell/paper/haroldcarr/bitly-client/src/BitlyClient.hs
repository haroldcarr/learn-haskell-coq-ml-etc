{-# LANGUAGE TemplateHaskell #-}


{-
Created       : 2014 Mar 03 (Mon) 20:39:50 by Harold Carr.
Last Modified : 2014 Mar 11 (Tue) 20:11:59 by Harold Carr.
-}

module BitlyClient where

import           BitlyClientCommon
import           BitlyClientTH
import           Control.Monad.IO.Class (MonadIO, liftIO)
import qualified Data.ByteString.Lazy   as L
import qualified Network.HTTP.Conduit   as C
import           System.IO

mk "Request" [ ("Expand" , ["shortUrl", "hash"], [])
             , ("Shorten", []                  , ["longUrl", "domain"])
             ]

accessTokenFile :: String
accessTokenFile = ".access_token" -- single line, no newline

addAccessToken :: String -> IO String
addAccessToken x = do
    token <- withFile accessTokenFile ReadMode hGetLine
    return $ x ++ "&" ++ urlEncodeVars [("access_token", token)]

doRequest :: (MonadIO m) => Request -> m L.ByteString
doRequest r = do
    url <- liftIO (addAccessToken (mkReqUrl r))
    C.simpleHttp url

{-
mkReqUrl :: Request -> String
mkReqUrl (RequestShorten l d) = mru "shorten" (zr "longUrl"  [l] ++ zr "domain" [d])
mkReqUrl (RequestShorten l d) = mru "shorten" ((++) (zr "longUrl"  [l]) (zr "domain" [d]))
mkReqUrl (RequestShorten l d) = mru "shorten" (concatMap id [("longUrl", "foo")])
-}
-- End of file.
