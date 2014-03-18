{-
Created       : 2014 Mar 03 (Mon) 20:39:50 by Harold Carr.
Last Modified : 2014 Mar 17 (Mon) 16:39:36 by Harold Carr.
-}

module BitlyClient where

import           BitlyClientCommon
import           BitlyClientRequests    as BCR
import           Control.Monad.IO.Class (MonadIO, liftIO)
import qualified Data.ByteString.Lazy   as L
import qualified Network.HTTP.Conduit   as C
import           System.IO

accessTokenFile :: String
accessTokenFile = ".access_token" -- single line, no newline

addAccessToken :: String -> IO String
addAccessToken x = do
    token <- withFile accessTokenFile ReadMode hGetLine
    return $ x ++ "&" ++ urlEncodeVars [("access_token", token)]

doRequest :: (MonadIO m) => Request -> m L.ByteString
doRequest r = do
    url <- liftIO (addAccessToken (BCR.makeRequestUrl r))
    C.simpleHttp url

-- End of file.
