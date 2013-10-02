{-
Created       : 2013 Oct 01 (Tue) 21:00:47 by carr.
Last Modified : 2013 Oct 02 (Wed) 07:37:05 by carr.
-}

{-# LANGUAGE OverloadedStrings #-}

module X03ObjSetsTweetReader where

import qualified Data.ByteString.Lazy       as BL
import qualified Data.ByteString.Lazy.Char8 as C8
import Data.Aeson
import Data.Monoid
import Control.Applicative
import Control.Monad
import X03ObjSetsTweetData
import X03ObjSetsTweetSet

data Tweets = Tweets
    { tweets :: [Tweet]
    } deriving Show

instance FromJSON Tweets where
    parseJSON (Object t) = Tweets <$>
                           t .: "tweets"
    parseJSON _ = mzero

instance FromJSON Tweet where
    parseJSON (Object t) = Tweet <$>
                           t .: "user" <*>
                           t .: "text" <*>
                           t .: "retweets"
    parseJSON _ = mzero

parseTweets :: [Char] -> IO ()
parseTweets x =
    let toParse = C8.pack x
      in case (eitherDecode' toParse :: Either String Tweets) of
        Right r -> print r
        Left e -> print e

main = do
    parseTweets gizmodo
    parseTweets techcrunch
    parseTweets engadget
    parseTweets amazondeals
    parseTweets cnet
    parseTweets gadgetlab
    parseTweets mashable

-- End of file.
