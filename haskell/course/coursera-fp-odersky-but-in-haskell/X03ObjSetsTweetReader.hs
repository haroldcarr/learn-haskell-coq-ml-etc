{-
Created       : 2013 Oct 01 (Tue) 21:00:47 by carr.
Last Modified : 2013 Oct 02 (Wed) 21:55:30 by carr.
-}

{-# LANGUAGE OverloadedStrings #-}

module X03ObjSetsTweetReader where

import           Control.Applicative
import           Control.Monad
import           Data.Aeson
import qualified Data.ByteString.Lazy       as BL
import qualified Data.ByteString.Lazy.Char8 as C8
import           Data.Monoid
import           X03ObjSetsTweetSet

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

parseTweets x =
    let toParse = C8.pack x
      in case (eitherDecode' toParse :: Either String Tweets) of
        Right r -> return r
        Left e -> error (show e)

allTweets :: [Tweets] -> TweetSet
allTweets = foldr outerStep Empty
  where outerStep (Tweets tl) acc = foldr (flip incl) acc tl

-- End of file.
