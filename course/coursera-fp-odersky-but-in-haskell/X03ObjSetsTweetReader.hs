{-
Created       : 2013 Oct 01 (Tue) 21:00:47 by carr.
Last Modified : 2013 Oct 01 (Tue) 21:42:13 by carr.
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

main = do
    let toParse = C8.pack gizmodo
      in case (eitherDecode' toParse :: Either String Tweets) of
        Right r -> print r
        Left e -> print "E" -- e

-- End of file.
