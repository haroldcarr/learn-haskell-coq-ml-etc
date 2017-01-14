{-
Created       : 2014 May 16 (Fri) 21:34:33 by Harold Carr.
Last Modified : 2016 Feb 09 (Tue) 14:35:40 by Harold Carr.
http://doingmyprogramming.com/2014/04/14/yet-another-aeson-tutorial/
-}

{-# LANGUAGE OverloadedStrings #-}

module AesonTut where

import           Control.Applicative
import           Data.Aeson
import           Data.Aeson.Types
import qualified Data.ByteString.Lazy.Char8 as L (pack)

m :: String
m = "{\
  \ \"error\": {\
      \ \"message\"      : \"Message describing the error\",\
      \ \"type\"         : \"OAuthException\",\
      \ \"code\"         : 190 ,\
      \ \"error_subcode\": 460\
    \}\
\}"

------------------------------------------------------------------------------

data EXTopLevel1 = EXTopLevel1 { getEx1 :: FBException1 } deriving (Show)

data FBException1 = FBException1 { exMsg1     :: String
                                 , exType1    :: String
                                 , exCode1    :: Int
                                 , exSubCode1 :: Maybe Int
                                 } deriving (Show)

instance FromJSON EXTopLevel1 where
    parseJSON (Object v) = EXTopLevel1
                           <$> v .: "error"

instance FromJSON FBException1 where
    parseJSON (Object v) = FBException1
                           <$> v .:  "message"
                           <*> v .:  "type"
                           <*> v .:  "code"
                           <*> v .:? "error_subcode"

p1 :: String -> Either String EXTopLevel1
p1 = eitherDecode .  L.pack

-- p1 m

------------------------------------------------------------------------------

data FBException2 = FBException2 { exMsg2     :: String
                                 , exType2    :: String
                                 , exCode2    :: Int
                                 , exSubCode2 :: Maybe Int
                                 } deriving (Show)

instance FromJSON FBException2 where
    parseJSON (Object v) = FBException2
                           <$> (e >>= (.: "message"))
                           <*> (e >>= (.: "type"))
                           <*> (e >>= (.: "code"))
                           <*> (e >>= (.:? "error_subcode"))
      where e = (v .: "error")

p2 :: String -> Either String FBException2
p2 = eitherDecode .  L.pack

-- p2 m

------------------------------------------------------------------------------

m3 = "{\
  \ \"value\": \"EVERYONE\"\
\}"

m3' = "{\
  \ \"value\": \"FRIENDS_OF_FRIENDS\"\
\}"

m3'' = "{\
  \ \"value\": \"NSA\"\
\}"

data Privacy3 = Everyone3
              | AllFriends3
              | FriendsOfFriends3
              | Self3
              deriving (Show)

instance FromJSON Privacy3 where
    parseJSON (Object v) = createPrivacy3 (v .: "value")

createPrivacy3 :: Parser String -> Parser Privacy3
createPrivacy3 x =
    x >>= \c -> case c of
                    "EVERYONE"           -> return Everyone3
                    "ALL_FRIENDS"        -> return AllFriends3
                    "FRIENDS_OF_FRIENDS" -> return FriendsOfFriends3
                    "SELF"               -> return Self3
                    _                    -> error "Invalid privacy setting!"

p3 :: String -> Either String Privacy3
p3 = eitherDecode .  L.pack

-- p3 m3
-- p3 m3'
-- p3 m3''


-- End of file.
