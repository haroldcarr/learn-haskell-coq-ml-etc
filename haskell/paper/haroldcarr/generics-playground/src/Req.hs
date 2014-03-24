data Request
  = Expand { shortUrl :: [String]
           , hash     :: [String]
           }
  | Shorten { longUrl :: String
            , domain  :: String
            }
  | LinkEdit { link     :: String
             , title    :: Maybe String
             , note     :: Maybe String
             , private  :: Maybe Bool
             , user_ts  :: Maybe Int
             , archived :: Maybe Bool
             , edit     :: [String]
             }
