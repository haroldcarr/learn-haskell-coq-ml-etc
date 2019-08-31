{-
Created       : 2014 Mar 16 (Sun) 12:23:18 by Harold Carr.
Last Modified : 2014 Mar 16 (Sun) 12:41:23 by Harold Carr.
-}

{-# LANGUAGE DeriveDataTypeable #-}

module Uni2 where

import           Data.Data
import           Data.Generics.Uniplate.Data
import           Data.Typeable

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
    deriving (Data, Show, Typeable)

{-
:t typeOf (Shorten "a" "b")
=> typeOf (Shorten "a" "b") :: TypeRep
-}

-- End of file.
