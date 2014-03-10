-- mkOptional then generates a new data type with all Names in that module with an added suffix _opt. Here is an example of its use:

{-# LANGUAGE TemplateHaskell #-}

module TT where

import           BitlyClientTH

mk "Request" [ ("Expand" , ["shortUrl", "hash"], [])
             , ("Shorten", []                  , ["longUrl", "domain"])
             ]

-- End of file.
