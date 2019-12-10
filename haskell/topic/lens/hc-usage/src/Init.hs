{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Init where

import           Types
------------------------------------------------------------------------------
import qualified Data.Map.Strict as Map
import           Protolude

biT :: BlockInfo
biT  = BlockInfo (Author "biauthor") (Epoch 0) (Round 0) (HashValue "0")

epT :: EventProcessor ByteString
epT  = EventProcessor
        (BlockStore (BlockTree Map.empty (HashValue "btrootid")) "bsstuff")
        (Pacemaker (Round 100) (Round 101))
        (Just ( Vote
                  (VoteData biT biT)
                  (Author "epauthor")
              , Round 45))

