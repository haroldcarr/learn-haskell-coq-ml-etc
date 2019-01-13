{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module XUtil where

import qualified Data.Text as T
import qualified Prelude
import           Protolude

fields :: [Text] -> Text
fields as = T.intercalate "; " (map toS as)

pshow :: (Show a, StringConv Prelude.String b) => a -> b
pshow  = toS . Prelude.show
