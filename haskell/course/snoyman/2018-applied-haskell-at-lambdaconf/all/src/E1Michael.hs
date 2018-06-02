{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wall -Werror #-}

module E1Michael where

import qualified Data.Text as T
import qualified Data.ByteString as B
import Data.Text.Encoding (decodeUtf8', encodeUtf8Builder)
import Data.ByteString.Builder (Builder)
import qualified Data.ByteString.Builder as BB
import Data.Map (Map)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import Data.Monoid ((<>))
import Control.Exception (throwIO)
import System.IO (stdout)

type Name = Text
type City = Text
type State = Text

data Row = Row
  { rowName :: !Name
  , rowCity :: !City
  , rowState :: !State
  }

parseRow :: Text -> Maybe Row
parseRow text =
  case T.splitOn "," text of
    [x, y, z] -> Just $ Row x y z
    _ -> Nothing

parseRows :: Text -> Maybe [Row]
parseRows = mapM parseRow . T.lines

type StateMap = Map State (Map City Int)

toStateMap :: [Row] -> StateMap
toStateMap =
  Map.unionsWith (Map.unionWith (+)) . map go
  where
    go row = Map.singleton (rowState row) (Map.singleton (rowCity row) 1)

toHtml :: StateMap -> Builder
toHtml sm =
  "<ul>" <>
  Map.foldMapWithKey perState sm <>
  "</ul>"
  where
    perState state cities =
      "<li>" <>
      encodeUtf8Builder state <>
      "<dl>" <>
      Map.foldMapWithKey perCity cities <>
      "</dl>" <>
      "</li>"

    perCity city count =
      "<dt>" <>
      encodeUtf8Builder city <>
      "</dt><dd>" <>
      BB.intDec count <>
      "</dd>"

main :: IO ()
main = do
  bs <- B.readFile "input.csv"
  case decodeUtf8' bs of
    Left e -> throwIO e
    Right text ->
      case parseRows text of
        Nothing -> error "parse failed"
        Just rows ->
          let sm = toStateMap rows
              html = toHtml sm
           in BB.hPutBuilder stdout html
