{-# LANGUAGE OverloadedStrings #-}

module Lib where

import           Control.Lens                ((&), (.~), (^.))
import           Data.Aeson.Lens             (_String)
import           Data.List                   as L
import           Data.Text                   as T
import           Data.Text.IO                as T
import           Data.Thyme.Time
import           Network.HTTP.Client.OpenSSL
import           Network.Wreq                as W
import           OpenSSL.Session             (context)
import           Prelude                     as P
import           Text.HTML.TagSoup

------------------------------------------------------------------------------

baseUrl = "https://www.zillow.com"

page1Url =
  baseUrl ++ "/homes/for_sale/84103_rb/?fromHomePage=true&shouldFireSellPageImplicitClaimGA=false&fromHomePageTab=buy"

dataDir = "data/"

------------------------------------------------------------------------------

downloadAll = do
  filename    <- download page1Url 1
  -- let filename = P.concat [dataDir, "2017-02-13", "-p", show 1, ".htm"]
  txt         <- T.readFile filename
  let links    = pageLinks txt
  filenames   <- mapM (\(pagelink,n) -> download (baseUrl ++ (T.unpack pagelink)) n)
                      (P.zip links [2 ..])
  return (filename : filenames)

download url n = do
  date        <- getDate
  src         <- openUrl url
  let filename = P.concat [dataDir, date, "-p", show n, ".htm"]
  P.writeFile filename src
  return filename

openUrl url =
  withOpenSSL $ do
    r <- getWith opts url
    print (r ^. responseStatus)
    print (r ^. responseHeaders)
    return (T.unpack (r ^. responseBody . _String))
 where
  opts = defaults & manager .~ Left (opensslManagerSettings context)

-- printTags =<< allListings ["data/2017-02-13-p1.htm", "data/2017-02-13-p2.htm", "data/2017-02-13-p3.htm"]
allListings filenames = do
  ls <- mapM go filenames
  return (P.concat ls)
 where
  go filename = do
    txt <- T.readFile filename
    return (listings txt)

------------------------------------------------------------------------------
-- page links

pp = ppp "./test/p1.html" pageLinks

pageLinks txt =
  L.nub (pickOutPageLinks (P.takeWhile (\x -> x /= TagClose "ol")
                                       (P.head (getPageLinks txt))))

getPageLinks :: Text -> [[Tag Text]]
getPageLinks t = textParseTags "<ol class=zsg-pagination>" t

pickOutPageLinks [] = []
pickOutPageLinks (x:xs) =
  case x of
    TagOpen tagName attributes | tagName == "a" && not (P.null attributes)
                                 -> (snd $ P.head attributes) : pickOutPageLinks xs
    _                            ->                             pickOutPageLinks xs

------------------------------------------------------------------------------
-- listings (via <article>) in page

pl = ppp "./test/p1.html" listings

listings txt =
  P.map cleanse (P.map (pickData . pickTags) (getArticles txt))

getArticles :: Text -> [[Tag Text]]
getArticles t = textParseTags "<article>" t

cleanse [] = []
cleanse (x:xs) =
  if T.isPrefixOf "/homedetails/" x
  then      x : cleanse xs
  else if T.isPrefixOf "/homedetail/AuthRequired.htm" x ||
          T.isPrefixOf "/" x ||
          P.elem x zillowIgnore
       then     cleanse xs
       else x : cleanse xs

pickTags :: [Tag Text] -> [Tag Text]
pickTags tags = do
  myFilter False tags
 where
  myFilter     _    []  = []
  myFilter  True (x:xs) = x : myFilter False xs
  myFilter False (x:xs) =
    if x == TagOpen "span" [("itemprop", "streetAddress")] ||
       x == TagOpen "span" [("class",    "zsg-photo-card-price")]
    then myFilter True  xs
    else if x ~== ("<a>"::String)
         then x : myFilter False xs
         else     myFilter False xs

pickData :: [Tag Text] -> [Text]
pickData tags = do
  P.map f tags
 where
  f x = case x of
    (TagText    t) -> t
    (TagOpen _ xs) -> if not (P.null xs) then snd $ P.head xs else ""

zillowIgnore = ["option","zsg-lightbox-show za-track-event","http://www.zillow.com/local-info/","http://www.facebook.com/Zillow","http://twitter.com/zillow","http://plus.google.com/+Zillow","zsg-notification-bar-close","mapped-result-count","#","#","#","#","#","#","menu-label","#fore-tip-filters","#coming-soon-tip-filters","#pm-tip-filters","#pmf-tip-filters","#pre-foreclosure-tip-filters","#mmm-tip-filters","#pending-tip-filters","price-menu-label","saf-entry-link","#payment","#income","#","saf-close zsg-button","saf-pre-approval-link","beds-menu-label","type-menu-label","menu-label","#hoa-dues-tooltip","http://www.zillow.com/community-pillar/","zsg-button_primary"]

------------------------------------------------------------------------------
-- util

textParseTags :: String -> Text -> [[Tag Text]]
textParseTags tag t = do
  let ts = parseTags t
  partitions (~== tag) ts

getDate = do
  now <- getCurrentTime
  myzone <- getCurrentTimeZone
  let x = show (utcToZonedTime myzone now :: ZonedTime)
  return $ P.take 10 x

ppp filename f = do
  txt <- T.readFile filename
  printTags (f txt)

printTags tags = do
  mapM_ print tags

