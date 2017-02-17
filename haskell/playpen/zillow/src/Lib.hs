{-# LANGUAGE OverloadedStrings #-}

module Lib where

import           Control.Lens                  ((&), (.~), (^.))
import           Data.ByteString.Lazy.Char8    as BSLC8
import           Data.List                     as L
import           Data.Text                     as T
import           Data.Text.IO                  as T
import           Data.Thyme.Time
import           Network.HTTP.Client.OpenSSL
import           Network.Wreq                  as W
import           OpenSSL.Session               (context)
import           Prelude                       as P
import           Text.Blaze.Html.Renderer.Utf8 (renderHtml)
import qualified Text.Blaze.Html5              as H
import qualified Text.Blaze.Html5.Attributes   as A
import           Text.HTML.TagSoup

------------------------------------------------------------------------------
-- constants

baseUrl = "https://www.zillow.com"

page1Url =
  baseUrl ++ "/homes/for_sale/84103_rb/?fromHomePage=true&shouldFireSellPageImplicitClaimGA=false&fromHomePageTab=buy"

dataDir = "data/"

------------------------------------------------------------------------------
-- downloads

downloadAll = do
  filename    <- download page1Url 1
  -- let filename = P.concat [dataDir, "2017-02-13", "-p", show 1, ".htm"]
  txt         <- T.readFile filename
  let links    = pageLinks txt
  filenames   <- mapM (\(pagelink,n) -> download (baseUrl ++ T.unpack pagelink) n)
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
    print  (r ^. responseStatus)
    print  (r ^. responseHeaders)
    return (BSLC8.unpack (r ^. responseBody))
 where
  opts = defaults & manager .~ Left (opensslManagerSettings context)

------------------------------------------------------------------------------
-- page links

pageLinks txt =
  L.nub (pickOutPageLinks (P.takeWhile (\x -> x /= TagClose "ol")
                                       (P.head (getPageLinks txt))))

getPageLinks :: Text -> [[Tag Text]]
getPageLinks = textParseTags "<ol class=zsg-pagination>"

pickOutPageLinks [] = []
pickOutPageLinks (x:xs) =
  case x of
    TagOpen tagName attributes | tagName == "a" && not (P.null attributes)
                                 -> snd (P.head attributes) : pickOutPageLinks xs
    _                            ->                           pickOutPageLinks xs

------------------------------------------------------------------------------
-- listings (via <article>) in page

-- these two for quick checks
pl = ppp "./data/2017-02-16-p1.htm" listings
pal = do
  txt <- T.readFile "./data/2017-02-16-p1.htm"
  mapM_ (mapM_ print) (getArticles txt)

-- printTags =<< allListings ["data/2017-02-16-p1.htm", "data/2017-02-16-p2.htm", "data/2017-02-16-p3.htm"]
-- printTags =<< allListings ["data/2017-02-13-p1.htm", "data/2017-02-13-p2.htm", "data/2017-02-13-p3.htm"]
allListings filenames = do
  ls <- mapM go filenames
  return (P.concat ls)
 where
  go filename = do
    txt <- T.readFile filename
    return (listings txt)

listings txt =
  sortIt
    (P.filter hasPrice
              (P.map (cleanse . pickData . pickTags)
                     (getArticles txt)))

getArticles :: Text -> [[Tag Text]]
getArticles = textParseTags "<article>"

pickTags :: [Tag Text] -> [Tag Text]
pickTags = myFilter False
 where
  myFilter     _    []  = []
  myFilter  True (x:xs) = x : myFilter False xs
  myFilter False (x:xs)
    | x == TagOpen "span" [("itemprop", "streetAddress")] ||
      x == TagOpen "span" [("class", "zsg-photo-card-price")]
                =     myFilter True xs
    | x ~== ("<a>" :: String) || x ~== ("<img>" :: String)
                = x : myFilter False xs
    | otherwise =     myFilter False xs

pickData :: [Tag Text] -> [Text]
pickData = P.map f
 where
  f x = case x of
    (TagText    t) -> t
    (TagOpen _ xs) -> if not (P.null xs) then snd $ P.head xs else ""

cleanse [] = []
cleanse (x:xs)
    | T.isPrefixOf "/homedetails/" x || T.isPrefixOf "https://photos" x
                = x : cleanse xs
    |    T.isPrefixOf "/homedetail/AuthRequired.htm" x
      || T.isPrefixOf "/" x
      || T.isPrefixOf "https://dev.virtualearth.net" x
      || T.isPrefixOf "https://sb.scorecardresearch.com" x
      || P.elem x zillowIgnore
                =     cleanse xs
    | otherwise = x : cleanse xs

hasPrice = P.any (T.isPrefixOf "$")

sortIt :: [[Text]] -> [[Text]]
sortIt = L.sortBy (\(_:p1:_) (_:p2:_) -> n p1 `compare` n p2)
 where
  n x = read (T.unpack (T.filter (\x -> x /= '$' && x /= ',') x)) :: Int

zillowIgnore = ["option","zsg-lightbox-show za-track-event","http://www.zillow.com/local-info/","http://www.facebook.com/Zillow","http://twitter.com/zillow","http://plus.google.com/+Zillow","zsg-notification-bar-close","mapped-result-count","#","#","#","#","#","#","menu-label","#fore-tip-filters","#coming-soon-tip-filters","#pm-tip-filters","#pmf-tip-filters","#pre-foreclosure-tip-filters","#mmm-tip-filters","#pending-tip-filters","price-menu-label","saf-entry-link","#payment","#income","#","saf-close zsg-button","saf-pre-approval-link","beds-menu-label","type-menu-label","menu-label","#hoa-dues-tooltip","http://www.zillow.com/community-pillar/","zsg-button_primary"]

------------------------------------------------------------------------------
-- display

writeDisplayListings = do
  al <- allListings ["data/2017-02-16-p1.htm", "data/2017-02-16-p2.htm", "data/2017-02-16-p3.htm"]
  -- print al
  let dl = displayListings al
  let rl = renderHtml dl
  -- print rl
  return $ BSLC8.writeFile "/tmp/xxx.html" rl

displayListings xs = H.docTypeHtml $
  H.head $ do
    H.title "84103 listings"
    H.body $
        mapM_ displayListing xs

displayListing :: [Text] -> H.Html
displayListing [address, price, pagelink, photolink] = do
  H.hr
  H.img H.! A.src  (H.preEscapedTextValue photolink) H.! A.alt (H.preEscapedTextValue photolink)
  H.br
  H.string (T.unpack address)
  H.string " "
  H.string (T.unpack price)
  H.string " "
  H.a   H.! A.href (H.preEscapedStringValue (baseUrl ++ T.unpack pagelink))  $   "details"
displayListing x = do
  H.hr
  H.h2 (H.string (show x))

renderListings = renderHtml

------------------------------------------------------------------------------
-- util

textParseTags :: String -> Text -> [[Tag Text]]
textParseTags tag t = do
  let ts = parseTags t
  partitions (~== tag) ts

getDate = do
  now    <- getCurrentTime
  myzone <- getCurrentTimeZone
  let x   = show (utcToZonedTime myzone now :: ZonedTime)
  return $ P.take 10 x

ppp filename f = do
  txt <- T.readFile filename
  printTags (f txt)

printTags = mapM_ print

