{-# LANGUAGE OverloadedStrings #-}

module Lib where

import           Data.List         as L
import           Data.Text         as T
import           Data.Text.IO      as T
import           Prelude           as P
import           Text.HTML.TagSoup

-- curl http://www.zillow.com/homes/for_sale/84103_rb/?fromHomePage=true&shouldFireSellPageImplicitClaimGA=false&fromHomePageTab=buy

pa = do
  tags <- fileParseArticleTags "./test/p1.html"
  printTags (P.map cleanse (P.map (pickData . pickTags) tags))

cleanse [] = []
cleanse (x:xs) = if T.isPrefixOf "/homedetails/" x
                 then x : cleanse xs
                 else if T.isPrefixOf "/homedetail/AuthRequired.htm" x ||
                         T.isPrefixOf "/" x ||
                         P.elem x zillowIgnore
                      then     cleanse xs
                      else x : cleanse xs

pl = do
  tags <- fileParsePageLinks "./test/p1.html"
  printTags (L.nub (pickOutPageLinks (P.takeWhile (\x -> x /= TagClose "ol")
                                                  (P.head tags))))

pickOutPageLinks [] = []
pickOutPageLinks (x:xs) =
  case x of
    TagOpen tagName attributes | tagName == "a" && not (P.null attributes)
                                 -> (snd $ P.head attributes) : pickOutPageLinks xs
    _                            ->                             pickOutPageLinks xs

printTags tags = do
  mapM_ print tags

fileParseArticleTags :: String -> IO [[Tag Text]]
fileParseArticleTags filename = do
  d  <- T.readFile filename
  return $ getArticles d

fileParsePageLinks :: String -> IO [[Tag Text]]
fileParsePageLinks filename = do
  d  <- T.readFile filename
  return $ getPageLinks d

getArticles :: Text -> [[Tag Text]]
getArticles t = textParseTags "<article>" t

getPageLinks :: Text -> [[Tag Text]]
-- getPageLinks t = textParseTags "<div id=search-pagination-wrapper class=zsg-content-item>" t
getPageLinks t = textParseTags "<ol class=zsg-pagination>" t

textParseTags :: String -> Text -> [[Tag Text]]
textParseTags tag t = do
  let ts = parseTags t
  partitions (~== tag) ts

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

{-

<div id="search-pagination-wrapper" class="zsg-content-item"><ol class="zsg-pagination"><li class="zsg-pagination_active"><a>1</a></li><li><a href="/homes/for_sale/84103_rb/2_p/" onclick="SearchMain.changePage(2);return false;">2</a></li><li><a href="/homes/for_sale/84103_rb/3_p/" onclick="SearchMain.changePage(3);return false;">3</a></li><li class="zsg-pagination-next"><a href="/homes/for_sale/84103_rb/2_p/" class="off" onclick="SearchMain.changePage(2);return false;">Next</a></li></ol></div></div>

<div id="search-pagination-wrapper" class="zsg-content-item">
  <ol class="zsg-pagination">
    <li class="zsg-pagination_active"><a>1</a></li>
    <li><a href="/homes/for_sale/84103_rb/2_p/" onclick="SearchMain.changePage(2);return false;">2</a></li>
    <li><a href="/homes/for_sale/84103_rb/3_p/" onclick="SearchMain.changePage(3);return false;">3</a></li>
    <li class="zsg-pagination-next">
        <a href="/homes/for_sale/84103_rb/2_p/" class="off" onclick="SearchMain.changePage(2);return false;">Next</a></li>
  </ol>
</div>

    [TagText "141 E Edgecombe Dr",TagText "$624,999",TagOpen "a" [("href","/homedetails/141-E-Edgecombe-Dr-Salt-Lake-City-UT-84103/12717491_zpid/"),("class","zsg-photo-card-overlay-link routable hdp-link routable mask hdp-link")],TagOpen "a" [("href","/myzillow/UpdateFavorites.htm?zpid=12717491&operation=add&ajax=false"),("rel","nofollow"),("data-fm-zpid","12717491"),("data-fm-callback","windowReloadSuccessHandler"),("data-after-auth-action-type","Event"),("data-after-auth-global-event","favoriteManager:addFavoriteProperty"),("data-target-id","register"),("title","Save this home"),("data-show-home-owner-lightbox","false"),("data-za-label","Save Map:List"),("data-address","141 E Edgecombe Dr , Salt Lake City, UT 84103"),("class","zsg-lightbox-show open-on-any-click"),("id","register_opener_0")]]


TagOpen "a" [("href","/homedetails/141-E-Edgecombe-Dr-Salt-Lake-City-UT-84103/12717491_zpid/"),("class","zsg-photo-card-overlay-link routable hdp-link routable mask hdp-link")]
-}
