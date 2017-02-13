module Lib where

import           Data.Text         as T
import           Data.Text.IO      as T
import           Prelude           as P
import           Text.HTML.TagSoup

-- curl http://www.zillow.com/homes/for_sale/84103_rb/?fromHomePage=true&shouldFireSellPageImplicitClaimGA=false&fromHomePageTab=buy

pa = do
  tags <- fileParseArticleTags "./test/p1.html"
  mapM_ print (P.map (pickData . pickTags) tags)

pl = do
  tags <- fileParsePageLinks "./test/p1.html"
  mapM_ print (P.head tags)

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
    if x == TagOpen (T.pack "span") [(T.pack "itemprop", T.pack "streetAddress")] ||
       x == TagOpen (T.pack "span") [(T.pack "class",    T.pack "zsg-photo-card-price")]
    then myFilter True  xs
    else if x ~== "<a>"
         then x : myFilter False xs
         else     myFilter False xs

pickData :: [Tag Text] -> [Text]
pickData tags = do
  P.map f tags
 where
  f x = case x of
    (TagText    t) -> t
    (TagOpen _ xs) -> snd $ P.head xs

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
