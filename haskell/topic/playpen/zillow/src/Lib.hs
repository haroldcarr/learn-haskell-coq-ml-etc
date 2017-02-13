module Lib where

import           Data.Text         as T
import           Data.Text.IO      as T
import           Prelude           as P
import           Text.HTML.TagSoup

a = do
  tags <- fileParseTags "./test/p1.html"
  return . pickData $ pickTags tags

fileParseTags :: String -> IO [Tag Text]
fileParseTags filename = do
  d  <- T.readFile filename
  let ts = parseTags d
  return $ P.head $ partitions (~== "<article>") ts

printTags tags = do
  mapM_ print tags

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
    [TagText "141 E Edgecombe Dr",TagText "$624,999",TagOpen "a" [("href","/homedetails/141-E-Edgecombe-Dr-Salt-Lake-City-UT-84103/12717491_zpid/"),("class","zsg-photo-card-overlay-link routable hdp-link routable mask hdp-link")],TagOpen "a" [("href","/myzillow/UpdateFavorites.htm?zpid=12717491&operation=add&ajax=false"),("rel","nofollow"),("data-fm-zpid","12717491"),("data-fm-callback","windowReloadSuccessHandler"),("data-after-auth-action-type","Event"),("data-after-auth-global-event","favoriteManager:addFavoriteProperty"),("data-target-id","register"),("title","Save this home"),("data-show-home-owner-lightbox","false"),("data-za-label","Save Map:List"),("data-address","141 E Edgecombe Dr , Salt Lake City, UT 84103"),("class","zsg-lightbox-show open-on-any-click"),("id","register_opener_0")]]


TagOpen "a" [("href","/homedetails/141-E-Edgecombe-Dr-Salt-Lake-City-UT-84103/12717491_zpid/"),("class","zsg-photo-card-overlay-link routable hdp-link routable mask hdp-link")]
-}
