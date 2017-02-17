{-# LANGUAGE OverloadedStrings #-}

import           Data.ByteString.Lazy as BS
import           Data.Text.IO         as T
import           Lib                  as L
import           Prelude              as P
import           Test.Hspec

main :: IO ()
main = hspec $ do
  describe "test" test

test :: Spec
test = do
  it "pageLinks" $ do
    t <- T.readFile "./data/2017-02-16-p1.htm"
    let pl = pageLinks t
    pl `shouldBe` ["/homes/for_sale/84103_rb/2_p/","/homes/for_sale/84103_rb/3_p/"]
  it "listings" $ do
    t <- T.readFile "./data/2017-02-16-p1.htm"
    let ls = listings t
    P.take 2 ls `shouldBe`
      [["141 E Edgecombe Dr"
       ,"$624,999"
       ,"/homedetails/141-E-Edgecombe-Dr-Salt-Lake-City-UT-84103/12717491_zpid/"
       ,"https://photos.zillowstatic.com/p_e/ISe40jipyeb86a0000000000.jpg"
       ]
      ,["463 11th Ave"
       ,"$699,000"
       ,"/homedetails/463-11th-Ave-Salt-Lake-City-UT-84103/12718192_zpid/"
       ,"https://photos.zillowstatic.com/p_e/ISyjx66u5tpae40000000000.jpg"
       ]]
  it "allListings" $ do
    al <- allListings ["data/2017-02-16-p1.htm", "data/2017-02-16-p2.htm", "data/2017-02-16-p3.htm"]
    P.length al `shouldBe` 52
  it "displayListings" $ do
    al <- allListings ["data/2017-02-16-p1.htm", "data/2017-02-16-p2.htm", "data/2017-02-16-p3.htm"]
    let dl = displayListings al
    BS.take 607 (renderListings dl) `shouldBe` "<!DOCTYPE HTML>\n<html><head><title>84103 listings</title><body><h2>141 E Edgecombe Dr</h2><p>$624,999</p><a href=\"/homedetails/141-E-Edgecombe-Dr-Salt-Lake-City-UT-84103/12717491_zpid/\">page</a><img src=\"https://photos.zillowstatic.com/p_e/ISe40jipyeb86a0000000000.jpg\" alt=\"https://photos.zillowstatic.com/p_e/ISe40jipyeb86a0000000000.jpg\"><h2>463 11th Ave</h2><p>$699,000</p><a href=\"/homedetails/463-11th-Ave-Salt-Lake-City-UT-84103/12718192_zpid/\">page</a><img src=\"https://photos.zillowstatic.com/p_e/ISyjx66u5tpae40000000000.jpg\" alt=\"https://photos.zillowstatic.com/p_e/ISyjx66u5tpae40000000000.jpg\">"
