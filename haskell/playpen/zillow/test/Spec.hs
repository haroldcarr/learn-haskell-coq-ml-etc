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
      [ Listing { link = "/homedetails/521-E-5th-Ave-APT-602-Salt-Lake-City-UT-84103/2095378819_zpid/"
                , photolink = Just "https://photos.zillowstatic.com/p_e/ISu867yw477gn41000000000.jpg"
                , address = "521 E 5th Ave APT 602"
                , price = 159900
                , priceS = "$159,900"
                }
      , Listing { link = "/homedetails/143-E-1st-Ave-APT-303-Salt-Lake-City-UT-84103/83828461_zpid/"
                , photolink = Just "https://photos.zillowstatic.com/p_e/ISm6jpgwcml9q80000000000.jpg"
                , address = "143 E 1st Ave APT 303"
                , price = 215000
                , priceS = "$215,000"
                }
      ]
  it "allListings" $ do
    al <- allListings ["data/2017-02-16-p1.htm", "data/2017-02-16-p2.htm", "data/2017-02-16-p3.htm"]
    P.length al `shouldBe` 52
  it "displayListings" $ do
    al <- allListings ["data/2017-02-16-p1.htm", "data/2017-02-16-p2.htm", "data/2017-02-16-p3.htm"]
    let dl = displayListings al
    BS.take 665 (renderListings dl) `shouldBe` "<!DOCTYPE HTML>\n<html><head><title>84103 listings</title><body><hr><img src=\"https://photos.zillowstatic.com/p_e/ISu867yw477gn41000000000.jpg\" alt=\"https://photos.zillowstatic.com/p_e/ISu867yw477gn41000000000.jpg\"> 521 E 5th Ave APT 602 $159,900 <a href=\"https://www.zillow.com/homedetails/521-E-5th-Ave-APT-602-Salt-Lake-City-UT-84103/2095378819_zpid/\">details</a><hr><img src=\"https://photos.zillowstatic.com/p_e/ISm6jpgwcml9q80000000000.jpg\" alt=\"https://photos.zillowstatic.com/p_e/ISm6jpgwcml9q80000000000.jpg\"> 143 E 1st Ave APT 303 $215,000 <a href=\"https://www.zillow.com/homedetails/143-E-1st-Ave-APT-303-Salt-Lake-City-UT-84103/83828461_zpid/\">details</a>"
