{-
Created       : 2014 Mar 03 (Mon) 20:39:50 by Harold Carr.
Last Modified : 2014 Mar 19 (Wed) 23:08:05 by Harold Carr.
-}

module BitlyClientTest where

import           BitlyClient
import           BitlyClientCommon
import           BitlyClientRequests
import           BitlyClientResponses
import qualified Data.ByteString.Lazy.Char8 as L (pack)
import           System.IO.Unsafe           (unsafePerformIO)
import           Test.HUnit
import           Test.HUnit.Util

encodeTestParams :: [ (String,String) ]
encodeTestParams  = [ ("hash"        , "api-client")
                    , ("hash"        , "phphotoWinterSunII")
                    , ("hash"        , "phphotoQuoVadis")
                    , ("hash"        , "phphotoDock3")
                    , ("hash"        , "phphotoZueriWest")
                    , ("shortUrl"    , "http://bit.ly/LCJq0b")
                    , ("shortUrl"    , "http://bit.ly/phphotoCrossroads")
                    , ("shortUrl"    , "http://bit.ly/springFever")
                    , ("shortUrl"    , "http://bit.ly/phphotoBenched")
                    , ("shortUrl"    , "http://bit.ly/Lt5SJo")
                    , ("access_token", "FOOBAR")
                    ]

expandRequestTestData :: Request
expandRequestTestData = mkExpandRequest [ "http://bit.ly/LCJq0b"
                                        , "http://bit.ly/phphotoCrossroads"
                                        , "http://bit.ly/springFever"
                                        , "http://bit.ly/phphotoBenched"
                                        , "http://bit.ly/Lt5SJo"
                                        ]
                                        [ "api-client"
                                        , "phphotoWinterSunII"
                                        , "phphotoQuoVadis"
                                        , "phphotoDock3"
                                        , "phphotoZueriWest"
                                        ]
infoRequestTestData :: Request
infoRequestTestData = mkInfoRequest [ "phphotoLakeZurichAtDusk"
                                    , "phphotoWinterSunII"
                                    , "phphotoQuoVadis"
                                    , "phphotoDock3"
                                    , "phphotoZueriWest"
                                    ]
                                    [ "http://bit.ly/LCJq0b"
                                    , "http://bit.ly/phphotoCrossroads"
                                    , "http://bit.ly/springFever"
                                    , "http://bit.ly/phphotoBenched"
                                    , "http://bit.ly/Lt5SJo"
                                    ]
                                    (Just False)



expandResponseTestData :: String
expandResponseTestData = "{ \"status_code\": 200, \"status_txt\": \"OK\", \"data\": { \"expand\": [ { \"short_url\": \"http:\\/\\/bit.ly\\/LCJq0b\", \"long_url\": \"http:\\/\\/blog.swisstech.net\\/2012\\/06\\/local-postfix-as-relay-to-amazon-ses.html\", \"user_hash\": \"LCJq0b\", \"global_hash\": \"LCJsVy\" }, { \"short_url\": \"http:\\/\\/bit.ly\\/phphotoCrossroads\", \"long_url\": \"http:\\/\\/500px.com\\/photo\\/8337350\", \"user_hash\": \"KShcAV\", \"global_hash\": \"KShcAW\" }, { \"short_url\": \"http:\\/\\/bit.ly\\/springFever\", \"long_url\": \"http:\\/\\/500px.com\\/photo\\/5802359\", \"user_hash\": \"MbmhB4\", \"global_hash\": \"MbmhB5\" }, { \"short_url\": \"http:\\/\\/bit.ly\\/phphotoBenched\", \"long_url\": \"http:\\/\\/500px.com\\/photo\\/8247630\", \"user_hash\": \"LqVZLZ\", \"global_hash\": \"LqW02c\" }, { \"short_url\": \"http:\\/\\/bit.ly\\/Lt5SJo\", \"long_url\": \"http:\\/\\/www.bieberphoto.com\\/index.php\\/2012\\/05\\/15\\/automating-mass-portraits-with-qr-codes\\/\", \"user_hash\": \"Lt5SJo\", \"global_hash\": \"JPNM5X\" }, { \"hash\": \"api-client\", \"long_url\": \"https:\\/\\/github.com\\/stackmagic\\/bitly-api-client\", \"user_hash\": \"LfXpbF\", \"global_hash\": \"LfXpbG\" }, { \"hash\": \"phphotoWinterSunII\", \"long_url\": \"http:\\/\\/500px.com\\/photo\\/7998374\", \"user_hash\": \"LjTbAy\", \"global_hash\": \"LjTbAz\" }, { \"hash\": \"phphotoQuoVadis\", \"long_url\": \"http:\\/\\/500px.com\\/photo\\/7980928\", \"user_hash\": \"JYROvi\", \"global_hash\": \"JYROvj\" }, { \"hash\": \"phphotoDock3\", \"long_url\": \"http:\\/\\/500px.com\\/photo\\/7968427\", \"user_hash\": \"KmfxWg\", \"global_hash\": \"KmfxWh\" }, { \"hash\": \"phphotoZueriWest\", \"long_url\": \"http:\\/\\/500px.com\\/photo\\/7010618\", \"user_hash\": \"LlyMLt\", \"global_hash\": \"LlyN1G\" } ] } }\n"

infoResponseTestData :: String
infoResponseTestData = "{\"status_code\": 200, \"data\": {\"info\": [{\"hash\": \"phphotoLakeZurichAtDusk\", \"keyword\": \"phphotoLakeZurichAtDusk\", \"title\": \"500px / Lake Zurich at Dusk by Patrick Huber\", \"created_at\": 1338666104, \"created_by\": \"stackmagic\", \"global_hash\": \"LTlncn\", \"user_hash\": \"LTlncm\"}, {\"hash\": \"phphotoWinterSunII\", \"keyword\": \"phphotoWinterSunII\", \"title\": \"500px / Winter Sun II by Patrick Huber\", \"created_at\": 1338679612, \"created_by\": \"stackmagic\", \"global_hash\": \"LjTbAz\", \"user_hash\": \"LjTbAy\"}, {\"hash\": \"phphotoQuoVadis\", \"keyword\": \"phphotoQuoVadis\", \"title\": \"500px / Quo Vadis? by Patrick Huber\", \"created_at\": 1338679845, \"created_by\": \"stackmagic\", \"global_hash\": \"JYROvj\", \"user_hash\": \"JYROvi\"}, {\"hash\": \"phphotoDock3\", \"keyword\": \"phphotoDock3\", \"title\": \"500px / Photo \\\"Dock 3\\\" by Patrick Huber\", \"created_at\": 1338679895, \"created_by\": \"stackmagic\", \"global_hash\": \"KmfxWh\", \"user_hash\": \"KmfxWg\"}, {\"hash\": \"phphotoZueriWest\", \"keyword\": \"phphotoZueriWest\", \"title\": \"500px / Z\\u00fcri West by Patrick Huber\", \"created_at\": 1338717627, \"created_by\": \"stackmagic\", \"global_hash\": \"LlyN1G\", \"user_hash\": \"LlyMLt\"}, {\"title\": \"swisstech.net: Local postfix as relay to Amazon SES\", \"short_url\": \"http://bit.ly/LCJq0b\", \"created_at\": 1339143918, \"created_by\": \"stackmagic\", \"global_hash\": \"LCJsVy\", \"user_hash\": \"LCJq0b\"}, {\"keyword\": \"phphotoCrossroads\", \"title\": \"500px / Crossroads by Patrick Huber\", \"short_url\": \"http://bit.ly/phphotoCrossroads\", \"created_at\": 1339013462, \"created_by\": \"stackmagic\", \"global_hash\": \"KShcAW\", \"user_hash\": \"KShcAV\"}, {\"keyword\": \"springFever\", \"title\": \"500px / Photo \\\"Spring Fever\\\" by Patrick Huber\", \"short_url\": \"http://bit.ly/springFever\", \"created_at\": 1339051124, \"created_by\": \"stackmagic\", \"global_hash\": \"MbmhB5\", \"user_hash\": \"MbmhB4\"}, {\"keyword\": \"phphotoBenched\", \"title\": \"500px / Benched by Patrick Huber\", \"short_url\": \"http://bit.ly/phphotoBenched\", \"created_at\": 1338760269, \"created_by\": \"stackmagic\", \"global_hash\": \"LqW02c\", \"user_hash\": \"LqVZLZ\"}, {\"title\": \"Automating Mass Portraits With QR Codes  \\u00ab Bieber Photographic\", \"short_url\": \"http://bit.ly/Lt5SJo\", \"created_at\": 1338844882, \"created_by\": \"stackmagic\", \"global_hash\": \"JPNM5X\", \"user_hash\": \"Lt5SJo\"}]}, \"status_txt\": \"OK\"}"


localTests :: Test
localTests = TestList
    [teq "urlEncodeVars"   (urlEncodeVars encodeTestParams) "hash=api-client&hash=phphotoWinterSunII&hash=phphotoQuoVadis&hash=phphotoDock3&hash=phphotoZueriWest&shortUrl=http%3A%2F%2Fbit.ly%2FLCJq0b&shortUrl=http%3A%2F%2Fbit.ly%2FphphotoCrossroads&shortUrl=http%3A%2F%2Fbit.ly%2FspringFever&shortUrl=http%3A%2F%2Fbit.ly%2FphphotoBenched&shortUrl=http%3A%2F%2Fbit.ly%2FLt5SJo&access_token=FOOBAR"

    ,teq "makeRequestUrl expand" (makeRequestUrl expandRequestTestData) "https://api-ssl.bitly.com/v3/expand?shortUrl=http%3A%2F%2Fbit.ly%2FLCJq0b&shortUrl=http%3A%2F%2Fbit.ly%2FphphotoCrossroads&shortUrl=http%3A%2F%2Fbit.ly%2FspringFever&shortUrl=http%3A%2F%2Fbit.ly%2FphphotoBenched&shortUrl=http%3A%2F%2Fbit.ly%2FLt5SJo&hash=api-client&hash=phphotoWinterSunII&hash=phphotoQuoVadis&hash=phphotoDock3&hash=phphotoZueriWest"

    ,teq "makeRequestUrl shorten" (makeRequestUrl (mkShortenRequest "u" "d")) "https://api-ssl.bitly.com/v3/shorten?longUrl=u&domain=d"
    ,teq "makeRequestUrl expand"  (makeRequestUrl (mkExpandRequest ["u1","u2"] ["h1"])) "https://api-ssl.bitly.com/v3/expand?shortUrl=u1&shortUrl=u2&hash=h1"
    ,teq "makeRequestUrl linkedit" (makeRequestUrl (mkLinkEditRequest "linkValue" (Just "maybeTitleValue") Nothing (Just True) (Just 3) Nothing ["edit1","edit2"])) "https://api-ssl.bitly.com/v3/linkedit?link=linkValue&title=maybeTitleValue&private=True&user_ts=3&edit=edit1&edit=edit2"
    ,teq "parseResponse expand" (Right (parseResponse expandResponseTestData)) (Left "EXPAND") -- (Right (DSCST (ExpandResponseData []) 200 "OK"))
    ,teq "parseResponse info"   (Right (parseResponse infoResponseTestData)) (Left "INFO")
    ]

remoteTests :: Test
remoteTests = TestList
    [teq "doRequest expand" (unsafePerformIO (doRequest expandRequestTestData)) (L.pack expandResponseTestData)
    ,teq "doRequest info"   (unsafePerformIO (doRequest   infoRequestTestData)) (L.pack infoResponseTestData)
    ]

runLocalTests :: IO Counts
runLocalTests = runTestTT localTests

runRemoteTests :: IO Counts
runRemoteTests = runTestTT remoteTests

runTests :: IO Counts
runTests = do
    _ <- runLocalTests
    runRemoteTests

-- End of file.
