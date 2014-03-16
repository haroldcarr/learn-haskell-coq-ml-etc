{-
Created       : 2014 Mar 03 (Mon) 20:39:50 by Harold Carr.
Last Modified : 2014 Mar 10 (Mon) 22:52:04 by Harold Carr.
-}

module BitlyClientTest where

import           BitlyClient
import           BitlyClientCommon
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
expandRequestTestData = RequestExpand [ "http://bit.ly/LCJq0b"
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

localTests :: Test
localTests = TestList
    [teq "urlEncodeVars"   (urlEncodeVars encodeTestParams) "hash=api-client&hash=phphotoWinterSunII&hash=phphotoQuoVadis&hash=phphotoDock3&hash=phphotoZueriWest&shortUrl=http%3A%2F%2Fbit.ly%2FLCJq0b&shortUrl=http%3A%2F%2Fbit.ly%2FphphotoCrossroads&shortUrl=http%3A%2F%2Fbit.ly%2FspringFever&shortUrl=http%3A%2F%2Fbit.ly%2FphphotoBenched&shortUrl=http%3A%2F%2Fbit.ly%2FLt5SJo&access_token=FOOBAR"

    ,teq "mkReqUrl expand" (mkReqUrl expandRequestTestData) "https://api-ssl.bitly.com/v3/expand?shortUrl=http%3A%2F%2Fbit.ly%2FLCJq0b&shortUrl=http%3A%2F%2Fbit.ly%2FphphotoCrossroads&shortUrl=http%3A%2F%2Fbit.ly%2FspringFever&shortUrl=http%3A%2F%2Fbit.ly%2FphphotoBenched&shortUrl=http%3A%2F%2Fbit.ly%2FLt5SJo&hash=api-client&hash=phphotoWinterSunII&hash=phphotoQuoVadis&hash=phphotoDock3&hash=phphotoZueriWest"
    ]

remoteTests :: Test
remoteTests = TestList
    [teq "doRequest expand" (unsafePerformIO (doRequest expandRequestTestData)) (L.pack "{ \"status_code\": 200, \"status_txt\": \"OK\", \"data\": { \"expand\": [ { \"short_url\": \"http:\\/\\/bit.ly\\/LCJq0b\", \"long_url\": \"http:\\/\\/blog.swisstech.net\\/2012\\/06\\/local-postfix-as-relay-to-amazon-ses.html\", \"user_hash\": \"LCJq0b\", \"global_hash\": \"LCJsVy\" }, { \"short_url\": \"http:\\/\\/bit.ly\\/phphotoCrossroads\", \"long_url\": \"http:\\/\\/500px.com\\/photo\\/8337350\", \"user_hash\": \"KShcAV\", \"global_hash\": \"KShcAW\" }, { \"short_url\": \"http:\\/\\/bit.ly\\/springFever\", \"long_url\": \"http:\\/\\/500px.com\\/photo\\/5802359\", \"user_hash\": \"MbmhB4\", \"global_hash\": \"MbmhB5\" }, { \"short_url\": \"http:\\/\\/bit.ly\\/phphotoBenched\", \"long_url\": \"http:\\/\\/500px.com\\/photo\\/8247630\", \"user_hash\": \"LqVZLZ\", \"global_hash\": \"LqW02c\" }, { \"short_url\": \"http:\\/\\/bit.ly\\/Lt5SJo\", \"long_url\": \"http:\\/\\/www.bieberphoto.com\\/index.php\\/2012\\/05\\/15\\/automating-mass-portraits-with-qr-codes\\/\", \"user_hash\": \"Lt5SJo\", \"global_hash\": \"JPNM5X\" }, { \"hash\": \"api-client\", \"long_url\": \"https:\\/\\/github.com\\/stackmagic\\/bitly-api-client\", \"user_hash\": \"LfXpbF\", \"global_hash\": \"LfXpbG\" }, { \"hash\": \"phphotoWinterSunII\", \"long_url\": \"http:\\/\\/500px.com\\/photo\\/7998374\", \"user_hash\": \"LjTbAy\", \"global_hash\": \"LjTbAz\" }, { \"hash\": \"phphotoQuoVadis\", \"long_url\": \"http:\\/\\/500px.com\\/photo\\/7980928\", \"user_hash\": \"JYROvi\", \"global_hash\": \"JYROvj\" }, { \"hash\": \"phphotoDock3\", \"long_url\": \"http:\\/\\/500px.com\\/photo\\/7968427\", \"user_hash\": \"KmfxWg\", \"global_hash\": \"KmfxWh\" }, { \"hash\": \"phphotoZueriWest\", \"long_url\": \"http:\\/\\/500px.com\\/photo\\/7010618\", \"user_hash\": \"LlyMLt\", \"global_hash\": \"LlyN1G\" } ] } }\n")
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
