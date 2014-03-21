{-
Created       : 2014 Mar 03 (Mon) 20:39:50 by Harold Carr.
Last Modified : 2014 Mar 21 (Fri) 16:28:03 by Harold Carr.
-}

module BitlyClientTest where

import           BitlyClient
import           BitlyClientCommon
import           BitlyClientRequests
import           BitlyClientResponses
import           BitlyClientTH
import qualified Data.ByteString.Lazy.Char8 as L (pack)
import           Language.Haskell.TH
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
expandResponseTestData :: String
expandResponseTestData = "{ \"status_code\": 200, \"status_txt\": \"OK\", \"data\": { \"expand\": [ { \"short_url\": \"http:\\/\\/bit.ly\\/LCJq0b\", \"long_url\": \"http:\\/\\/blog.swisstech.net\\/2012\\/06\\/local-postfix-as-relay-to-amazon-ses.html\", \"user_hash\": \"LCJq0b\", \"global_hash\": \"LCJsVy\" }, { \"short_url\": \"http:\\/\\/bit.ly\\/phphotoCrossroads\", \"long_url\": \"http:\\/\\/500px.com\\/photo\\/8337350\", \"user_hash\": \"KShcAV\", \"global_hash\": \"KShcAW\" }, { \"short_url\": \"http:\\/\\/bit.ly\\/springFever\", \"long_url\": \"http:\\/\\/500px.com\\/photo\\/5802359\", \"user_hash\": \"MbmhB4\", \"global_hash\": \"MbmhB5\" }, { \"short_url\": \"http:\\/\\/bit.ly\\/phphotoBenched\", \"long_url\": \"http:\\/\\/500px.com\\/photo\\/8247630\", \"user_hash\": \"LqVZLZ\", \"global_hash\": \"LqW02c\" }, { \"short_url\": \"http:\\/\\/bit.ly\\/Lt5SJo\", \"long_url\": \"http:\\/\\/www.bieberphoto.com\\/index.php\\/2012\\/05\\/15\\/automating-mass-portraits-with-qr-codes\\/\", \"user_hash\": \"Lt5SJo\", \"global_hash\": \"JPNM5X\" }, { \"hash\": \"api-client\", \"long_url\": \"https:\\/\\/github.com\\/stackmagic\\/bitly-api-client\", \"user_hash\": \"LfXpbF\", \"global_hash\": \"LfXpbG\" }, { \"hash\": \"phphotoWinterSunII\", \"long_url\": \"http:\\/\\/500px.com\\/photo\\/7998374\", \"user_hash\": \"LjTbAy\", \"global_hash\": \"LjTbAz\" }, { \"hash\": \"phphotoQuoVadis\", \"long_url\": \"http:\\/\\/500px.com\\/photo\\/7980928\", \"user_hash\": \"JYROvi\", \"global_hash\": \"JYROvj\" }, { \"hash\": \"phphotoDock3\", \"long_url\": \"http:\\/\\/500px.com\\/photo\\/7968427\", \"user_hash\": \"KmfxWg\", \"global_hash\": \"KmfxWh\" }, { \"hash\": \"phphotoZueriWest\", \"long_url\": \"http:\\/\\/500px.com\\/photo\\/7010618\", \"user_hash\": \"LlyMLt\", \"global_hash\": \"LlyN1G\" } ] } }\n"

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

infoResponseTestData :: String
infoResponseTestData = "{\"status_code\": 200, \"data\": {\"info\": [{\"hash\": \"phphotoLakeZurichAtDusk\", \"keyword\": \"phphotoLakeZurichAtDusk\", \"title\": \"500px / Lake Zurich at Dusk by Patrick Huber\", \"created_at\": 1338666104, \"created_by\": \"stackmagic\", \"global_hash\": \"LTlncn\", \"user_hash\": \"LTlncm\"}, {\"hash\": \"phphotoWinterSunII\", \"keyword\": \"phphotoWinterSunII\", \"title\": \"500px / Winter Sun II by Patrick Huber\", \"created_at\": 1338679612, \"created_by\": \"stackmagic\", \"global_hash\": \"LjTbAz\", \"user_hash\": \"LjTbAy\"}, {\"hash\": \"phphotoQuoVadis\", \"keyword\": \"phphotoQuoVadis\", \"title\": \"500px / Quo Vadis? by Patrick Huber\", \"created_at\": 1338679845, \"created_by\": \"stackmagic\", \"global_hash\": \"JYROvj\", \"user_hash\": \"JYROvi\"}, {\"hash\": \"phphotoDock3\", \"keyword\": \"phphotoDock3\", \"title\": \"500px / Photo \\\"Dock 3\\\" by Patrick Huber\", \"created_at\": 1338679895, \"created_by\": \"stackmagic\", \"global_hash\": \"KmfxWh\", \"user_hash\": \"KmfxWg\"}, {\"hash\": \"phphotoZueriWest\", \"keyword\": \"phphotoZueriWest\", \"title\": \"500px / Z\\u00fcri West by Patrick Huber\", \"created_at\": 1338717627, \"created_by\": \"stackmagic\", \"global_hash\": \"LlyN1G\", \"user_hash\": \"LlyMLt\"}, {\"title\": \"swisstech.net: Local postfix as relay to Amazon SES\", \"short_url\": \"http://bit.ly/LCJq0b\", \"created_at\": 1339143918, \"created_by\": \"stackmagic\", \"global_hash\": \"LCJsVy\", \"user_hash\": \"LCJq0b\"}, {\"keyword\": \"phphotoCrossroads\", \"title\": \"500px / Crossroads by Patrick Huber\", \"short_url\": \"http://bit.ly/phphotoCrossroads\", \"created_at\": 1339013462, \"created_by\": \"stackmagic\", \"global_hash\": \"KShcAW\", \"user_hash\": \"KShcAV\"}, {\"keyword\": \"springFever\", \"title\": \"500px / Photo \\\"Spring Fever\\\" by Patrick Huber\", \"short_url\": \"http://bit.ly/springFever\", \"created_at\": 1339051124, \"created_by\": \"stackmagic\", \"global_hash\": \"MbmhB5\", \"user_hash\": \"MbmhB4\"}, {\"keyword\": \"phphotoBenched\", \"title\": \"500px / Benched by Patrick Huber\", \"short_url\": \"http://bit.ly/phphotoBenched\", \"created_at\": 1338760269, \"created_by\": \"stackmagic\", \"global_hash\": \"LqW02c\", \"user_hash\": \"LqVZLZ\"}, {\"title\": \"Automating Mass Portraits With QR Codes  \\u00ab Bieber Photographic\", \"short_url\": \"http://bit.ly/Lt5SJo\", \"created_at\": 1338844882, \"created_by\": \"stackmagic\", \"global_hash\": \"JPNM5X\", \"user_hash\": \"Lt5SJo\"}]}, \"status_txt\": \"OK\"}"


linkLookupRequestTestData :: Request
linkLookupRequestTestData = mkLinkLookupRequest [ "https://www.example.com/"
                                                , "https://www.example.com/1"
                                                , "https://www.example.com/2"
                                                , "https://www.example.com/1"
                                                , "https://www.example.com/2"
                                                ]

linkLookupResponseTestData :: String
linkLookupResponseTestData = "{\"status_code\": 200, \"data\": {\"link_lookup\": [{\"url\": \"https://www.example.com/\", \"aggregate_link\": \"http://bit.ly/maiCS\"}, {\"url\": \"https://www.example.com/1\", \"error\": \"NOT_FOUND\"}, {\"url\": \"https://www.example.com/2\", \"error\": \"NOT_FOUND\"}, {\"url\": \"https://www.example.com/1\", \"error\": \"NOT_FOUND\"}, {\"url\": \"https://www.example.com/2\", \"error\": \"NOT_FOUND\"}]}, \"status_txt\": \"OK\"}"

shortenRequestTestData :: Request
shortenRequestTestData = mkShortenRequest "https://www.example.com/" Nothing

shortenResponseTestData :: String
shortenResponseTestData = "{ \"status_code\": 200, \"status_txt\": \"OK\", \"data\": { \"long_url\": \"https:\\/\\/www.example.com\\/\", \"url\": \"http:\\/\\/bit.ly\\/1hHLtl2\", \"hash\": \"1hHLtl2\", \"global_hash\": \"maiCS\", \"new_hash\": 0 } }\n"

------------------------------------------------------------------------------
{-
let b = (reverse (stripQualifier (mkName "BitlyClientRequests.Request")))
let l = (reverse (stripQualifier (mkName "BitlyClientRequests.Link_SL_LookupRequest")))
let r = stripPrefix b l
-}

namingTests :: Test
namingTests = TestList
    [teq "expand"     (mkOpName base (mkName "BitlyClientRequests.ExpandRequest"))         "expand"
    ,teq "info"       (mkOpName base (mkName "BitlyClientRequests.InfoRequest"))           "info"
    ,teq "linklookup" (mkOpName base (mkName "BitlyClientRequests.Link_SL_LookupRequest")) "link/lookup"
    ,teq "shorten"    (mkOpName base (mkName "BitlyClientRequests.ShortenRequest"))        "shorten"
    ,teq "link_edit"  (mkOpName base (mkName "BitlyClientRequests.Link_EditRequest"))      "link_edit"
    ]
 where
   base = mkName "BitlyClientRequests.Request"

------------------------------------------------------------------------------

makeRequestUrlTests :: Test
makeRequestUrlTests = TestList
    [teq "urlEncodeVars"             (urlEncodeVars encodeTestParams)
                                     "hash=api-client&hash=phphotoWinterSunII&hash=phphotoQuoVadis&hash=phphotoDock3&hash=phphotoZueriWest&shortUrl=http%3A%2F%2Fbit.ly%2FLCJq0b&shortUrl=http%3A%2F%2Fbit.ly%2FphphotoCrossroads&shortUrl=http%3A%2F%2Fbit.ly%2FspringFever&shortUrl=http%3A%2F%2Fbit.ly%2FphphotoBenched&shortUrl=http%3A%2F%2Fbit.ly%2FLt5SJo&access_token=FOOBAR"

    ,teq "makeRequestUrl expand"     (makeRequestUrl expandRequestTestData)
                                     "https://api-ssl.bitly.com/v3/expand?shortUrl=http%3A%2F%2Fbit.ly%2FLCJq0b&shortUrl=http%3A%2F%2Fbit.ly%2FphphotoCrossroads&shortUrl=http%3A%2F%2Fbit.ly%2FspringFever&shortUrl=http%3A%2F%2Fbit.ly%2FphphotoBenched&shortUrl=http%3A%2F%2Fbit.ly%2FLt5SJo&hash=api-client&hash=phphotoWinterSunII&hash=phphotoQuoVadis&hash=phphotoDock3&hash=phphotoZueriWest"

    ,teq "makeRequestUrl expand"     (makeRequestUrl (mkExpandRequest ["u1","u2"] ["h1"]))
                                     "https://api-ssl.bitly.com/v3/expand?shortUrl=u1&shortUrl=u2&hash=h1"

    ,teq "makeRequestUrl info"       (makeRequestUrl infoRequestTestData)
                                     "https://api-ssl.bitly.com/v3/info?hash=phphotoLakeZurichAtDusk&hash=phphotoWinterSunII&hash=phphotoQuoVadis&hash=phphotoDock3&hash=phphotoZueriWest&shortUrl=http%3A%2F%2Fbit.ly%2FLCJq0b&shortUrl=http%3A%2F%2Fbit.ly%2FphphotoCrossroads&shortUrl=http%3A%2F%2Fbit.ly%2FspringFever&shortUrl=http%3A%2F%2Fbit.ly%2FphphotoBenched&shortUrl=http%3A%2F%2Fbit.ly%2FLt5SJo&expand_user=False"

    ,teq "makeRequestUrl linkLookup" (makeRequestUrl linkLookupRequestTestData)
                                     "https://api-ssl.bitly.com/v3/link/lookup?url=https%3A%2F%2Fwww.example.com%2F&url=https%3A%2F%2Fwww.example.com%2F1&url=https%3A%2F%2Fwww.example.com%2F2&url=https%3A%2F%2Fwww.example.com%2F1&url=https%3A%2F%2Fwww.example.com%2F2"

    ,teq "makeRequestUrl shorten"    (makeRequestUrl (mkShortenRequest "u" (Just "d")))
                                     "https://api-ssl.bitly.com/v3/shorten?longUrl=u&domain=d"

    ,teq "makeRequestUrl linkedit"   (makeRequestUrl (mkLinkEditRequest "linkValue" (Just "maybeTitleValue") Nothing (Just True) (Just 3) Nothing ["edit1","edit2"]))
                                     "https://api-ssl.bitly.com/v3/user/link_edit?link=linkValue&title=maybeTitleValue&private=True&user_ts=3&edit=edit1&edit=edit2"
    ]

parseResponseTests :: Test
parseResponseTests = TestList
    [teq "parseResponse expand" ((Right (parseResponse expandResponseTestData)) :: Either String (Either String DataStatusCodeStatusTxt))
                                (Right (Right (DSCST {ddata = ExpandResponseData {expand = [ExpandResponse {er_short_url = Just "http://bit.ly/LCJq0b", er_long_url = "http://blog.swisstech.net/2012/06/local-postfix-as-relay-to-amazon-ses.html", er_user_hash = "LCJq0b", er_global_hash = "LCJsVy", er_hash = Nothing, er_error = Nothing},ExpandResponse {er_short_url = Just "http://bit.ly/phphotoCrossroads", er_long_url = "http://500px.com/photo/8337350", er_user_hash = "KShcAV", er_global_hash = "KShcAW", er_hash = Nothing, er_error = Nothing},ExpandResponse {er_short_url = Just "http://bit.ly/springFever", er_long_url = "http://500px.com/photo/5802359", er_user_hash = "MbmhB4", er_global_hash = "MbmhB5", er_hash = Nothing, er_error = Nothing},ExpandResponse {er_short_url = Just "http://bit.ly/phphotoBenched", er_long_url = "http://500px.com/photo/8247630", er_user_hash = "LqVZLZ", er_global_hash = "LqW02c", er_hash = Nothing, er_error = Nothing},ExpandResponse {er_short_url = Just "http://bit.ly/Lt5SJo", er_long_url = "http://www.bieberphoto.com/index.php/2012/05/15/automating-mass-portraits-with-qr-codes/", er_user_hash = "Lt5SJo", er_global_hash = "JPNM5X", er_hash = Nothing, er_error = Nothing},ExpandResponse {er_short_url = Nothing, er_long_url = "https://github.com/stackmagic/bitly-api-client", er_user_hash = "LfXpbF", er_global_hash = "LfXpbG", er_hash = Just "api-client", er_error = Nothing},ExpandResponse {er_short_url = Nothing, er_long_url = "http://500px.com/photo/7998374", er_user_hash = "LjTbAy", er_global_hash = "LjTbAz", er_hash = Just "phphotoWinterSunII", er_error = Nothing},ExpandResponse {er_short_url = Nothing, er_long_url = "http://500px.com/photo/7980928", er_user_hash = "JYROvi", er_global_hash = "JYROvj", er_hash = Just "phphotoQuoVadis", er_error = Nothing},ExpandResponse {er_short_url = Nothing, er_long_url = "http://500px.com/photo/7968427", er_user_hash = "KmfxWg", er_global_hash = "KmfxWh", er_hash = Just "phphotoDock3", er_error = Nothing},ExpandResponse {er_short_url = Nothing, er_long_url = "http://500px.com/photo/7010618", er_user_hash = "LlyMLt", er_global_hash = "LlyN1G", er_hash = Just "phphotoZueriWest", er_error = Nothing}]}, status_code = 200, status_txt = "OK"})) :: Either String (Either String DataStatusCodeStatusTxt))


    ,teq "parseResponse info"   ((Right (parseResponse infoResponseTestData)) :: Either String (Either String DataStatusCodeStatusTxt))
                                (Right (Right (DSCST {ddata = InfoResponseData {info = [InfoResponse {ir_created_by = "stackmagic", ir_created_at = 1338666104, ir_short_url = Nothing, ir_hash = Just "phphotoLakeZurichAtDusk", ir_user_hash = Just "LTlncm", ir_global_hash = Just "LTlncn", ir_error = Nothing, ir_title = Just "500px / Lake Zurich at Dusk by Patrick Huber"},InfoResponse {ir_created_by = "stackmagic", ir_created_at = 1338679612, ir_short_url = Nothing, ir_hash = Just "phphotoWinterSunII", ir_user_hash = Just "LjTbAy", ir_global_hash = Just "LjTbAz", ir_error = Nothing, ir_title = Just "500px / Winter Sun II by Patrick Huber"},InfoResponse {ir_created_by = "stackmagic", ir_created_at = 1338679845, ir_short_url = Nothing, ir_hash = Just "phphotoQuoVadis", ir_user_hash = Just "JYROvi", ir_global_hash = Just "JYROvj", ir_error = Nothing, ir_title = Just "500px / Quo Vadis? by Patrick Huber"},InfoResponse {ir_created_by = "stackmagic", ir_created_at = 1338679895, ir_short_url = Nothing, ir_hash = Just "phphotoDock3", ir_user_hash = Just "KmfxWg", ir_global_hash = Just "KmfxWh", ir_error = Nothing, ir_title = Just "500px / Photo \"Dock 3\" by Patrick Huber"},InfoResponse {ir_created_by = "stackmagic", ir_created_at = 1338717627, ir_short_url = Nothing, ir_hash = Just "phphotoZueriWest", ir_user_hash = Just "LlyMLt", ir_global_hash = Just "LlyN1G", ir_error = Nothing, ir_title = Just "500px / Z\252ri West by Patrick Huber"},InfoResponse {ir_created_by = "stackmagic", ir_created_at = 1339143918, ir_short_url = Just "http://bit.ly/LCJq0b", ir_hash = Nothing, ir_user_hash = Just "LCJq0b", ir_global_hash = Just "LCJsVy", ir_error = Nothing, ir_title = Just "swisstech.net: Local postfix as relay to Amazon SES"},InfoResponse {ir_created_by = "stackmagic", ir_created_at = 1339013462, ir_short_url = Just "http://bit.ly/phphotoCrossroads", ir_hash = Nothing, ir_user_hash = Just "KShcAV", ir_global_hash = Just "KShcAW", ir_error = Nothing, ir_title = Just "500px / Crossroads by Patrick Huber"},InfoResponse {ir_created_by = "stackmagic", ir_created_at = 1339051124, ir_short_url = Just "http://bit.ly/springFever", ir_hash = Nothing, ir_user_hash = Just "MbmhB4", ir_global_hash = Just "MbmhB5", ir_error = Nothing, ir_title = Just "500px / Photo \"Spring Fever\" by Patrick Huber"},InfoResponse {ir_created_by = "stackmagic", ir_created_at = 1338760269, ir_short_url = Just "http://bit.ly/phphotoBenched", ir_hash = Nothing, ir_user_hash = Just "LqVZLZ", ir_global_hash = Just "LqW02c", ir_error = Nothing, ir_title = Just "500px / Benched by Patrick Huber"},InfoResponse {ir_created_by = "stackmagic", ir_created_at = 1338844882, ir_short_url = Just "http://bit.ly/Lt5SJo", ir_hash = Nothing, ir_user_hash = Just "Lt5SJo", ir_global_hash = Just "JPNM5X", ir_error = Nothing, ir_title = Just "Automating Mass Portraits With QR Codes  \171 Bieber Photographic"}]}, status_code = 200, status_txt = "OK"})) :: Either String (Either String DataStatusCodeStatusTxt))

    ,teq "parseResponse linkLookup" ((Right (parseResponse linkLookupResponseTestData)) :: Either String (Either String DataStatusCodeStatusTxt))
                                    (Left "LINKLOOKUP")

    ,teq "parseResponse shorten" ((Right (parseResponse shortenResponseTestData)) :: Either String (Either String DataStatusCodeStatusTxt))
                                 (Left "SHORTEN")
    ]

remoteTests :: Test
remoteTests = TestList
    [teq "doRequest expand"     (unsafePerformIO (doRequest     expandRequestTestData)) (L.pack     expandResponseTestData)
    ,teq "doRequest info"       (unsafePerformIO (doRequest       infoRequestTestData)) (L.pack       infoResponseTestData)
    ,teq "doRequest linkLookup" (unsafePerformIO (doRequest linkLookupRequestTestData)) (L.pack linkLookupResponseTestData)
    ,teq "doRequest shorten"    (unsafePerformIO (doRequest    shortenRequestTestData)) (L.pack    shortenResponseTestData)
    ]

------------------------------------------------------------------------------

runLocalTests :: IO Counts
runLocalTests = do
    _ <- runTestTT namingTests
    _ <- runTestTT makeRequestUrlTests
    runTestTT parseResponseTests

runRemoteTests :: IO Counts
runRemoteTests = runTestTT remoteTests

runTests :: IO Counts
runTests = do
    _ <- runLocalTests
    runRemoteTests

-- End of file.
