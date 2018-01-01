> {-# LANGUAGE DataKinds #-}
>
> module E2_usage where
>
> import           Data.Proxy
> import qualified Database.Redis as Hedis
> import           E1_impl

https://hackage.haskell.org/package/edis-0.0.1.0/docs/Database-Edis.html

> e1 :: IO (Either Hedis.Reply (Maybe String))
> e1  = do
>   -- connects to localhost:6379
>   conn <- Hedis.connect Hedis.defaultConnectInfo
>   -- Send commands to the server
>   Hedis.runRedis conn $ unEdis $ start
>     `bind` \_ -> set    (Proxy :: Proxy "hello")    True
>     `bind` \_ -> set    (Proxy :: Proxy "hello")    "foo"
>     `bind` \_ -> set    (Proxy :: Proxy "world")    [True, False]
>     `bind` \_ -> set    (Proxy :: Proxy "world")    "bar"
>     `bind` \_ -> del    (Proxy :: Proxy "hello")
>     `bind` \_ -> get    (Proxy :: Proxy "world")
> --    `bind` \_ -> flushall

> e2 :: IO (Either Hedis.Reply (Maybe [Int]))
> e2  = do
>   -- connects to localhost:6379
>   conn <- Hedis.connect Hedis.defaultConnectInfo
>   -- Send commands to the server
>   Hedis.runRedis conn $ unEdis $ start
>     `bind` \_ -> set    (Proxy :: Proxy "hello")    [1,2,3]
>     `bind` \_ -> get    (Proxy :: Proxy "hello")
