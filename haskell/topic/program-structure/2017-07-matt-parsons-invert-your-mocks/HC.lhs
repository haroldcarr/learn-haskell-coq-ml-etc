> {-# LANGUAGE DeriveFunctor              #-}
> {-# LANGUAGE FlexibleContexts           #-}
> {-# LANGUAGE FlexibleInstances          #-}
> {-# LANGUAGE GeneralizedNewtypeDeriving #-}
> {-# LANGUAGE NoImplicitPrelude          #-}
> {-# LANGUAGE OverloadedStrings          #-}
> {-# LANGUAGE TypeSynonymInstances       #-}

> module HC where
>
> import           Control.Monad.Except
> import           Control.Monad.Reader
> import           Control.Monad.Writer   hiding ((<>))
> import qualified Data.Map.Strict        as M
> import qualified Prelude                as P
> import           Universum              as U

> {-# ANN module ("HLint: ignore Reduce duplication" :: String) #-}

> data AppCtx = AppCtx
>   { httpConn  :: Text
>   , dbConn    :: Text
>   , redisConn :: Text
>   }
>
> newtype AppError = AppError Text deriving Show
>
> newtype App a = App { unApp :: ReaderT AppCtx (ExceptT AppError IO) a }
>   deriving (Functor, Applicative, Monad,
>             MonadReader AppCtx,
>             MonadError AppError,
>             MonadIO)
>
> appCtx :: AppCtx
> appCtx = AppCtx "my-http-conn" "my-db-conn" "my-redis-conn"
>
> runApp :: App a -> IO (Either AppError a)
> runApp a = runExceptT $ runReaderT (unApp a) appCtx

cost: must manually lift to use an App function inside of a Conduit, MaybeT, ...

- parameterized over any monad : Identity, State, IO, ..., you choose
- pure specification of effect logic (i.e., dependency injection)

> runAbstract
>   :: Monad m
>   => (Text        -> m Query)  -- ^ runHTTP
>   -> (Query       -> m [User]) -- ^ runDB
>   -> (User        -> m Thing)  -- ^ getSomething
>   -> (WriteResult -> m ())     -- ^ runRedis
>   ->                 m ()
> runAbstract runhttp rundb getsomething runredis = do
>   query <- runhttp getUserQuery
>   users <- rundb (usersSatisfying query)
>   for_ users $ \user -> do
>     thing <- getsomething user
>     let result = compute thing
>         key    = userRedisKey user
>         wkr    = writeKey key result
>     runredis wkr

production IO version

> runIO :: App ()
> runIO = do it; it
>  where
>   it = runAbstract runHTTP runDB getSomething runRedis

> runIt :: IO (Either AppError ())
> runIt = runApp runIO

test version

> data TestCtx = TestCtx [Text] (Map Query [User])
>
> test :: (MonadState TestCtx m, MonadReader AppCtx m, MonadWriter [Text] m) => m ()
> test = do it; it; it; it; it
>  where
>   it = runAbstract runhttp rundb getsomething runredis
>   runhttp xx = do   -- ignore input
>     c <- asks httpConn
>     tell ["runHTTP " <> c <> " : " <> xx]
>     TestCtx (q:qs) x <- get
>     put (TestCtx qs x)
>     pure (Query q)
>   rundb q = do
>     c <- asks dbConn
>     tell ["runDB " <> c <> " : " <> show q]
>     TestCtx _ m <- get
>     let us = fromMaybe [] (M.lookup q m)
>     pure us
>   getsomething u = do
>     tell ["getSomething " <> show u]
>     pure (getSomethingPure u)
>   runredis r = do
>     c <- asks redisConn
>     tell ["runRedis: " <> c <> " : " <> show r]
>
> testCtx :: TestCtx
> testCtx = TestCtx userQueries
>                   (M.fromList [ (Query "USER-HC"         , [User "QHC1"  , User "QHC2"])
>                               , (Query "runHTTPBAD"      , [User "QHTTP1", User "QHTTP2"])
>                               , (Query "runDBBAD"        , [User "QDB1"  , User "QDB2"])
>                               , (Query "getSomethingBAD" , [User "QGS1"  , User "QGS2"])
>                               , (Query "runRedisBAD"     , [User "QRED1" , User "QRED2"])
>                               ])
>
> runTest :: IO ()
> runTest = do
>   let r = execWriterT (execStateT test testCtx) appCtx
>   U.mapM_ putStrLn r

Note: above does NOT use: mtl, Eff, type classes, ...

------------------------------------------------------------------------------
-- effects

> runHTTP :: Text -> App Query
> runHTTP x = do
>   when (x == "runHTTPBAD") $ throwError (AppError "runHTTP FAILED")
>   c <- asks httpConn
>   putStrLn ("runHTTP " <> c <> " : " <> x)
>   return (Query x)
>
> runDB :: Query -> App [User]
> runDB (Query "runDBBAD") = throwError (AppError "runDB FAILED")
> runDB q@(Query x) = do
>   c <- asks dbConn
>   putStrLn ("runDB " <> c <> " : " <> show q)
>   return [User x]
>
> getSomething :: User -> App Thing
> getSomething (User "getSomethingBAD") = throwError (AppError "getSomething FAILED")
> getSomething x = return (getSomethingPure x)
>
> runRedis :: WriteResult -> App ()
> runRedis r@(WriteResult _ (Result x)) = do
>   when (x == "runRedisBAD") $ throwError (AppError "runRedis FAILED")
>   c <- asks redisConn
>   putStrLn ("runRedis " <> c <> " : " <> show r)
>   return ()

------------------------------------------------------------------------------
-- types and pure

> newtype Query       = Query  Text  deriving (Eq, Ord, Show)
> newtype User        = User   Text  deriving Show
> newtype Thing       = Thing  Text  deriving Show
> newtype Result      = Result Text  deriving Show
> data    RedisKey    = RedisKey     deriving Show
> data    WriteResult = WriteResult RedisKey Result deriving Show
>
> userQueries :: [Text]
> userQueries = ["USER-HC", "runHTTPBAD", "runDBBAD", "getSomethingBAD", "runRedisBAD"]
>
> {-# ANN getUserQuery ("HLint: ignore Use head" :: String) #-}
> getUserQuery :: Text
> getUserQuery = userQueries P.!! 0
>
> getSomethingPure :: User -> Thing
> getSomethingPure (User x) = Thing x
>
> usersSatisfying :: Query -> Query
> usersSatisfying = id
>
> compute :: Thing -> Result
> compute (Thing x) = Result x
>
> userRedisKey :: User -> RedisKey
> userRedisKey _ = RedisKey
>
> writeKey :: RedisKey -> Result -> WriteResult
> writeKey = WriteResult
