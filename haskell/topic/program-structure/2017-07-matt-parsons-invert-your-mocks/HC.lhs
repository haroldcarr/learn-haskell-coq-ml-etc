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
> import           Universum

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
>   =>                        m Query   -- ^ runHTTP
>   -> (Query              -> m [User]) -- ^ runDB
>   -> (User               -> m Thing)  -- ^ getSomething
>   -> (RedisKey -> Result -> m ())     -- ^ runRedis
>   ->                        m ()
> runAbstract runhttp rundb getsomething runredis = do
>   query <- runhttp
>   users <- rundb query
>   for_ users $ \user -> do
>     thing <- getsomething user
>     let result = compute thing
>     runredis (userRedisKey user) result

production IO version

> runIO :: App ()
> runIO =
>   runAbstract
>     (runHTTP getUserQuery)
>     (runDB . usersSatisfying)
>     getSomethingApp
>     (\key result -> runRedis (writeKey key result))

> runIt :: IO (Either AppError ())
> runIt = runApp runIO

test version

> data TestCtx = TestCtx [Text] (Map Query [User])
>
> test :: (MonadState TestCtx m, MonadReader AppCtx m, MonadWriter [Text] m) => m ()
> test = do
>   runAbstract runhttp rundb getsomething runredis
>   runAbstract runhttp rundb getsomething runredis
>  where
>   runhttp = do
>     c <- asks httpConn
>     tell ["runHTTP " <> c]
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
>     pure (getSomething u)
>   runredis k v = do
>     c <- asks redisConn
>     tell ["runRedis: " <> c <> " : " <> show k <> "/" <> show v]
>
> testCtx :: TestCtx
> testCtx = TestCtx ["Q1", "Q2"]
>                   (M.fromList [ (Query "Q1", [User "Q1X", User "Q1Y"])
>                               , (Query "Q2", [User "Q2X", User "Q2Y"])
>                               ])
>
> runTest :: [Text]
> runTest = execWriterT (execStateT test testCtx) appCtx

Note: above does NOT use: mtl, Eff, type classes, ...

------------------------------------------------------------------------------
-- support for above

> getUserQuery :: Text
> getUserQuery =
>   "USER-HC"
>   --"runHTTPBAD"
>   --"runDBBAD"
>   --"getSomethingBAD"
>   --"runRedisBAD"
>
> data Query
>   = Query  Text
>   | AnyUserQuery
>   deriving (Eq, Ord, Show)
>
> runHTTP :: Text -> App Query
> runHTTP x = do
>   when (x == "runHTTPBAD") $ throwError (AppError "runHTTP FAILED")
>   c <- asks httpConn
>   putStrLn ("runHTTP " <> c <> " : " <> x)
>   return (Query x)
>
> usersSatisfying :: Query -> Query
> usersSatisfying = id
>
> newtype User = User Text deriving Show
>
> runDB :: Query -> App [User]
> runDB (Query "runDBBAD") = throwError (AppError "runDB FAILED")
> runDB q = do
>   let r = case q of
>             (Query x)    -> [User x]
>             AnyUserQuery -> [User "any"]
>   c <- asks dbConn
>   putStrLn ("runDB " <> c <> " : " <> show q)
>   return  r
>
> newtype Thing = Thing Text deriving Show
>
> getSomething :: User -> Thing
> getSomething (User x) = Thing x
>
> getSomethingApp :: User -> App Thing
> getSomethingApp (User "getSomethingBAD") = throwError (AppError "getSomething FAILED")
> getSomethingApp x = return (getSomething x)
>
> newtype Result = Result Text  deriving Show
>
> compute :: Thing -> Result
> compute (Thing x) = Result x
>
> data RedisKey = RedisKey deriving Show
>
> userRedisKey :: User -> RedisKey
> userRedisKey _ = RedisKey
>
> writeKey :: RedisKey -> Result -> Result
> writeKey _ r = r
>
> runRedis :: Result -> App ()
> runRedis r@(Result x) = do
>   when (x == "runRedisBAD") $ throwError (AppError "runRedis FAILED")
>   c <- asks redisConn
>   putStrLn ("runRedis " <> c <> " : " <> show r)
>   return ()
