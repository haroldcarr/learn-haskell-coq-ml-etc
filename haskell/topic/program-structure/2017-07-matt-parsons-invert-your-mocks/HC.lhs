> {-# LANGUAGE DeriveFunctor              #-}
> {-# LANGUAGE FlexibleContexts           #-}
> {-# LANGUAGE FlexibleInstances          #-}
> {-# LANGUAGE GeneralizedNewtypeDeriving #-}
> {-# LANGUAGE MultiParamTypeClasses      #-}
> {-# LANGUAGE NoImplicitPrelude          #-}
> {-# LANGUAGE OverloadedStrings          #-}
> {-# LANGUAGE TypeSynonymInstances       #-}

> module HC where
>
> import           Control.Monad.Except
> import           Control.Monad.Reader
> import           Control.Monad.Writer   hiding ((<>))
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
>   query <- runhttp getUserInput
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

> instance MonadError AppError (Either AppError) where
>   throwError              = Left
>   catchError (Left err) h = h err
>   catchError r          _ = r
>
> test :: ( MonadState  Int      m
>         , MonadReader AppCtx   m
>         , MonadWriter [Text]   m
>         , MonadError  AppError m)
>      =>                        m ()
> test = {-do-} it -- ; it; it; it; it
>  where
>   it = runAbstract runhttp rundb getsomething runredis
>   runhttp "runHTTPBAD" = throwError (AppError "runHTTP FAILED")
>   runhttp x = do   -- ignore input
>     modify' (+1)
>     c <- asks httpConn
>     tell ["runHTTP " <> c <> " : " <> x]
>     pure (Query x)
>   rundb (Query "runDBBAD") = throwError (AppError "runDB FAILED")
>   rundb q@(Query x) = do
>     modify' (+1)
>     c <- asks dbConn
>     tell ["runDB " <> c <> " : " <> show q]
>     pure [User x, User "y"]
>   getsomething (User "getSomethingBAD") = throwError (AppError "getSomething FAILED")
>   getsomething u = do
>     modify' (+1)
>     tell ["getSomething " <> show u]
>     pure (getSomethingPure u)
>   runredis (WriteResult _ (Result "runRedisBAD")) = throwError (AppError "runRedis FAILED")
>   runredis r = do
>     modify' (+1)
>     c <- asks redisConn
>     tell ["runRedis: " <> c <> " : " <> show r]
>
> rt,runTest :: IO ()
> rt = runTest
> runTest = do
>   let (e,w) = runWriterT (runExceptT (execStateT test 0)) appCtx
>   print e
>   U.mapM_ go w
>  where go x = do
>          putStrLn ("-----------------------"::Text)
>          putStrLn x

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
> userInputs :: [Text]
> userInputs = ["USER-HC", "runHTTPBAD", "runDBBAD", "getSomethingBAD", "runRedisBAD"]
>
> {-# ANN getUserInput ("HLint: ignore Use head" :: String) #-}
> getUserInput :: Text
> getUserInput = userInputs P.!! 0
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

> -- | https://stackoverflow.com/a/16379034/814846
> rotate :: Int -> [a] -> [a]
> rotate _ [] = []
> rotate n xs = zipWith const (drop n (cycle xs)) xs
