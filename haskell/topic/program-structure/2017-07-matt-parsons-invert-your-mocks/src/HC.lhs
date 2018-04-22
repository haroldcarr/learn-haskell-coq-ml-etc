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
> import           Test.Hspec             hiding (runIO)
> import           Universum              as U

> {-# ANN module ("HLint: ignore Reduce duplication" :: String) #-}

> data AppCtx = AppCtx
>   { httpConn  :: Text
>   , dbConn    :: Text
>   , redisConn :: Text
>   }
>
> newtype AppError = AppError Text deriving (Eq, Show)
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
>   =>                 m Text    -- ^ getUserInput
>   -> (Text        -> m Query)  -- ^ runHTTP
>   -> (Query       -> m [User]) -- ^ runDB
>   -> (User        -> m Thing)  -- ^ getSomething
>   -> (WriteResult -> m ())     -- ^ runRedis
>   ->                 m ()
> runAbstract getuserinput runhttp rundb getsomething runredis = do
>   input <- getuserinput
>   query <- runhttp input
>   users <- rundb (usersSatisfying query)
>   for_ users $ \user -> do
>     thing <- getsomething user
>     let result = compute thing
>         key    = userRedisKey user
>         wkr    = writeKey key result
>     runredis wkr

production IO version

> runIO :: Int -> App ()
> runIO i = runAbstract (getUserInput i) runHTTP runDB getSomething runRedis
>
> rio :: IO [Either AppError ()]
> rio = mapM (runApp . runIO) [0 .. 4]

test version

> instance MonadError AppError (Either AppError) where
>   throwError              = Left
>   catchError (Left err) h = h err
>   catchError r          _ = r
>
> test :: ( MonadState  (Int,Int) m
>         , MonadReader AppCtx    m
>         , MonadWriter [Text]    m
>         , MonadError  AppError  m)
>      =>                         m ()
> test = runAbstract getuserinput runhttp rundb getsomething runredis
>  where
>   getuserinput = do
>     (i, _) <- get
>     modify (bimap id (+1))
>     let ui = getUserInputPure i
>     tell ["getUserInput " <> ui]
>     pure ui
>   runhttp "runHTTPBAD" = throwError (AppError "runHTTP FAILED")
>   runhttp x = do   -- ignore input
>     modify (bimap id (+1))
>     c <- asks httpConn
>     tell ["runHTTP " <> c <> " : " <> x]
>     pure (Query x)
>   rundb (Query "runDBBAD") = throwError (AppError "runDB FAILED")
>   rundb q@(Query x) = do
>     modify (bimap id (+1))
>     c <- asks dbConn
>     tell ["runDB " <> c <> " : " <> show q]
>     pure [User x, User "y"]
>   getsomething (User "getSomethingBAD") = throwError (AppError "getSomething FAILED")
>   getsomething u = do
>     modify (bimap id (+1))
>     tell ["getSomething " <> show u]
>     pure (getSomethingPure u)
>   runredis (WriteResult _ (Result "runRedisBAD")) = throwError (AppError "runRedis FAILED")
>   runredis r = do
>     modify (bimap id (+1))
>     c <- asks redisConn
>     tell ["runRedis: " <> c <> " : " <> show r]
>
> runTest :: Int -> (Either AppError (Int, Int), [Text])
> runTest i = runWriterT (runExceptT (execStateT test (i,0))) appCtx
>
> rat :: [(Either AppError (Int, Int), [Text])]
> rat = map runTest [0 .. 4]
>
> st :: IO ()
> st =  U.mapM_ go rat
>  where
>   go (e,w) = do
>     putStrLn ("======================="::Text)
>     print e
>     U.mapM_ go' w
>   go' x = do
>     putStrLn ("-----------------------"::Text)
>     putStrLn x

Note: above does NOT use: mtl, Eff, type classes, ...

> testIt :: Spec
> testIt =
>   describe "testIt" $
>     it "works" $
>       rat `shouldBe`
>       [ (Right (0,7)
>         ,["getUserInput USER-HC"
>          ,"runHTTP my-http-conn : USER-HC"
>          ,"runDB my-db-conn : Query \"USER-HC\""
>          ,"getSomething User \"USER-HC\""
>          ,"runRedis: my-redis-conn : WriteResult RedisKey (Result \"USER-HC\")"
>          ,"getSomething User \"y\""
>          ,"runRedis: my-redis-conn : WriteResult RedisKey (Result \"y\")"
>          ])
>       , (Left (AppError "runHTTP FAILED")
>         ,[ "getUserInput runHTTPBAD" ])
>       , (Left (AppError "runDB FAILED")
>         ,["getUserInput runDBBAD"
>          ,"runHTTP my-http-conn : runDBBAD"])
>       ,(Left (AppError "getSomething FAILED")
>        ,["getUserInput getSomethingBAD"
>         ,"runHTTP my-http-conn : getSomethingBAD"
>         ,"runDB my-db-conn : Query \"getSomethingBAD\""])
>       ,(Left (AppError "runRedis FAILED")
>        ,["getUserInput runRedisBAD"
>         ,"runHTTP my-http-conn : runRedisBAD"
>         ,"runDB my-db-conn : Query \"runRedisBAD\""
>         ,"getSomething User \"runRedisBAD\""])]


------------------------------------------------------------------------------
-- effects

> getUserInput :: Int -> App Text
> getUserInput i = return (userInputs P.!! i)
>
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
> getUserInputPure :: Int -> Text
> getUserInputPure i = userInputs P.!! i
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
