> {-# OPTIONS_GHC -Wno-warnings-deprecations   #-}
> {-# LANGUAGE DeriveFunctor              #-}
> {-# LANGUAGE FlexibleContexts           #-}
> {-# LANGUAGE FlexibleInstances          #-}
> {-# LANGUAGE GeneralizedNewtypeDeriving #-}
> {-# LANGUAGE NoImplicitPrelude          #-}
> {-# LANGUAGE OverloadedStrings          #-}
> {-# LANGUAGE TypeSynonymInstances       #-}

> module Lib where
>
> import           Control.Monad.Except
> import           Control.Monad.Reader
> import           Control.Monad.Writer hiding ((<>))
> import           Data.Conduit
> import qualified Data.Conduit.List as CL
> import           Database.Persist.Sql (SqlPersistT)
> import qualified Data.Text         as T
> -- import           Test.QuickCheck (Gen, arbitrary)
> import           Universum

> {-# ANN module ("HLint: ignore Reduce duplication" :: String) #-}

Invert Your Mocks
27 Jul 2017
http://www.parsonsmatt.org/2017/07/27/inverted_mocking.html

advantages of mtl, Eff freer monads : swap implementations; but heavy-weight/complex

instead

> data AppCtx = AppCtx
>
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
>   deriving (Eq, Show)
>
> runHTTP :: Text -> App Query
> runHTTP x = do
>   when (x == "runHTTPBAD") $ throwError (AppError "runHTTP FAILED")
>   putStrLn ("runHTTP " <> x)
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
>   putStrLn ("runDB " <> T.pack (show q))
>   return  r
>
> newtype Thing = Thing Text deriving Show
>
> getSomething :: User -> App Thing
> getSomething (User "getSomethingBAD") = throwError (AppError "getSomething FAILED")
> getSomething (User x) = return (Thing x)
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
>   putStrLn ("runRedis " <> T.pack (show r))
>   return ()

> fakeSomethingFor :: User -> Thing
> fakeSomethingFor (User x) = Thing x

> selectList :: a
> selectList = undefined
> convertToQuery :: a
> convertToQuery = undefined
> setUserId :: a
> setUserId = undefined
> setUserEmail :: a
> setUserEmail = undefined

> -- HC
> newtype AppError = AppError Text deriving Show

> newtype App a = App { unApp :: ReaderT AppCtx (ExceptT AppError IO) a }
>   deriving (Functor, Applicative, Monad,
>             -- HC: added
>             MonadIO,
>             MonadReader AppCtx,
>             MonadError AppError
>             )

> runApp :: App a -> IO (Either AppError a)
> runApp a = runExceptT $ runReaderT (unApp a) AppCtx

cost: must manually lift to use an App function inside of a Conduit, MaybeT, ... block

testing

> doWork1 :: App ()
> doWork1 = do
>   query <- runHTTP getUserQuery
>   users <- runDB (usersSatisfying query)
>   for_ users $ \user -> do
>     thing <- getSomething user
>     let result = compute thing
>     runRedis (writeKey (userRedisKey user) result)

If using mtl, Eff, etc: need to mock HTTP, database, Redis effects.

Instead.

Decomposing Effects

effects and values are separate: keep them as separate as possible

functions at look like:

    doWork :: App ()

are not functional

one approach : get all the inputs from effects

> doWork2 :: App ()
> doWork2 = do
>   query <- runHTTP getUserQuery
>   users <- runDB (usersSatisfying query)
>   doWorkHelper2 users

to reduce mocking only getSomething and runRedis in the following

> doWorkHelper2 :: [User] -> App ()
> doWorkHelper2 users =
>   for_ users $ \user -> do
>     thing <- getSomething user
>     let result = compute thing
>     runRedis (writeKey (userRedisKey user) result)

Can get rid of the getSomething by factoring another helper out.

> doWorkHelper3 :: [User] -> App ()
> doWorkHelper3 users = do
>   things'users <- forM users $ \user -> do
>     thing <- getSomething user
>     pure (thing, user)
>   lookMaNoInputs3 things'users
>
> lookMaNoInputs3 :: [(Thing, User)] -> App ()
> lookMaNoInputs3 things'users =
>   for_ things'users $ \(thing, user) -> do
>     let result = compute thing
>     runRedis (writeKey (userRedisKey user) result)

above factors out all INPUT EFFECTS

do the pure stuff

> businessLogic4 :: (Thing, User) -> (RedisKey, Result)
> businessLogic4  (thing, user) = (userRedisKey user, computeIt thing user)
>  where computeIt _ _ = Result "x"

and give to the output effect

    runRedis (writeKey (userRedisKey user) result)

> lookMaNoInputs4 :: [(Thing, User)] -> App ()
> lookMaNoInputs4 users =
>   for_ (map businessLogic4 users) $ \(key, result) ->
>     runRedis (writeKey key result)

above factors pure code from effect code

------------------------------------------------------------------------------

> doWorkAbstract5
>   :: Monad m
>   =>                        m Query   -- ^ runHTTP getUserQuery
>   -> (Query              -> m [User]) -- ^ database action
>   -> (User               -> m Thing)  -- ^ getSomething
>   -> (RedisKey -> Result -> m ())     -- ^ redis action
>   ->                        m ()
> doWorkAbstract5 getUserQuery0 getUsers0 getSomething0 redisAction0 = do
>   query <- getUserQuery0
>   users <- getUsers0 query
>   for_ users $ \user -> do
>     thing <- getSomething0 user
>     let result = compute thing
>     redisAction0 (userRedisKey user) result

- parameterized over any monad : Identity, State, IO, ..., you choose
- pure specification of effect logic (i.e., dependency injection)

> doWork5 :: App ()
> doWork5 =
>   doWorkAbstract5
>     (runHTTP getUserQuery)
>     (runDB . usersSatisfying)
>     getSomething
>     (\key result -> runRedis (writeKey key result))

> doWorkScribe :: Writer [String] ()
> doWorkScribe =
>   doWorkAbstract5 getQ getUsers0 getSomething0 redis0
>  where
>   getQ = do
>     tell ["getting users query"]
>     pure AnyUserQuery
>   getUsers0 _ = do
>     tell ["getting users"]
>     pure [User "X", User "Y"]
>   getSomething0 u = do
>     tell ["getting something for " <> show u]
>     pure (fakeSomethingFor u)
>   redis0 k v = do
>     tell ["wrote k: " <> show k]
>     tell ["wrote v: " <> show v]
>
> runW :: [String]
> runW = execWriter doWorkScribe

Note: above does NOT use: mtl, Eff, type classes, ...

------------------------------------------------------------------------------

separate effects from pure/business logic

if you must test effectful code
- don't create a mock as complex as the thing you are mocking

instead:

> type UserId = Text
> type Email  = Text
> data UserQuery
>   = AllUsers
>   | UserById    UserId
>   | UserByEmail Email
>
> class Monad m => GetUsers m where
>   runUserQuery :: UserQuery -> m [User]
>
> instance MonadIO m => GetUsers (SqlPersistT m) where
>   runUserQuery = selectList . convertToQuery
> {-
> instance GetUsers Gen where
>   runUserQuery query =
>     case query of
>       AllUsers              -> arbitrary
>       UserById    userId    -> take 1 . fmap (setUserId    userId)    <$> arbitrary
>       UserByEmail userEmail -> take 1 . fmap (setUserEmail userEmail) <$> arbitrary
> -}

Or, manually pass functions instead of type class

------------------------------------------------------------------------------
Decomposition: Conduit-style

> data RealThing = RealThing deriving Show
> parseFromByteString :: ByteString -> RealThing
> parseFromByteString = undefined
> convertSomeThing :: RealThing -> RealThing
> convertSomeThing = undefined
> someFilterCondition :: RealThing -> Bool
> someFilterCondition = undefined
> makeHttpPost :: a
> makeHttpPost = undefined
> saveToDatabase :: a
> saveToDatabase = undefined

Pipes, Conduit : can decompose functions and provide "inverted mocking"

> streamSomeStuff :: IO ()
> streamSomeStuff =
>   runConduit
>      $ conduitThatGetsStuff
>     .| conduitThatProcessesStuff
>     .| conduitThatConsumesStuff

each part of this conduit can itself have many conduits inside of it:

> conduitThatGetsStuff :: Producer IO ByteString
> conduitThatGetsStuff = undefined
>
> -- | does not care where ByteString comes from: e.g., CL.sourceList [ex1, ex2, ex3]
> conduitThatProcessesStuff :: Conduit ByteString IO RealThing
> conduitThatProcessesStuff =
>   CL.map parseFromByteString
>   .| CL.map    convertSomeThing
>   .| CL.filter someFilterCondition
>
> passThrough :: (a -> IO ()) -> Conduit a IO a
> passThrough action = CL.mapM $ \a -> do
>   action a
>   pure a
>
> -- | does not care where RealThing come from: e.g., : CL.sourceList ...
> conduitThatConsumesStuff :: Consumer RealThing IO ()
> conduitThatConsumesStuff =
>   passThrough print
>   .| passThrough makeHttpPost
>   .| CL.mapM_ saveToDatabase


not usually working directly with Conduits
functions usually provided to CL.mapM_, CL.filter, CL.map
enables writing functions
  a -> m b
  a -> Bool
  a -> b

