> {-# LANGUAGE FlexibleContexts           #-}
> {-# LANGUAGE GeneralizedNewtypeDeriving #-}
>
> module Lib where
>
> import Control.Monad.IO.Class
> import Control.Monad.Reader
> import Control.Monad.Trans.Either

------------------------------------------------------------------------------
The Precursor: ReaderT-based Transformer Stack

talk that covers many of same points
- Next Level MTL by George Wilson
- https://www.youtube.com/watch?v=GZPup5Iuaqw

most apps need piece of read-only state, e.g.,
- database connection pools
- application configuration
- logging environment

ReaderT-based monad transformer good fit
- logging environment : add namespaces; pause logging
- use MonadReader local combinator to temporarily modify/restore reader context

GeneralizedNewtypeDeriving (above)
- enables piggybacking on ReaderT’s instances
  - if ReaderT r m a has instance, then app can get it without any boilerplate

> data AppState1 = AppState1 Int

> newtype AppT1 m a = AppT1 { unAppT1 :: ReaderT AppState1 m a}
>   deriving (Applicative, Functor, Monad, MonadIO, MonadReader AppState1)

> -- unAppT1     : unwraps top level app function to ReaderT AppState1 m a
> -- runReaderT : unwraps to AppState1 -> m a
> -- AppState1 given to top level that returns m a
> runAppT1 :: AppState1 -> AppT1 m a -> m a
> runAppT1 s m = runReaderT (unAppT1 m) s

> top1 :: AppT1 IO Int
> top1 = do
>   AppState1 x <- ask
>   return (x + 1)

> testTop1 = runAppT1 (AppState1 3) top1

------------------------------------------------------------------------------
Baby’s First Whitelabel App

app that logs

> data Config = Config { companyName :: String }

> data AppState2 =
>   AppState2 { asConfig :: Config
>             , asLogger :: String -> IO ()
>             }

> newtype AppT2 m a = AppT2 { unAppT2 :: ReaderT AppState2 m a}
>   deriving (Applicative, Functor, Monad, MonadIO, MonadReader AppState2)

> logMsg :: String -> AppT2 IO ()
> logMsg msg = do
>   logger <- asks asLogger
>   liftIO (logger msg)

> getCompanyName :: AppT2 IO String
> getCompanyName = asks (companyName . asConfig)

> runAppT2 :: AppState2 -> AppT2 m a -> m a
> runAppT2 s m = runReaderT (unAppT2 m) s

> top2 :: AppT2 IO ()
> top2 = do
>   n <- getCompanyName
>   logMsg n

> testTop2 = runAppT2 (AppState2 (Config "CPC") putStrLn) top2

Problems with above
- getCompanyName has IO in its type (but not doing IO)
- functions too specific about monad they run in
  - requires lifts to use from deeper in a stack

example: adding more layers in stack

> tryUpdate :: Bool -> AppT2 IO (Either String String)
> tryUpdate True  = return (Right "OK")
> tryUpdate False = return (Left "update failed")

> update :: EitherT String (AppT2 IO) ()
> update = do
>   EitherT (tryUpdate True)        -- set to False to fail
>   lift (logMsg "Update complete") -- this line will not happen if previous line fails

> testTop2e = runAppT2 (AppState2 (Config "CPC") putStrLn)
>                      (runEitherT update >> top2)

Better to be able to access logging/company AppState is available:

------------------------------------------------------------------------------
Use the MTL!

Improvement:

MonadReader AppState m says:
- in this monad, can call ask / get an AppState
- asks enables refinining a selector function to grab a piece of state
- logMsg3 runs in any monad that has access to AppState and can run IO

Above constraints act like capabilities
- only specify what is needed
- can create an alternative transformer stack for testing that satisfies constraints

getCompanyName no longer needs IO

No more lifts

> logMsg3 :: (MonadIO m, MonadReader AppState2 m) => String -> m ()
> logMsg3 msg = do
>   logger <- asks asLogger
>   liftIO (logger msg)

> getCompanyName3 :: (MonadReader AppState2 m) => m String
> getCompanyName3 = asks (companyName . asConfig)

> tryUpdate3 :: Monad m => Bool -> m (Either String String)
> tryUpdate3 True  = return (Right "OK")
> tryUpdate3 False = return (Left "update failed")

> update3 :: (MonadIO m, MonadReader AppState2 m) => EitherT String m ()
> update3 = do
>   EitherT (tryUpdate3 True)
>   logMsg3 "Update complete"

> testTop3 = runAppT2 (AppState2 (Config "CPC") putStrLn)
>                     (runEitherT update3 >> top2)
