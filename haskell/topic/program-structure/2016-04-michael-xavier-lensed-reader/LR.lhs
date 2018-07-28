> {-# LANGUAGE FlexibleContexts           #-}
> {-# LANGUAGE GeneralizedNewtypeDeriving #-}
> {-# LANGUAGE LambdaCase                 #-}
> {-# LANGUAGE TemplateHaskell            #-}
>
> module LR where
>
> import Control.Lens
> import Control.Monad.IO.Class
> import Control.Monad.Reader
> import Control.Monad.Except

 http://michaelxavier.net/posts/2016-04-03-Enterprise-Haskell-Pattern-Lensed-Reader.html

------------------------------------------------------------------------------
1. The Precursor: ReaderT-based Transformer Stack

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

> newtype AppState1 = AppState1 Int

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

> testTop1 :: IO Int
> testTop1 = runAppT1 (AppState1 3) top1

------------------------------------------------------------------------------
2. Baby’s First Whitelabel App

app that logs

> newtype Config = Config { companyName :: String }

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

> testTop2 :: IO ()
> testTop2 = runAppT2 (AppState2 (Config "CPC") putStrLn) top2

Problems with above
- getCompanyName has IO in its type (but not doing IO)
- functions too specific about monad they run in
  - requires lifts to use from deeper in a stack

example: adding more layers in stack

> tryUpdate :: Bool -> AppT2 IO (Either String String)
> tryUpdate True  = return (Right "OK")
> tryUpdate False = return (Left "update failed")

> update :: ExceptT String (AppT2 IO) ()
> update = do
>   _ <- ExceptT (tryUpdate True)   -- set to False to fail
>   lift (logMsg "Update complete") -- this line will not happen if previous line fails

> testTop2e :: IO ()
> testTop2e = runAppT2 (AppState2 (Config "CPC") putStrLn)
>                      (runExceptT update >> top2)

Better to be able to access logging/company AppState is available:

------------------------------------------------------------------------------
3. Use the MTL!

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

  -- IO no longer present
  getCompanyName3 :: (MonadReader AppState2 m) => m String
  getCompanyName3 = asks (companyName . asConfig)

> tryUpdate3 :: Monad m => Bool -> m (Either String String)
> tryUpdate3 True  = return (Right "OK")
> tryUpdate3 False = return (Left "update failed")

> update3 :: (MonadIO m, MonadReader AppState2 m) => ExceptT String m ()
> update3 = do
>   _ <- ExceptT (tryUpdate3 False)
>   logMsg3 "Update complete"

> topNoop :: AppT2 IO ()
> topNoop = return ()

> testTop3 :: IO ()
> testTop3 = runAppT2 (AppState2 (Config "CPC") putStrLn)
>                     (runExceptT update3 >>= \case
>                                                Left x -> do
>                                                  liftIO $ putStrLn x
>                                                  topNoop
>                                                Right _x -> top2)

------------------------------------------------------------------------------
4. More Granularity with Lensed Reader

When just need config from AppState
- if code uses MonadReader AppState m, then requires whole AppState
- solution: refactor AppState into what is needed
- via classy lenses

> newtype Config4   = Config4   { _companyName4 :: String }
> data    AppState4 = AppState4 { _asConfig4    :: Config4
>                               , _asLogger4    :: String -> IO ()
>                               }
> makeClassy ''Config4
> makeLenses ''AppState4

makeClassy creates something like:

class HasConfig a where
  config :: Lens' a Config
  companyName :: Lens' a String

instance HasConfig Config where -- ...

> instance HasConfig4 AppState4 where
>   config4 = asConfig4

now have
- way to specify data types that contain Config
- companyName has a default implementation that pulls it off of Config

this type of abstraction has been refered to as a "seam""
- line in fabric of code that can be opened and modified if need be

final piece
- view from lens (like asks from MonadReader, but it takes a lens)

> -- note : lenses compose in opposite direction of functions, therefore config, then companyName
> getCompanyName4 :: (MonadReader r m, HasConfig4 r) => m String
> getCompanyName4 = view (config4 . companyName4)

now can provide a specific context for each function

> heavyReport :: (MonadReader AppState4 m) => m String
> heavyReport = do
>   cn <- getCompanyName4
>   return (cn ++ " HEAVY")

> lightReport :: (MonadReader r m, HasConfig4 r) => m String
> lightReport = do
>   cn <- getCompanyName4
>   return (cn ++ " LIGHT")

benefits
- do not need AppT or IO
- lightReport can be used in a minimal Reader or in AppT

> runLightReportC :: Config4 -> String
> runLightReportC = runReader lightReport

> runLightReportA :: AppState4 -> String
> runLightReportA = runReader lightReport

> testRunLightReportC :: String
> testRunLightReportC = runLightReportC (Config4 "CPC")
> testRunLightReportA :: String
> testRunLightReportA = runLightReportA (AppState4 (Config4 "CPC") putStrLn)

note: heaveReport can only be used in AppState4 because too specific

> {- will not compile:
> runHeavyReportC :: Config4 -> String
> runHeavyReportC = runReader heavyReport
> -}

> runHeavyReportA :: AppState4 -> String
> runHeavyReportA = runReader heavyReport
> testRunHeavyReportA :: String
> testRunHeavyReportA = runHeavyReportA (AppState4 (Config4 "CPC") putStrLn)

------------------------------------------------------------------------------
HC

> -- note : lenses compose in opposite direction of functions, therefore config, then companyName
> getLogger4 :: (MonadReader AppState4 m) => m (String -> IO ())
> getLogger4 = do
>    AppState4 _ l <- ask
>    return l

> reallyHeavyReport :: (MonadIO m, MonadReader AppState4 m) => m String
> reallyHeavyReport = do
>   cn <- getCompanyName4
>   l <- getLogger4          -- can access, even without MonadIO
>   let rpt = cn ++ " HEAVY"
>   liftIO (l rpt)           -- can NOT call unless MonadIO included
>   return rpt

> newtype AppT4 m a = AppT4 { unAppT4 :: ReaderT AppState4 m a}
>   deriving (Applicative, Functor, Monad, MonadIO, MonadReader AppState4)

> runAppT4 :: AppState4 -> AppT4 m a -> m a
> runAppT4 s m = runReaderT (unAppT4 m) s

> top4 :: AppT4 IO String
> top4 = do
>   lr <- lightReport
>   liftIO (putStrLn lr)
>   hr <- heavyReport
>   liftIO (putStrLn hr)
>   rhr <- reallyHeavyReport
>   liftIO (putStrLn rhr)
>   AppState4 x _y <- ask
>   return (_companyName4 x)

> testTop4 :: IO String
> testTop4 = runAppT4 (AppState4 (Config4 "HC") putStrLn) top4

------------------------------------------------------------------------------
Summary

- create "classy"" lenses
  - for app’s state type and
  - any subcomponents to access independently
- use constraints in code instead of concrete transformer stacks
  - only specifying the stack near main where it is run
- use minimal set of constraints needed for functions
  - low-level functions end up with smaller sets of constraints
  - larger ones accumulate combined constraints
  - constraints show functions capabilities
- GHC 8.0 with -Wall -Werror warns about unnecessary constraints

------------------------------------------------------------------------------

> test :: IO ()
> test = do
>   testTop1 >>= print
>   testTop2
>   testTop2e
>   testTop3
>   print testRunLightReportC
>   print testRunLightReportA
>   print testRunHeavyReportA
>   testTop4 >>= print
