{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE NoImplicitPrelude    #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Lib where

import RIO
import RIO.Process
import System.Environment
import System.Exit

someFunc :: IO ()
someFunc = undefined

-- * What is RIO
{-
- Recommendation for Haskell best practices
- Collected from real world code
- Solution to major bugs and productivity woes
- Opinionated
- Collection of libraries you should use
- Alternative prelude
- Recommended for new projects.
-}

-- * Avoid monad transformers
{-
- https://www.fpcomplete.com/blog/2017/06/readert-design-pattern
- most application-wide effects are captured by ReaderT with an IO
- capture this in single RIO data type to make type inference, error messages, etc, much nicer
- use monad transformers "in the small," but not "in the large"
- if you need to do error handling within a transformer, you're probably doing it wrong
- Monad transformer state slides video
    - https://www.snoyman.com/reveal/monad-transformer-state
    - https://www.youtube.com/watch?v=KZIN9f9rI34
- use RIO for your application
-}

-- * HasFoo pattern
{-
mtl-style typeclasses encourage two things:
- m*n instance complexity
- Lots of layers of transformers

Different approach in rio. Instead of:

    class MonadLogger m where
      log :: Text -> m ()

Use:
-}
class HasLogger1 env where
  getLog1 :: env -> (Text -> IO ())


-- log :: HasLogger env => Text -> RIO env ()
-- more general signature, see liftRIO function
log1 :: (MonadReader env m, MonadIO m, HasLogger1 env) => Text -> m ()
log1 text = do
  log <- asks getLog1
  liftIO $ log text
{-
But use lenses (see next section)

Advantages:
- No m*n instance problem
- Deal with functionality directly as functions in a data structure
Disadvantages:
- Some boilerplate incurred
- Hard-coding IO at the base, not always appropriate
-}

-- * Lens
{-
- need to update logging function for a subset of the code
- getLog doesn't support that
- could introduce setLog :: (Text -> IO ()) -> env -> env
- can do via lenses
- rio contains specific subset of lens functionality
-}

type Logger2 = Text -> IO ()

class HasLogger2 env where
  loggerL2 :: Lens' env (Text -> IO ())
instance HasLogger2 Logger2 where
  loggerL2 = id

-- newtypes cannot have strictness annotations
{-# ANN App2 ("HLint: ignore Use newtype instead of data" :: String) #-}
data App2 = App2 { appLogger :: !Logger2 }

instance HasLogger2 App2 where
  loggerL2 = lens appLogger (\x y -> x { appLogger = y })

log2 :: HasLogger2 env => Text -> RIO env ()
log2 text = do
  logger <- view loggerL2
  liftIO $ logger text

-- * Helper modules
{-
- do not need to add lots of packages to package.yaml
- easier for Newcomers to know which libraries to rely on
- rio exports RIO.ByteString, RIO.Text, etc
- partial functions are moved to partial modules
- functions are lifted with MonadIO and MonadUnliftIO
-}
-- * Batteries included prelude
{-
- reexports many types
- removes partial functions
- reexports all of UnliftIO
-}
-- * Stack template
{-
- `stack new projectname rio`
- Options parsing
- Logging, external process running, all set up
- Test suite
-}
-- * Logging
{-
- built in support for logging
- uses HasCallstack to get file source location
- efficient builder for output
- deals with character encoding issues.
-}
-- * Running external processes
-- Mostly follows the tutorial for typed-process
-- However, different definition for proc to allow for logging how long a process runs for

data App = App
  { appLogFunc        :: ! LogFunc
  , appProcessContext :: ! ProcessContext
  }
instance HasLogFunc App where
  logFuncL = lens appLogFunc (\x y -> x { appLogFunc = y })
instance HasProcessContext App where
  processContextL = lens appProcessContext (\x y -> x { appProcessContext = y })

main :: IO ()
main = do
  lo <- logOptionsHandle stderr True
  pc <- mkDefaultProcessContext
  withLogFunc lo $ \lf ->
    let app = App { appLogFunc = lf, appProcessContext = pc }
     in runRIO app run

run :: RIO App ()
run = do
  args <- liftIO getArgs
  case args of
    [] -> do
      logError "You need to provide a command to run"
      liftIO exitFailure
    x:xs -> proc x xs runProcess_

