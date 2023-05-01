{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE GADTs                #-}
{-# LANGUAGE LambdaCase           #-}
{-# LANGUAGE RankNTypes           #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE TypeApplications     #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE UndecidableInstances #-}

module Dynamic where

import           Control.Exception            (IOException)
import           Control.Monad
import           Control.Monad.Catch          (catch)
import           Data.Char
import qualified Data.Map.Strict              as Map
import           Effectful
import           Effectful.Dispatch.Dynamic
import           Effectful.Error.Static
import           Effectful.State.Static.Local
import           Effectful.TH
import           Prelude                      hiding (readFile, writeFile)
import qualified System.IO                    as IO
import           UnliftIO.IO                  (getMonotonicTime)

{-
dynamically dispatched effect are operations that can be interpreted in different ways
at runtime, depending on handler in scope.

separates __what__ from __how__
-}

------------------------------------------------------------------------------
-- example : file access

data FileSystem :: Effect where
  ReadFile  :: FilePath ->           FileSystem m String
  WriteFile :: FilePath -> String -> FileSystem m ()

makeEffect ''FileSystem

-- EffectHandler that reads/writes from disk.

newtype FsError = FsError String deriving Show

handleFileSystemIO
  :: (IOE :> es, Error FsError :> es)
  => Eff (FileSystem : es) a
  -> Eff es a
handleFileSystemIO = interpret $ \_ -> \case
  ReadFile  path          -> adapt $ IO.readFile path
  WriteFile path contents -> adapt $ IO.writeFile path contents
 where
  adapt m = liftIO m `catch` \(e::IOException) -> throwError . FsError $ show e

-- EffectHandler that reads/write from memory.

-- use 'reinterpret'
-- use 'Effectful.State.Static.Local.State' for storage
-- - it is private to the effect handler and cannot be accessed outside of it.
handleFileSystemPure
  :: Error FsError :> es
  => Map.Map FilePath String
  -> Eff (FileSystem : es) a
  -> Eff es a
handleFileSystemPure fs0 = reinterpret (evalState fs0) $ \_ -> \case
  ReadFile path -> gets (Map.lookup path) >>= \case
    Just contents -> pure contents
    Nothing       -> throwError . FsError $ "File not found: " ++ show path
  WriteFile path contents -> modify $ Map.insert path contents

read1File :: FilePath -> (FileSystem :> es) => Eff es Int
read1File filepath = do
  file <- readFile filepath
  pure $  length file

writeAndReadFile :: FilePath -> (FileSystem :> es) => Eff es Int
writeAndReadFile filepath = do
  writeFile filepath "abcdefg"
  read1File filepath

runFileSystemIO
  :: FilePath
  -> IO (Either (CallStack, FsError) Int)
runFileSystemIO filepath =
  runEff . runError @FsError . handleFileSystemIO $ read1File filepath

runFileSystemMap
  :: Map.Map FilePath String
  -> FilePath
  -> Either FsError Int
runFileSystemMap mem filepath =
  runPureEff . runErrorNoCallStack @FsError . handleFileSystemPure mem $ read1File filepath

runFileSystemMap'
  :: Map.Map FilePath String
  -> FilePath
  -> Either FsError Int
runFileSystemMap' mem filepath =
  runPureEff . runErrorNoCallStack @FsError . handleFileSystemPure mem $ writeAndReadFile filepath

www,xxx :: IO (Either (CallStack, FsError) Int)
www = runFileSystemIO "hie.yaml"
xxx = runFileSystemIO "../hie.yaml"

yyy,zzz :: Either FsError Int
yyy = runFileSystemMap Map.empty "hie.yaml"
zzz = runFileSystemMap (Map.fromList [("hie.yaml", "stuff")]) "hie.yaml"

yyy' :: Either FsError Int
yyy' = runFileSystemMap' Map.empty "test"

------------------------------------------------------------------------------
-- order

{-
Definition of FileSystem above does not use @m@ type parameter.
When the effect is interpreted, 'LocalEnv' arg of 'EffectHandler' is also not used.
Such effects are /first order/.

If an effect makes use of the @m@ parameter, it is a /higher order effect/.

Interpretation of higher order effects.

example : @Profiling@ effect for logging how much time an action takes
-}

data Profiling :: Effect where
  Profile :: String -> m a -> Profiling m a

makeEffect ''Profiling

{- generates:
  profile :: (HasCallStack, Profiling :> es) => String -> Eff es a -> Eff es a
  profile label action = send (Profile label action)

use the 'LocalEnv' that an 'EffectHandler' is given to run
the action using one of the functions from the 'localUnlift' family:
-}

runProfiling :: IOE :> es => Eff (Profiling : es) a -> Eff es a
runProfiling = interpret $ \env -> \case
  Profile label action -> localSeqUnliftIO env $ \unlift -> do
    t1 <- getMonotonicTime
    r  <- unlift action
    t2 <- getMonotonicTime
    putStrLn $ "Action '" ++ label ++ "' took " ++ show (t2 - t1)
    pure r

-- dummy interpreter that does no profiling
runNoProfiling :: Eff (Profiling : es) a -> Eff es a
runNoProfiling  = interpret $ \env -> \case
  Profile _label action -> localSeqUnlift env $ \unlift -> unlift action

greetAction :: (Profiling :> es, IOE :> es) => Eff es ()
greetAction  = profile "greet" . liftIO $ putStrLn "Hello!"

greatActionIO :: IO ()
greatActionIO  = runEff . runProfiling $ greetAction

greatActionIONoProf :: IO ()
greatActionIONoProf = runEff . runNoProfiling $ greetAction

------------------------------------------------------------------------------
-- integration

{-
Libraries that provide functionality as an @mtl@ style effect
(generally is a type class) can be used with the 'Eff' monad.

example, : generation of random numbers
-}

class Monad m => MonadRNG m where
  randomInt :: m Int

-- function for generation of random strings

randomString :: MonadRNG m => Int -> m String
randomString n = map chr <$> replicateM n randomInt

-- To use MonadRNG with the 'Eff' monad,
-- first create an effect with operations that mirror the type class ops

data RNG :: Effect where
  RandomInt :: RNG m Int

type instance DispatchOf RNG = 'Dynamic

{-
@randomInt@ tied to @RandomInt@ : this function is already declared by @MonadRNG@ type class.

So provide an __orphan__, __canonical__ instance of @MonadRNG@ for 'Eff'
that delegates to the @RNG@ effect

-- >>> :set -XUndecidableInstances
-}

instance RNG :> es => MonadRNG (Eff es) where
  randomInt = send RandomInt

-- interpreter

runDummyRNG :: Eff (RNG : es) a -> Eff es a
runDummyRNG  = interpret $ \_ -> \case
  RandomInt -> pure 65

-- can use any function that requires a @MonadRNG@ constraint with the Eff' monad
-- as long as the @RNG@ effect is in place:

aaa :: IO String
aaa  = runEff . runDummyRNG $ randomString 3

-- "AAA"


