{-# LANGUAGE DeriveFunctor    #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell  #-}

module FM_MX_Redis where

import           Control.Monad.Free    (Free (Free), MonadFree, iterM, liftF)
import           Control.Monad.Free.TH (makeFree)
import           Control.Monad.State   as MS hiding (get, put)
import qualified Data.Map              as M
import           Test.HUnit            (Counts, Test (TestList), runTestTT)
import qualified Test.HUnit.Util       as U (t)

-- http://michaelxavier.net/posts/2014-04-27-Cool-Idea-Free-Monads-for-Testing-Redis-Calls.html

------------------------------------------------------------------------------
-- imperative code

data Conn = Conn
get         :: Conn -> String -> IO (Maybe String)
get Conn k   = do putStrLn k; v <- getLine; return (Just v)
put         :: Conn -> String -> String -> IO ()
put Conn k v = putStrLn (k ++ "/" ++ v)
multi       :: Conn -> t -> t
multi Conn x = x

foo :: Conn -> IO ()
foo c = do
    mv <- get c "foo"
    case mv of
        Nothing -> return ()
        Just v -> multi c $ do
            put c "foo1" v
            put c "foo2" v

------------------------------------------------------------------------------
-- free monad version builds an AST representation

data RedisCmd next = Get'    String       (Maybe String -> next)
                   | Put'    String        String          next
                   | Multi' (RedisCmdM ())                 next
                   deriving (Functor)

type RedisCmdM = Free RedisCmd

-- generates get', put', and multi'
makeFree ''RedisCmd

foo' :: RedisCmdM ()
foo' = do
    mv <- get' "foo"
    case mv of
        Nothing -> return ()
        Just v -> multi' $ do
            put' "foo1" v
            put' "foo2" v

------------------------------------------------------------------------------
-- interpreters

runDebug :: RedisCmdM a -> IO a
runDebug = iterM run
  where
    run :: RedisCmd (IO a) -> IO a
    run (Get' k f) = do
        putStrLn $ unwords ["GET", k]
        f . Just =<< getLine
    run (Put' k v n) = do
        putStrLn $ unwords ["PUT", k, v]
        n
    run (Multi' txn n) = do
        putStrLn "MULTI"
        runDebug txn
        putStrLn "EXEC"
        n

type FakeDB = M.Map String String

runTest :: MonadState FakeDB m => RedisCmdM a -> m a
runTest = iterM run
  where
    run (Get' k f) = f =<< gets (M.lookup k)
    run (Put' k v n) = do
        MS.modify $ M.insert k v
        n
    run (Multi' txn n) = do
        runTest txn
        n

trt :: [Test]
trt = U.t "trt"
    (execState (runTest foo') (M.fromList [("foo","fooval")]))
    (M.fromList [("foo","fooval"),("foo1","fooval"),("foo2","fooval")])
{-
runRedis :: (MonadState Conn m, MonadIO m) => RedisCmdM a -> m a
runRedis = withConn $ \c -> iterM (run c)
  where
    run c (Get' k f)     = get   c k   >>= f
    run c (Put' k v n)   = put   c k v >>  n
    run c (Multi' txn n) = multi c txn >>  n
    withConn action      = liftIO (action <$> db)
-}
------------------------------------------------------------------------------

test :: IO Counts
test =
    runTestTT $ TestList {- $ -} trt
