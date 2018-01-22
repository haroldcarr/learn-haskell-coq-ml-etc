> module MS where
>
> import Control.Concurrent
> import Control.Monad
> import Control.Monad.ST
> import Control.Monad.ST
> import Data.IORef
> import Data.Foldable (forM_)
> import Data.STRef
> --import GHC.Conc.Sync
> import Control.Concurrent.STM

https://blog.jakuba.net/2014/07/20/mutable-state-in-haskell.html

Mutable State in Haskell

Jul 20, 2014

IORef
STRef in the ST monad
MVar
TVar in Software Transactional Memory (STM)

------------------------------------------------------------------------------
IORef

IO monad
- allows arbitrary effects
- allows mutable reference to a type : IORef

All operations on IORef are in IO monad.

data IORef a

newIORef    :: a -> IO (IORef a)
readIORef   :: IORef a -> IO a
writeIORef  :: IORef a -> a -> IO ()
modifyIORef :: IORef a -> (a -> a) -> IO ()

IORef always contains a value.

> one = do
>   ref <- newIORef (0 :: Int)
>   modifyIORef ref (+1)
>   readIORef ref >>= print

common pattern : put immutable data structure inside a mutable reference

> magicIORef :: IORef (Maybe Int) -> IO ()
> magicIORef ref = do
>   value <- readIORef ref
>   case value of
>     Just _  -> return ()
>     Nothing -> writeIORef ref (Just 42)

> two = do
>   ref <- newIORef Nothing
>   magicIORef ref
>   readIORef ref >>= print

In-place bubble sort with IORef

key part : swapping elements of list

> bubbleSort :: [Int] -> IO [Int]
> bubbleSort input = do
>   let ln = length input
>   xs <- mapM newIORef input
>   forM_ [0..ln - 1] $ \_ -> do
>     forM_ [0..ln - 2] $ \j -> do
>       let ix = xs !! j
>       let iy = xs !! (j + 1)
>       x <- readIORef ix
>       y <- readIORef iy
>       when (x > y) $ do
>         writeIORef ix y
>         writeIORef iy x
>   mapM readIORef xs

λ> :t map newIORef
:: [a] -> [IO (IORef a)]

bubbleSort [1,2,3,4]
bubbleSort [4,3,2,1]
bubbleSort [4,99,23,93,17]

------------------------------------------------------------------------------
ST monad

Only "IO" above is IORef ops.

STRef : State Thread Reference

data STRef s a

newSTRef    ::         a -> ST s (STRef s a)
readSTRef   :: STRef s a -> ST s a
writeSTRef  :: STRef s a -> a -> ST s ()
modifySTRef :: STRef s a -> (a -> a) -> ST s ()

key difference
- can’t escape from IO monad
- CAN   escape from ST monad via 'runST :: ST s a -> a' (making computation pure)

> magicSTRef :: Int -> Int
> magicSTRef x = runST $ do
>   ref <- newSTRef x
>   modifySTRef ref (+1)
>   readSTRef ref

typ of magicSTRef is just Int -> Int : able to escape ST monad via runST.

------------------------------------------------------------------------------
MVar

main difference
- IORef/STRef must have a value
- MVar can be empty

constructing an MVar

newMVar      :: a -> IO (MVar a)
newEmptyMVar ::      IO (MVar a)

-- | takes a value out of an MVar
-- leaves it empty
-- takeMVar on emptyMVar will block thread another thread puts a value into the MVar
takeMVar :: MVar a -> IO

putMVar on FULL MVar blocks until empty

> three = do
>   a <- newEmptyMVar
>   takeMVar a

When run, get:

*** Exception: thread blocked indefinitely in an MVar operation

because no other threads that can modify MVar, so runtime kills the thread.

> four = do
>   a <- newEmptyMVar
>   putMVar a "hello"
>   takeMVar a >>= print

------------------------------------------------------------------------------
Synchronizing threads using MVar

use: synchronized communication between threads

> five = do
>   a <- newEmptyMVar
>   forkIO $ forever $ takeMVar a >>= putStrLn
>   forever $ do
>     text <- getLine
>     putMVar a text


------------------------------------------------------------------------------
Software Transactional Memory - STM

STM : TVar : Transaction Variable

STM builds log of actions to be performed atomically.

Not covering STM as a method for managing concurrency.
Will cover mutable state using STM using a TVar.

STM ops happen inside STM monad.
Can chain with >>=
Run via 'atomically :: STM a -> IO a' in single atomic step.

data TVar a

newTVar    :: a -> STM (TVar a)
readTVar   :: TVar a -> STM a
writeTVar  :: TVar a -> a -> STM ()
modifyTVar :: TVar a -> (a -> a) -> STM ()

There are also alternatives that work in the IO monad.

newTVarIO   :: a -> IO (TVar a)
readTVarIO  :: TVar a -> IO a

Example: one big atomically with all steps:

> bigTransaction :: IO ()
> bigTransaction = do
>   value <- atomically $ do
>     var <- newTVar (0 :: Int)
>     modifyTVar var (+1)
>     readTVar var
>   print value

'modifyTVar' is fine.  Another way:

> atomicReadWrite :: IO ()
> atomicReadWrite = do
>   var <- newTVarIO (0 :: Int)
>   atomically $ do             -- make sure read/write done "atomically"
>     value <- readTVar var
>     writeTVar var (value + 1)
>   readTVarIO var >>= print

monadic chaining:

> f :: TVar Int -> STM ()
> f var = modifyTVar var (+1)

> twoCombined :: IO ()
> twoCombined = do
>   var <- newTVarIO (0 :: Int)
>   atomically $ do
>     f var
>     f var
>   readTVarIO var >>= print
