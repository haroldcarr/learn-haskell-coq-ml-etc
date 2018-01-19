> module Cookbook where
>
> import Control.Concurrent               (forkIO, newEmptyMVar, putMVar, takeMVar, threadDelay)
> import qualified Control.Exception.Safe as S
> import Data.Typeable                    (Typeable, cast)
>
> {-# ANN module ("HLint: ignore Reduce duplication" :: Prelude.String) #-}

https://github.com/fpco/safe-exceptions/blob/master/COOKBOOK.md

cookbook for usage of safe-exceptions

User-defined async exceptions

leverage extensible exception machinery

run program

then comment out implementation of toException and fromException to see difference in behavior

> data E1 = E1 deriving (Show, Typeable)
>
> instance S.Exception E1 where
>   toException = S.toException . S.SomeAsyncException
>   fromException se = do
>     S.SomeAsyncException e <- S.fromException se
>     cast e
>
> one :: IO ()
> one = do
>   baton <- newEmptyMVar -- give the handler a chance to run
>   tid <- forkIO $ threadDelay maxBound
>     `S.withException` (\e -> print ("Inside withException", e :: E1))
>     `S.finally` putMVar baton ()
>   putStrLn "before S.throwTo"
>   S.throwTo tid E1
>   putStrLn "after S.throwTo"
>   takeMVar baton
>   putStrLn "Done!"

one works fine

in two : withException message is not printed
- because throwTo wraps E2 in a different async exception type
- because no to/fromException defined for E2
- so foils exception handler from firing

> data E2 = E2 deriving (Show, Typeable)
>
> instance S.Exception E2 where
>
> two :: IO ()
> two = do
>   baton <- newEmptyMVar -- give the handler a chance to run
>   tid <- forkIO $ threadDelay maxBound
>     `S.withException` (\e -> print ("Inside withException", e :: E2))
>     `S.finally` putMVar baton ()
>   putStrLn "before S.throwTo"
>   S.throwTo tid E2
>   putStrLn "after S.throwTo"
>   takeMVar baton
>   putStrLn "Done!"

NOTE: not recommended concurrency code : use async package
