> {-# LANGUAGE GeneralizedNewtypeDeriving #-}
> {-# LANGUAGE GADTs              #-}
> {-# LANGUAGE KindSignatures     #-}
> {-# LANGUAGE StandaloneDeriving #-}
>
> module Lib where
>
> import Control.Concurrent
> import Control.Monad.Reader
> import System.Random

------------------------------------------------------------------------------
1. intro

key idea : remote monadic commands can be locally combined before sending

send device (lineWidth 10 >> strokeStyle "red")

complications
- monadic commands can return a result, which may be used by subsequent commands.

   isPointInPath :: (Double,Double) -> Canvas Bool

   send device $ do
     inside <- isPointInPath (0,0)
     lineWidth (if inside then 10 else 2)

- invocation of send can also return a value:

    do res <- send device (isPointInPath (0,0))

monadic commands inside send are executed in a remote location
- but the results of those executions need to be made available for use locally

------------------------------------------------------------------------------

--------------------------------------------------
general infrastructure

> data Device0 = Device0
>   {
>     -- Async Command
>     -- o request to perform an action for remote effect
>     -- o no result value, therefore no need for back channel, thus ()
>     -- o no temporal consequence
>     async0 :: String -> IO ()
>     -- Synchronous Procedure
>     -- o request to perform an action for its remote effects
>     -- o there is a result value
>     -- o or there is a temporal consequence (return signal actions completed)
>   , sync0  :: String -> IO String
>   }

> sendAsync0 :: Device0 -> Command -> IO ()
> sendAsync0 d m = async0 d (show m) -- serializes Command and send to remote device

> sendSync0 :: Device0 -> Procedure a -> IO a
> sendSync0 d m = do
>   r <- sync0 d (show m)           -- serializes the Procedure, sends over synchronous channel
>   return (readProcedureReply m r) -- interprets reply in terms of type of same Procedure

--------------------------------------------------
app-specific

> data Command = Say String deriving (Read, Show)

> -- | GADT with phantom type index denoting expected result type
> data Procedure :: * -> * where
>   Temperature ::        Procedure Int
>   Toast       :: Int -> Procedure ()

> instance Show (Procedure a) where
>   show Temperature = "RTemperature"
>   show (Toast i)   = "RToast " ++ show i

> -- | deserialization : uses the phantom type index of Procedure to determine which Read instance to use
> readProcedureReply :: Procedure a -> String -> a
> readProcedureReply Temperature {} i = read i
> readProcedureReply Toast       {} i = read i

> -- | simulation of remote device requires
> -- - representation of commands on remote device
> -- - deserialization function (Read + commandToRCommand) that reads commands
> data RCommand = RSay String deriving Read

> commandToRCommand :: Command -> RCommand
> commandToRCommand (Say s) = RSay s

> -- | remote execution function commands
> execRCommand :: RCommand -> IO ()
> execRCommand (RSay str) = putStrLn ("Remote: " ++ str)

> data RProcedure
>   = RTemperature
>   | RToast Int
>   deriving Read

> execRProcedure :: RProcedure -> IO String
> execRProcedure RTemperature = do
>   t <- randomRIO (50, 100 :: Int)
>   return (show t)
> execRProcedure (RToast n)   = do
>   putStrLn "Remote: Toasting..."
>   threadDelay (1000 * 1000 * n)
>   putStrLn "Remote: Done!"
>   return (show ())

> -- | simulates remote interpreter handle
> -- note: merges serialization of result value into execution function, could be separated
> device0 :: Device0
> device0 = Device0 (execRCommand   . commandToRCommand . read)
>                   (execRProcedure                     . read)

> t10 = sendAsync0 device0 (Say "Do you want some toast?")
> t20 = sendSync0  device0 Temperature
> t30 = sendSync0  device0 (Toast 3)

------------------------------------------------------------------------------
4. Weak Remote Monad

In the above, the arguments to send routines are not Monad instances.

weak remote monad
- "weak" " sends each of its remote calls individually to a remote interpreter
- e.g., does not amortize cost of communication

--------------------------------------------------
general infrastructure

> data DeviceW = DeviceW
>   { syncW  :: String -> IO String
>   , asyncW :: String -> IO ()
>   }

> -- each async command invokes remote procedure call immediately
> sendAsyncW :: Command -> Remote ()
> sendAsyncW m = Remote $ do
>   d <- ask
>   liftIO (asyncW d (show m))
>   return ()

> -- each sync procedures invokes remote procedure call immediately
> sendSyncW :: Procedure a -> Remote a
> sendSyncW m = Remote $ do
>   d <- ask
>   r <- liftIO (syncW d (show m))
>   return (readProcedureReply m r)

> -- KEY POINT : this enables args to following `send` procedure to be a monad instance
> newtype Remote a = Remote (ReaderT DeviceW IO a)
> deriving instance Functor     Remote
> deriving instance Applicative Remote
> deriving instance Monad       Remote

> -- | send that takes an instance of the monad above
> send :: DeviceW -> Remote a -> IO a
> send d (Remote m) = runReaderT m d

--------------------------------------------------
app-specific

> -- | virtual remote Device
> deviceW :: DeviceW
> deviceW = DeviceW (execRProcedure                     . read)
>                   (execRCommand   . commandToRCommand . read)

> say :: String -> Remote ()
> say txt = sendAsyncW (Say txt)

> temperature :: Remote Int
> temperature = sendSyncW Temperature

> toast :: Int -> Remote ()
> toast n = sendSyncW (Toast n)

send with monadic argument: chains of commands and procedures connected using Remote monad

> t1w = send deviceW $ do
>   say "Do you want some toast?" -- command
>   t <- temperature              -- procedure
>   say (show t ++ "F")           -- command
>   return t

> t2w = send deviceW (toast 3)
