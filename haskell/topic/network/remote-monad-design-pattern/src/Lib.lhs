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
3.1 Asynchronous Remote Command Call

Command design pattern

Definition. remote command
- request to perform an action for remote effect
- no result value
- no temporal consequence

> data Command = Say String deriving (Read, Show)

> --- | representation of remote device - no need for back channel, thus ()
> data ASDevice = ASDevice { async :: String -> IO () }

> -- | serializes Command and send to remote device
> asSend :: ASDevice -> Command -> IO ()
> asSend d m = async d (show m)

> -- | simulation of remote device requires
> -- - representation of commands on remote device
> -- - deserialization function (Read) that reads commands
> data RCommand = RSay String deriving Read

> commandToRCommand :: Command -> RCommand
> commandToRCommand (Say s) = RSay s

> -- | remote execution function commands
> execRCommand :: RCommand -> IO ()
> execRCommand (RSay str) = putStrLn ("Remote: " ++ str)

> -- | simulates remote interpreter handle
> asDevice :: ASDevice
> asDevice = ASDevice (execRCommand . commandToRCommand . read)

> -- | test
> asyncTest = asSend asDevice (Say "Do you want some toast?")

------------------------------------------------------------------------------

3.2 A Synchronous Remote Call

Definition. remote procedure :
- request to perform an action for its remote effects
- there is a result value
- or there is a temporal consequence (return signal actions completed)

> data SDevice = SDevice { sync :: String -> IO String }

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

> -- | sSend is polymorphic in type parameter of Procedure
> sSend :: SDevice -> Procedure a -> IO a
> sSend d m = do
>   r <- sync d (show m)            -- serializes the Procedure, sends over synchronous channel
>   return (readProcedureReply m r) -- interprets reply in terms of type of same Procedure

simulated remote Device (note: merges serialization of result value into execution function, could be separated)

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

> sDevice :: SDevice
> sDevice = SDevice (execRProcedure . read)

> syncTest1 = sSend sDevice Temperature
> syncTest2 = sSend sDevice (Toast 3)

------------------------------------------------------------------------------

4. The Weak Remote Monad

Above, args to send not Monad instances

join async/synch models into monad called Remote

weak remote monad : initial version : does not amortize cost of communication

Definition. weak remote monad :
- sends each of its remote calls individually to a remote interpreter

> -- | implemented using reader monad, where environment is Device, nested around IO monad
> -- gives access to specific Device used to send commands to remote interpreter
> newtype Remote a = Remote (ReaderT WDevice IO a)
> deriving instance Functor     Remote
> deriving instance Applicative Remote
> deriving instance Monad       Remote

> data WDevice = WDevice
>   { wsync  :: String -> IO String
>   , wasync :: String -> IO ()
>   }

> -- each async command invokes remote procedure call immediately
> sendCommand :: Command -> Remote ()
> sendCommand m = Remote $ do
>   d <- ask
>   liftIO (wasync d (show m))
>   return ()

> say :: String -> Remote ()
> say txt = sendCommand (Say txt)

> -- each sync procedures invokes remote procedure call immediately
> sendProcedure :: Procedure a -> Remote a
> sendProcedure m = Remote $ do
>   d <- ask
>   r <- liftIO (wsync d (show m))
>   return (readProcedureReply m r)

> temperature :: Remote Int
> temperature = sendProcedure Temperature

> toast :: Int -> Remote ()
> toast n = sendProcedure (Toast n)

> -- | run Remote monad
> send :: WDevice -> Remote a -> IO a
> send d (Remote m) = runReaderT m d

> -- | virtual remote Device
> wdevice :: WDevice
> wdevice = WDevice (execRProcedure                     . read)
>                   (execRCommand   . commandToRCommand . read)

test : send with monadic argument, and chains of primitives, connected using the monad
achieved goal: weak remote monad where commands and procedures are executed in remote location

> wTest1 = send wdevice $ do
>   say "Do you want some toast?" -- command
>   t <- temperature              -- procedure
>   say (show t ++ "F")           -- command
>   return t

> wTest2 = send wdevice (toast 3)
