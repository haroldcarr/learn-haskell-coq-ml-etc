{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures             #-}
{-# LANGUAGE RecursiveDo                #-}
{-# LANGUAGE StandaloneDeriving         #-}

module Lib where

import           Control.Concurrent
import           Control.Monad.Reader
import           Control.Monad.State
import           Control.Monad.Writer
import           System.Random

------------------------------------------------------------------------------
{-
1. intro

key idea : remote monadic commands can be locally queued then sent in batch

complications
- monadic commands can return a result, which may be used by subsequent commands.
   send device $ do
     inside <- isPointInPath (0,0)
     lineWidth (if inside then 10 else 2)
- invocation of send can also return a value:
    do res <- send device (isPointInPath (0,0))
monadic commands inside send are executed in a remote location
- but the results of those executions need to be made available for use locally
-}
------------------------------------------------------------------------------
-- 3.1 async / 3.2 sync

--------------------------------------------------
-- general infrastructure

data Device = Device
  {
    -- Async Command
    -- o request to perform an action for remote effect
    -- o no result value, therefore no need for back channel, thus ()
    -- o no temporal consequence
    async :: String -> IO ()
    -- Synchronous Procedure
    -- o request to perform an action for remote effect
    -- o there is a result value, and/or
    -- o there is a temporal consequence (return signal actions completed)
  , sync  :: String -> IO String
  }

sendAsync0 :: Device -> Command -> IO ()
sendAsync0 d m = async d (show m) -- serializes Command and send to remote device

sendSync0 :: Device -> Procedure a -> IO a
sendSync0 d m = do
  r <- sync d (show m)            -- serializes the Procedure, sends over synchronous channel
  return (readProcedureReply m r) -- interprets reply in terms of type of same Procedure

--------------------------------------------------
-- app-specific

-- | simulates remote interpreter handle
-- note: merges serialization of result value into execution function, could be separated
device0 :: Device
device0  = Device (execRCommand   . commandToRCommand . read)
                  (execRProcedure                     . read)

data Command = Say String deriving (Read, Show)

-- | simulation of remote device requires
-- - representation of commands on remote device
-- - deserialization function (Read + commandToRCommand) that reads commands
data RCommand = RSay String deriving (Read, Show)

commandToRCommand :: Command -> RCommand -- TODO : handle this like other R*
commandToRCommand (Say s) = RSay s

-- | GADT with phantom type index denoting expected result type
data Procedure :: * -> * where
  Temperature ::        Procedure Int
  Toast       :: Int -> Procedure ()

instance Show (Procedure a) where
  show Temperature = "RTemperature"      -- NOTE: R* : paper used same constructor name for both Procedure and RProcedure
  show (Toast i)   = "RToast " ++ show i -- NOTE: R*

data RProcedure
  = RTemperature
  | RToast Int
  deriving (Read)

-- | deserialization : uses the phantom type index of Procedure to determine which Read instance to use
readProcedureReply :: Procedure a -> String -> a
readProcedureReply Temperature {} i = read i
readProcedureReply Toast       {} i = read i

-- | remote execution function commands
execRCommand :: RCommand -> IO ()
execRCommand (RSay str) = putStrLn ("Remote: " ++ str)

execRProcedure :: RProcedure -> IO String
execRProcedure RTemperature = do
  t <- randomRIO (50, 100 :: Int)
  return (show t)
execRProcedure (RToast n)   = do
  putStrLn "Remote: Toasting..."
  threadDelay (1000 * 1000 * n)
  putStrLn "Remote: Done!"
  return (show ())

--------------------------------------------------
-- exercise

t10 = sendAsync0 device0 (Say "Do you want some toast?")
t20 = sendSync0  device0 Temperature
t30 = sendSync0  device0 (Toast 3)

------------------------------------------------------------------------------
{-
4. Weak Remote Monad

In the above, the arguments to send routines are not Monad instances.

weak remote monad
- "weak" " sends each of its remote calls individually to a remote interpreter
- e.g., does not amortize cost of communication
-}
--------------------------------------------------
-- general infrastructure

-- KEY POINT : this enables args to following `send` procedure to be a monad instance
newtype RemoteW a = RemoteW (ReaderT Device IO a)
deriving instance Functor     RemoteW
deriving instance Applicative RemoteW
deriving instance Monad       RemoteW

-- each async command invokes remote procedure call immediately
sendAsyncW :: Command -> RemoteW ()
sendAsyncW m = RemoteW $ do
  d <- ask
  liftIO (async d (show m))
  return ()

-- each sync procedures invokes remote procedure call immediately
sendSyncW :: Procedure a -> RemoteW a
sendSyncW m = RemoteW $ do
  d <- ask
  r <- liftIO (sync d (show m))
  return (readProcedureReply m r)

-- | send that takes an instance of the monad above
sendW :: Device -> RemoteW a -> IO a
sendW d (RemoteW m) = runReaderT m d

--------------------------------------------------
-- app-specific

sayW :: String -> RemoteW ()
sayW txt = sendAsyncW (Say txt)

temperatureW :: RemoteW Int
temperatureW = sendSyncW Temperature

toastW :: Int -> RemoteW ()
toastW n = sendSyncW (Toast n)

--------------------------------------------------
-- exercise

-- send with monadic argument: chains of commands and procedures connected using RemoteW monad

t1w = sendW device0 $ do
  sayW "Do you want some toast?" -- command
  t <- temperatureW              -- procedure
  sayW (show t ++ "F")           -- command
  return t

t2w = sendW device0 (toastW 3)

------------------------------------------------------------------------------
{-
5. Strong Remote Monad

bundle monadic remote calls
- async commands  : queue to send later
- sync procedures : must transmit immediately
  - first send queued commands
  - then send the procedure and await result
- assumes only command and/or procedures are in in remote monad
  - no way of locally pausing the pipeline of commands
-}
--------------------------------------------------
-- general infrastructure

newtype Remote a = Remote (ReaderT Device (StateT [Command] IO) a)
deriving instance Functor     Remote
deriving instance Applicative Remote
deriving instance Monad       Remote

data Packet a = Packet [Command] (Procedure a)
instance Show (Packet a) where
  show (Packet cmds p) = "RPacket " ++ show (map commandToRCommand cmds) ++ " (" ++ show p ++ ")"-- NOTE: R*

data RPacket = RPacket [RCommand] RProcedure deriving Read

sendAsync :: Command -> Remote ()
sendAsync cmd = Remote (modify (++ [cmd])) -- append Commands to queue

sendSync :: Procedure a -> Remote a
sendSync p = Remote $ do
  d  <- ask
  cs <- get
  r  <- liftIO (sync d (show (Packet cs p))) -- send queued Commands and single Procedure
  put []
  return (readProcedureReply p r)

send :: Device -> Remote a -> IO a
send d (Remote m) = do
  (r,cs) <- runStateT (runReaderT m d) []
  when (not (null cs)) (async d (show cs)) -- this action happens if remote monad does NOT contain a procedure
  return r

execRPacket :: RPacket -> IO String
execRPacket (RPacket cs p) = do
  mapM_ execRCommand cs
  execRProcedure p

--------------------------------------------------
-- app-specific

device :: Device
device  = Device (mapM_ execRCommand . map commandToRCommand . read) -- called if monad only contains commands
                 (execRPacket                                . read) -- called if monad contains a procedure

say :: String -> Remote ()
say txt = sendAsync (Say txt)

temperature :: Remote Int
temperature = sendSync Temperature

toast :: Int -> Remote ()
toast n = sendSync (Toast n)

--------------------------------------------------
-- exercise

t0s = send device $ do
  say "Do you want some toast?" -- command
  say "and jamm"                -- command

t1s = send device $ do
  t <- temperature              -- procedure
  return t

t2s = send device $ do
  say "Do you want some toast?" -- command
  say "and jamm"                -- command
  t <- temperature              -- procedure
  say (show t ++ "F")           -- command
  toast 3                       -- procedure
  say "thanks for the toast"    -- command
  return t

------------------------------------------------------------------------------
-- 6. Remote Applicative
{-
applicatives are better suited to remoteness
- applicative com putations cannot depend on results of prior computations

weak :  same capabilities as weak remote monad (primitives transmitted individually)
strong : bundle all the Commands and Procedures into a single packet
- represented by [Prims]
-}
-- PRIMitive remote calls
data Prim :: * where
  Command   ::           Command     -> Prim -- above : Command/Procedure are types; here : constructors
  Procedure :: Show a => Procedure a -> Prim
instance Show Prim where
  show (Command   c) = show c
  show (Procedure a) = show a

data RPrim
  = RCommand   RCommand
  | RProcedure RProcedure
  deriving Read

-- writer : queues calls
-- state : results
-- lazy evaluation "connects" send/receiving -- TODO
newtype RemoteA a = RemoteA (WriterT [Prim] (State [String]) a)
deriving instance Functor     RemoteA
deriving instance Applicative RemoteA

sendAsyncA :: Command -> RemoteA ()
sendAsyncA cmd = RemoteA (tell [Command cmd]) -- queue

-- remember the Procedure
-- then read result from state, lazily pulling it off a list
-- important: no external effects inside inner monad
sendSyncA :: Show a => Procedure a -> RemoteA a
sendSyncA p = RemoteA $ do
  tell [Procedure p]
  ~(r:rs) <- get
  put rs
  return (readProcedureReply p r)

-- uses recursive do

sendA :: Device -> RemoteA a -> IO a
sendA d (RemoteA m) = do
  rec let ((a,ps),_) = runState (runWriterT m) r -- recursively feed result list back into invocation
      r <- if all isCommand ps
               then do async d (show ps)       -- send all cmds at once
                       return []
               else do str <- sync d (show ps) -- send all cmds/procs at once : OK : no intermediate results used
                       return (read str)
  return a
 where
  isCommand (Command   {}) = True
  isCommand (Procedure {}) = False

{-
  execRPrims :: [RPrim] -> IO [String]
  execRPrims [] = return []
  execRPrims (Command c : ps) = do
           execRCommand c
           execRPrims ps
  execRPrims (Procedure p : ps) = do
           r  <- execRProcedure p
           rs <- execRPrims ps
           return (r:rs)
-}
