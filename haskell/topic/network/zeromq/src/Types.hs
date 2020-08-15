{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Types where

------------------------------------------------------------------------------
import           Control.Concurrent                       (newMVar,
                                                           putMVar, takeMVar,
                                                           threadDelay
                                                           )
import qualified Control.Concurrent.Chan.Unagi            as U
import qualified Control.Concurrent.Chan.Unagi.NoBlocking as UNB
import           Control.Monad.State.Strict
import qualified Data.Map.Strict                          as Map
import           Data.Serialize.Text                      ()
import qualified Data.Set                                 as Set
import           GHC.Generics                             (Generic)
import           Protolude                                hiding (async,
                                                           newChan, readChan,
                                                           to)
------------------------------------------------------------------------------

newtype Addr            a   = Addr            {unAddr            :: a} deriving (Eq, Ord, Show)
newtype Connection      c   = Connection      {unConnection      :: c}
newtype ConnectionCache a c = ConnectionCache {unConnectionCache :: Map.Map (Addr a)(Connection c)}

-- | who to send a message to
data Recipients a
  = RAll
  | RSome !(Set.Set (Addr a))
  | ROne  !(Addr a)
  deriving (Eq, Generic, Show)

data OutBoundMsg addr msg = OutBoundMsg
  { obmTo   :: !(Recipients addr)
  , obmBody :: !msg
  } deriving (Eq, Generic)

data TransportEnv rpc addr = TransportEnv
  { inboxWrite :: !(UNB.InChan rpc)
  , outboxRead :: !(U.OutChan  (OutBoundMsg addr ByteString))
  , myAddr     :: !(Addr addr)
  , addrList   :: ![Addr addr]
  , logErr     :: !([Text] -> IO ())
  , logInfo    :: !([Text] -> IO ()) }

setup
  :: Addr addr
  -> (Addr addr -> [Text] -> IO ())
  -> (Addr addr -> [Text] -> IO ())
  -> IO ( TransportEnv rpc addr
        , MVar (UNB.Stream rpc)
        , U.InChan (OutBoundMsg addr ByteString) )
setup me le li = do
  (inboxW , inboxR)  <- newNoBlockChan
  (outboxW, outboxR) <- U.newChan
  pure ( TransportEnv inboxW outboxR me [] (le me) (li me)
       , inboxR, outboxW )


------------------------------------------------------------------------------

newNoBlockChan :: IO (UNB.InChan a, MVar (UNB.Stream a))
newNoBlockChan = do
  (w, r) <- UNB.newChan
  r'     <- fmap head (UNB.streamChan 1 r) >>= \case
    Nothing -> panic "newNoBlockChan"
    Just x  -> newMVar x
  pure (w, r')

getMsgSync :: MVar (UNB.Stream msg) -> IO msg
getMsgSync m = do
  inboxRead <- takeMVar m
  t         <- UNB.tryReadNext inboxRead
  case t of
    UNB.Pending           -> putMVar m inboxRead  >> getMsgSync m
    UNB.Next v inboxRead' -> putMVar m inboxRead' >> pure v

tryGetMsgs :: Eq msg => MVar (UNB.Stream msg) -> Int -> IO [msg]
tryGetMsgs m i0 = do
  inboxRead <- takeMVar m
  msgs <- go inboxRead i0
  if msgs /= []
    then pure msgs
    else threadDelay 1000 >> pure []
 where
  go strm i =
    if i <= 0
    then putMVar m strm >> pure []
    else do
      s <- UNB.tryReadNext strm
      case s of
        UNB.Next a strm' -> fmap (a:) (go strm' (i - 1))
        UNB.Pending      -> putMVar m strm >> pure []

