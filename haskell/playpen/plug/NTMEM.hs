module NTMEM (createTransport) where

import NT
import Control.Concurrent.Chan (Chan, newChan, readChan, writeChan)
import Control.Applicative ((<$>))
import Control.Category ((>>>))
import Control.Concurrent.MVar (MVar, newMVar, modifyMVar, modifyMVar_, readMVar)
import Control.Exception (throwIO)
import Control.Monad (forM_, when)
import Data.Map (Map)
import qualified Data.Map as Map (empty, insert, size, delete, findWithDefault)
import Data.Set (Set)
import qualified Data.Set as Set (empty, elems, insert, delete)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BSC (pack)
import Data.Accessor (Accessor, accessor, (^.), (^=), (^:))
import qualified Data.Accessor.Container as DAC (mapMaybe)

data TransportState = State { _channels         :: Map EndPointAddress (Chan Event)
                            , _nextConnectionId :: Map EndPointAddress ConnectionId
                            }

createTransport :: IO Transport
createTransport = do
  state <- newMVar State { _channels         = Map.empty
                         , _nextConnectionId = Map.empty
                         }
  return Transport { newEndPoint    = apiNewEndPoint state
                   , closeTransport = throwIO (userError "closeEndPoint not implemented")
                   }

apiNewEndPoint :: MVar TransportState -> IO (Either (TransportError NewEndPointErrorCode) EndPoint)
apiNewEndPoint state = do
  chan <- newChan
  addr <- modifyMVar state $ \st -> do
    let addr = EndPointAddress . BSC.pack . show . Map.size $ st ^. channels
    return ((channelAt addr ^= chan) . (nextConnectionIdAt addr ^= 1) $ st, addr)
  return . Right $ EndPoint { receive       = readChan chan
                            , address       = addr
                            , connect       = apiConnect addr state
                            , closeEndPoint = throwIO (userError "closeEndPoint not implemented")
                            }

apiConnect :: EndPointAddress
           -> MVar TransportState
           -> EndPointAddress
           -> IO (Either (TransportError ConnectErrorCode) Connection)
apiConnect myAddress state theirAddress = do
  (chan, conn) <- modifyMVar state $ \st -> do
    let chan = st ^. channelAt theirAddress
    let conn = st ^. nextConnectionIdAt theirAddress
    return (nextConnectionIdAt theirAddress ^: (+ 1) $ st, (chan, conn))
  writeChan chan $ ConnectionOpened conn myAddress
  connAlive <- newMVar True
  return . Right $ Connection { send  = apiSend chan conn connAlive
                              , close = apiClose chan conn connAlive
                              }

apiSend :: Chan Event -> ConnectionId -> MVar Bool -> [ByteString] -> IO (Either (TransportError SendErrorCode) ())
apiSend chan conn connAlive msg =
  modifyMVar connAlive $ \alive ->
    if alive
      then do
        writeChan chan (Received conn msg)
        return (alive, Right ())
      else
        return (alive, Left (TransportError SendFailed "Connection closed"))

apiClose :: Chan Event -> ConnectionId -> MVar Bool -> IO ()
apiClose chan conn connAlive =
  modifyMVar_ connAlive $ \alive -> do
    when alive . writeChan chan $ ConnectionClosed conn
    return False

--------------------------------------------------------------------------------
-- Lens definitions                                                           --
--------------------------------------------------------------------------------

channels :: Accessor TransportState (Map EndPointAddress (Chan Event))
channels = accessor _channels (\ch st -> st { _channels = ch })

nextConnectionId :: Accessor TransportState (Map EndPointAddress ConnectionId)
nextConnectionId = accessor  _nextConnectionId (\cid st -> st { _nextConnectionId = cid })

at :: Ord k => k -> String -> Accessor (Map k v) v
at k err = accessor (Map.findWithDefault (error err) k) (Map.insert k)

channelAt :: EndPointAddress -> Accessor TransportState (Chan Event)
channelAt addr = channels >>> at addr "Invalid channel"

nextConnectionIdAt :: EndPointAddress -> Accessor TransportState ConnectionId
nextConnectionIdAt addr = nextConnectionId >>> at addr "Invalid connection ID"

-- End of file.
