{-# LANGUAGE DeriveDataTypeable #-}

module NT
  ( -- * Types
    Transport(..)
  , EndPoint(..)
  , Connection(..)
  , Event(..)
  , ConnectionId
  , EndPointAddress(..)
    -- * Error codes
  , TransportError(..)
  , NewEndPointErrorCode(..)
  , ConnectErrorCode(..)
  , SendErrorCode(..)
  , EventErrorCode(..)
  ) where

import           Control.Applicative   ((<$>))
import           Control.Exception     (Exception)
import           Data.Binary           (Binary (get, put))
import           Data.ByteString       (ByteString)
import qualified Data.ByteString       as BS (copy)
import qualified Data.ByteString.Char8 as BSC (unpack)
import           Data.Typeable         (Typeable)
import           Data.Word             (Word64)

data Transport = Transport {
    newEndPoint    :: IO (Either (TransportError NewEndPointErrorCode) EndPoint)
  , closeTransport :: IO ()
  }

data EndPoint = EndPoint {
    receive       :: IO Event
  , address       :: EndPointAddress
  , connect       :: EndPointAddress -> IO (Either (TransportError ConnectErrorCode) Connection)
  , closeEndPoint :: IO ()
  }

data Connection = Connection {
    send  :: [ByteString] -> IO (Either (TransportError SendErrorCode) ())
  , close :: IO ()
  }

data Event =
    Received {-# UNPACK #-} !ConnectionId [ByteString]
  | ConnectionClosed {-# UNPACK #-} !ConnectionId
  | ConnectionOpened {-# UNPACK #-} !ConnectionId EndPointAddress
  | EndPointClosed
  | ErrorEvent (TransportError EventErrorCode)
  deriving (Show, Eq)

type ConnectionId = Word64

newtype EndPointAddress = EndPointAddress { endPointAddressToByteString :: ByteString }
  deriving (Eq, Ord, Typeable)

instance Binary EndPointAddress where
  put = put . endPointAddressToByteString
  get = EndPointAddress . BS.copy <$> get

instance Show EndPointAddress where
  show = BSC.unpack . endPointAddressToByteString

data TransportError error = TransportError error String
  deriving (Show, Typeable)

instance (Typeable err, Show err) => Exception (TransportError err)

instance Eq error => Eq (TransportError error) where
  TransportError err1 _ == TransportError err2 _ = err1 == err2

data NewEndPointErrorCode =
    NewEndPointInsufficientResources
  | NewEndPointFailed
  deriving (Show, Typeable, Eq)

data ConnectErrorCode =
    ConnectNotFound
  | ConnectInsufficientResources
  | ConnectTimeout
  | ConnectFailed
  deriving (Show, Typeable, Eq)

data SendErrorCode =
    SendClosed
  | SendFailed
  deriving (Show, Typeable, Eq)

data EventErrorCode =
    EventEndPointFailed
  | EventTransportFailed
  | EventConnectionLost EndPointAddress
  deriving (Show, Typeable, Eq)

-- End of file.
