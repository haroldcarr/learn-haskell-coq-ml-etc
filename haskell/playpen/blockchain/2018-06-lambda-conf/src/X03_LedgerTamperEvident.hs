{-# OPTIONS_GHC -Wno-unused-do-bind #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module X03_LedgerTamperEvident where

import qualified Control.Concurrent.Async           as Async
import qualified Crypto.Hash.SHA256                 as SHA
import qualified Data.Atomics                       as A
import qualified Data.ByteString                    as BS
import qualified Data.Concurrent.Queue.MichaelScott as Q
import qualified Data.IORef                         as IOR
import           Data.Monoid                        ((<>))
import qualified Data.Sequence                      as Seq
import qualified Data.Text                          as T
import qualified Data.Text.Encoding                 as TE
import qualified Prelude
import           RIO
import qualified System.Log.Logger                  as Log
------------------------------------------------------------------------------
import           Config
import           Ledger
import           Logging
import           X00_Base
import           X02_LedgerWithPool                 (miner)

type BHash = ByteString
type BData = ByteString

data Block = Block
  { bPrevHash :: ! BHash -- ^ hash of previous block
  , bData     :: ! BData -- ^ this block's data
  } deriving (Eq, Show)

calculateHash :: BHash -> BData -> BHash
calculateHash p d = SHA.hash (BS.concat [p, d]) -- TODO: serialize

genesisBlock :: Block
genesisBlock = Block
  { bPrevHash = "0"
  , bData     = "March 22, 2018"
  }

-- | Returns `Right ()` if valid.
-- otherwise `Left reason`
isValidBlockchain :: Seq.Seq Block -> Either T.Text ()
isValidBlockchain bc = do
  when (Seq.length bc == 0)                                   (Left "empty blockchain")
  when (Seq.length bc == 1 && Seq.index bc 0 /= genesisBlock) (Left "invalid genesis block")
  let elements = toList bc
    -- `sequence_` causes function to return on/with first `Left` value
  sequence_ (map isValidBlock (Prelude.zip3 [1 .. ] elements (Prelude.tail elements)))
  return ()

-- | Given a valid previous block and a block to check.
-- | Returns `Just ()` if valid.
-- otherwise `Left reason`
isValidBlock :: (Int, Block, Block) -> Either Text ()
isValidBlock (i, validBlock, checkBlock) =
  if hashBlock validBlock /= bPrevHash checkBlock then
    Left ("invalid bPrevHash at " <> T.pack (show i))
  else
    Right ()
 where
  hashBlock b = calculateHash (bPrevHash b) (bData b)

createLedgerWithBlocks
  :: IO (Ledger Block)
createLedgerWithBlocks = do
  r <- IOR.newIORef (Seq.singleton genesisBlock)
  return Ledger
    { lContents = IOR.readIORef r

    , lCommit   = \_ (Block _ d) -> A.atomicModifyIORefCAS_ r $ \existing ->
        let l             = Seq.length existing
            (Block ph pd) = Seq.index existing (l - 1)
            h             = calculateHash ph pd
        in existing Seq.|> Block h d

    , lModify   = \i a -> A.atomicModifyIORefCAS_ r $ \existing -> Seq.update i a existing

    , lCheck    = do
        c <- IOR.readIORef r
        return $
          case isValidBlockchain c of
            Left err -> Just err
            _        -> Nothing

    , fromText  = Block "0" . TE.encodeUtf8
    }

runLedgerTamperEvident :: IO ()
runLedgerTamperEvident = do
  l <- createLedgerWithBlocks
  q <- Q.newQ
  let e            = defaultConfig
      committer    = lCommit l e
      txHandler tx = do
        Q.pushL q tx
        Log.infoM lTAMPEREVIDENT ("POOLED: " <> show tx)
  Async.replicateConcurrently_ (cNumMiners (getConfig e)) (miner q committer)
   `Async.concurrently_`
   runServerAndClients e l txHandler
