> {-# LANGUAGE DeriveDataTypeable         #-}
> {-# LANGUAGE DeriveGeneric              #-}
> {-# LANGUAGE DeriveTraversable          #-}
> {-# LANGUAGE FlexibleInstances          #-}
> {-# LANGUAGE GeneralizedNewtypeDeriving #-}
> {-# LANGUAGE NoImplicitPrelude          #-}
> {-# LANGUAGE OverloadedStrings          #-}
> {-# LANGUAGE StandaloneDeriving         #-}
> {-# LANGUAGE TypeSynonymInstances       #-}
> {-# LANGUAGE UndecidableInstances       #-}
>
> module BC where
>
> import           Control.Comonad.Cofree
> import qualified Crypto.Hash             as CH
> import           Crypto.Number.Serialize (os2ip)
> import qualified Data.Data               as D
> import qualified Data.Vector             as V
> import qualified Data.Binary             as B
> import qualified Data.Binary.Get         as B
> import qualified Data.ByteArray          as BA
> import qualified Data.ByteString         as BS
> import qualified Data.ByteString.Lazy    as BSL
> import qualified Data.Vector.Binary      as VB
> import qualified Data.Map.Strict         as M
> import           Data.Time.Clock.POSIX   as C
> import           Data.Time.Clock         as C
> import qualified GHC.Generics            as G
> import qualified Prelude
> import           Protolude
> import qualified System.Environment      as SE
> import qualified System.Directory        as SE

http://www.michaelburge.us/2017/08/17/rolling-your-own-blockchain.html

Rolling your Own Blockchain in Haskell
Michael Burge

- write binary serializer and deserializer
- use cryptography to calculate hashes
- adjusting difficulty of a miner in response to computation time

> newtype Account = Account Integer deriving (Eq, Ord, Num, Show)
>
> data Transaction = Transaction
>   { _from   :: Account
>   , _to     :: Account
>   , _amount :: Integer
>   } deriving (Eq, Show)
>
> newtype BlockF a = Block (V.Vector a) deriving (Eq, Foldable, Functor, Traversable, Monoid, Show)
> type Block = BlockF Transaction
>
> type HaskoinHash = CH.Digest CH.SHA1
>
> data BlockHeader = BlockHeader
>   { _miner       :: Account
>   , _parentHash  :: HaskoinHash
>   , _nonce       :: Integer
>   , _minedAt     :: C.POSIXTime
>   } deriving (Eq, Show)

MerkleF : higher-order Merkle tree that adds a layer onto some other type.

> data MerkleF a
>   = Genesis
>   | Node BlockHeader a
>   deriving (Eq, Foldable, Functor, Show, Traversable)

Recursively apply MerkleF to produce a type for all depths of Merkle trees
and attach an annotation of type Block to each node in the tree.

When using Cofree, anno :< xf will construct an annotated value.

> type Blockchain = Cofree MerkleF Block

Why MerkleF type instead of

newtype Block = Block (V.Vector Transaction)

data Blockchain
  = Genesis Block
  | Node Block BlockHeader Blockchain

reason: to get Functor, Traversable, Foldable instances: example usage:

> e =
>   let genesis_block = Block (V.fromList [])
>       genesis_chain = genesis_block :< Genesis
>       block1        = Block (V.fromList [Transaction 0 1 1000])
>       chain1        = block1 :< Node BlockHeader{ _miner = 0, _parentHash = undefined, _nonce = 0, _minedAt = C.nominalDay} genesis_chain
>       chain2        = block1 :< Node BlockHeader{ _miner = 0, _parentHash = undefined, _nonce = 0, _minedAt = C.nominalDay} chain1
>       txns          = toList $ mconcat $ toList chain2
>       totalVolume   = sum $ map _amount txns
>   in totalVolume

"inverted" tree
- each node knows its parent (rather than its children)
- if each node knew its children, adding new block to end would require changing every node in tree
- therefore, MerkleF produces a chain (not a tree)

------------------------------------------------------------------------------
Constructing Chains

module Haskoin.Mining where

> type TransactionPool = IO [Transaction]
>
> makeGenesis :: IO Blockchain
> makeGenesis = return $ Block (V.fromList []) :< Genesis

No difficulty, transaction limiting, or security:

> mineOn0 :: TransactionPool -> Account -> Blockchain -> IO Blockchain
> mineOn0 pendingTransactions minerAccount parent = do
>   ts <- pendingTransactions
>   let block  = Block (V.fromList ts)
>   let header = BlockHeader { _miner      = minerAccount
>                            , _parentHash = CH.hashlazy $ serialize parent
>                            , _nonce      = 0
>                            , _minedAt    = C.nominalDay
>                            }
>   return $ block :< Node header parent

need bytes to hash : implies need serialization : use "binary" library

> instance (B.Binary (f (Cofree f a)), B.Binary a) => B.Binary (Cofree f a) where
> instance (B.Binary a) => B.Binary (MerkleF a) where
> instance B.Binary BlockHeader where
> instance B.Binary Transaction where
> deriving instance B.Binary Account
> deriving instance B.Binary Block

> deriving instance Generic (Cofree f a)
> deriving instance Generic (MerkleF a)
> deriving instance Generic BlockHeader
> deriving instance Generic Transaction

hand-write because Digest SHA1 from Crypto.Hash does not provide it or a Generic instance

> instance B.Binary HaskoinHash where
>   get = do
>     mDigest <- CH.digestFromByteString <$> (B.get :: B.Get BS.ByteString)
>     case mDigest of
>       Nothing     -> Prelude.fail "Not a valid digest"
>       Just digest -> return digest
>   put digest = B.put $ (BA.convert digest :: BS.ByteString)

> instance B.Binary C.POSIXTime where
>   get = fromInteger <$> (B.get :: B.Get Integer)
>   put x = B.put $ (round x :: Integer)

> deserialize :: BSL.ByteString -> Blockchain
> deserialize = B.decode
>
> serialize :: Blockchain -> BSL.ByteString
> serialize = B.encode

> chain :: IO Blockchain
> chain = do
>   let txnPool = return []
>   chain <- makeGenesis
>   chain <- mineOn txnPool 0 chain
>   chain <- mineOn txnPool 0 chain
>   chain <- mineOn txnPool 0 chain
>   chain <- mineOn txnPool 0 chain
>   chain <- mineOn txnPool 0 chain
>   return chain

> en = chain >>= return . serialize
> de = chain >>= return . deserialize . serialize

------------------------------------------------------------------------------
Mining

problems with above
1. can have negative balances
2. no transaction limiting : create huge block and run miners out of memory
3. mine empty blocks, so nobody can transfer money
4. no difficulty, so miners not proving they’ve done any work

 #1

> blockReward = 1000

> balances :: Blockchain -> M.Map Account Integer
> balances bc =
>   let txns = toList $ mconcat $ toList bc
>       debits  = map (\Transaction{ _from = acc, _amount = amount} -> (acc, -amount)) txns
>       credits = map (\Transaction{ _to   = acc, _amount = amount} -> (acc,  amount)) txns
>       minings = map (\h -> (_miner h, blockReward)) $ headers bc
>   in M.fromListWith (+) $ debits ++ credits ++ minings

Given parent blockchain, filter invalid transactions

> validTransactions :: Blockchain -> [Transaction] -> [Transaction]
> validTransactions bc txns =
>   let accounts     = balances bc
>       validTxn txn = case M.lookup (_from txn) accounts of
>         Nothing -> False
>         Just balance -> balance >= _amount txn
>   in filter validTxn txns

 #2 : let current miner choose however # transactions to put in block

globalTransactionLimit = 1000 at the top to use when mining
but we will not verify past blocks using it.

 #4, add nonce field to BlockHeader : miner increment good hash found

Make it large integer to avoid scenario where no nonce values yield a sufficiently-difficult hash.

Want to adjust difficulty so blocks take roughly the same time to mine, so store a timestamp in the header.

> globalTransactionLimit = 1000

enter loop; incrementing counter and fetching time until candidate with right difficulty found

> mineOn :: TransactionPool -> Account -> Blockchain -> IO Blockchain
> mineOn pendingTransactions minerAccount parent = do
>   ts <- pendingTransactions
>   ts <- return $ validTransactions parent ts
>   ts <- return $ take globalTransactionLimit ts
>   loop ts 0
>   where
>     validChain bc = difficulty bc < desiredDifficulty parent
>     loop ts nonce = do
>       now <- getPOSIXTime
>       let header = BlockHeader
>            { _miner      = minerAccount
>            , _parentHash = CH.hashlazy $ serialize parent
>            , _nonce      = nonce
>            , _minedAt    = now
>            }
>           block = Block (V.fromList ts)
>           candidate = block :< Node header parent
>       if validChain candidate
>         then return candidate
>         else loop ts (nonce+1)
>
> difficulty :: Blockchain -> Integer
> difficulty bc = os2ip $ (CH.hashlazy $ serialize bc :: HaskoinHash)

what is right difficulty?

start : calculate average time-between-blocks for last 100 blocks

> numBlocksToCalculateDifficulty = 100
>
> blockTimeAverage :: Blockchain -> C.NominalDiffTime
> blockTimeAverage bc = average $ zipWith (-) times (Prelude.tail times)
>   where times = take numBlocksToCalculateDifficulty $ map _minedAt $ headers bc
>
> headers :: Blockchain -> [BlockHeader]
> headers (_ :< Node x next) = x : headers next
> headers _                  = []
>
> average :: (Foldable f, Num a, Fractional a, Eq a) => f a -> a
> average xs = sum xs / (if d == 0 then 1 else d) where d = fromIntegral $ length xs

make target time be 10 seconds

Say blockTimeAverage bc gives 2 seconds
- want blocks to take 5 times as long: adjustmentFactor = targetTime / blockTimeAverage bc = 5
- means only 1/5 of the originally-accepted blocks to be accepted

since hashes uniformly-distributed, 1/5 of original hashes are less than originalDifficulty / 5
- will be new difficulty
- That’s what Bitcoin does: difficulty = oldDifficulty * (2 weeks) / (time for past 2015 blocks).

> genesisBlockDifficulty = 0xFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
> targetTime = 10

> -- BEWARE: O(n * k), where k = numBlocksToCalculateDifficulty
> desiredDifficulty :: Blockchain -> Integer
> desiredDifficulty x = round $ loop x
>   where
>     loop   (_ :< Genesis)   = genesisBlockDifficulty
>     loop x@(_ :< Node _ xs) = oldDifficulty / adjustmentFactor
>       where
>         oldDifficulty    = loop xs
>         adjustmentFactor = min 4.0 $ targetTime / (blockTimeAverage x)

> exampleChain = chain >>= \c -> mineOn (return []) 0 c -- Repeat a bunch of times
> -- xx = exampleChain >>= \c -> mapM_ print $ map blockTimeAverage c

6.61261425s
6.73220925s
7.97893375s
12.96145975s
10.923974s
9.59857375s
7.1819445s
2.2767425s
3.2307515s
7.215131s
15.98277575s

hover around 10s because targetTime = 10

------------------------------------------------------------------------------
Persistence

save on
- tool to mine blocks and create a new chain

> defaultChainFile = "main.chain"
> defaultAccount = "10"
>
> mineBlocks :: [Prelude.String] -> IO ()
> mineBlocks args = do
>   let (filename, accountS) = case args of
>        []                  -> (defaultChainFile, defaultAccount)
>        [filename]          -> (filename, defaultAccount)
>        [filename, account] -> (filename, account)
>        _                   -> panic "Usage: mine [filename] [account]"
>   let swapFile = filename ++ ".tmp"
>       txnPool  = return []
>       account  = Account $ Prelude.read accountS
>   forever $ do
>     chain <- loadOrCreate filename makeGenesis :: IO Blockchain
>     newChain <- mineOn txnPool account chain
>     B.encodeFile swapFile newChain
>     SE.copyFile swapFile filename
>     print "Block mined and saved!"
>
> loadOrCreate :: B.Binary a => FilePath -> (IO a) -> IO a
> loadOrCreate filename init = do
>   exists <- SE.doesFileExist filename
>   if exists then
>     B.decodeFile filename
>   else do
>     x <- init
>     B.encodeFile filename x
>     return x

prints all account balances

> printAccountBalances :: [Prelude.String] -> IO ()
> printAccountBalances args = do
>   let (filename) = case args of
>        []         -> (defaultChainFile)
>        [filename] -> (filename)
>        _          -> panic "Usage: list-balances [filename]"
>   chain <- B.decodeFile filename :: IO Blockchain
>   forM_ (M.toAscList $ balances chain) $ \(account, balance) -> do
>     print (account, balance)

$ stack exec list-balances
(Account 10,23000)

mined 23 blocks

