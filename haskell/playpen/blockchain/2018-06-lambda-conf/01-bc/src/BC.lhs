> {-# OPTIONS_GHC -fno-warn-unused-do-bind #-}
>
> {-# LANGUAGE OverloadedStrings #-}
>
> module BC where
>
> import qualified Control.Monad                        as CM
> import qualified Crypto.Hash.SHA256                   as SHA
> import qualified Data.Atomics                         as A
> import qualified Data.ByteString                      as BS
> import qualified Data.ByteString.Lazy                 as BSL
> import qualified Data.ByteString.Builder              as BSB
> import qualified Data.ByteString.Char8                as BSC8
> import qualified Data.ByteString.Lazy.Char8           as BSLC8
> import           Data.List                            ((\\))
> import qualified Data.Hex                             as Hex
> import qualified Data.IORef                           as IOR
> import           Data.Monoid                          ((<>))
> import qualified Data.Text                            as T
> import qualified Data.Text.Encoding                   as TE
> import qualified Network.HTTP.Types                   as H
> import qualified Network.HTTP.Client                  as H
> import qualified Network.Wai                          as Wai
> import qualified Network.Wai.Handler.Warp             as Wai
> import qualified Network.Wai.Middleware.RequestLogger as Wai
> import qualified System.Log.Logger                    as Log

> lBC :: String
> lBC = "BC"

https://hackernoon.com/learn-blockchains-by-building-one-117428612f46
https://github.com/dvf/blockchain

> data Block = Block
>   { bPreviousHash :: BHash
>   , bIndex        :: BIndex
>   , bTimestamp    :: BTimestamp
>   , bTransactions :: [Transaction]
>   , bProof        :: Proof
>   } deriving (Eq, Read, Show)

> type BHash       = BS.ByteString
> type BIndex      = Int
> type BTimestamp  = T.Text
> type Transaction = T.Text
> type Proof       = Integer

> genesisBlock :: Block
> genesisBlock = Block
>   { bPreviousHash = "1"
>   , bIndex        = 0
>   , bTimestamp    = "2018-04-01"
>   , bTransactions = []
>   , bProof        = 100
>   }

> type Address = T.Text
> type Chain   = [Block]
>
> data Env = Env
>   { eCurrentTransactions :: [Transaction]
>   , eChain               :: Chain
>   , eProofDifficulty     :: ProofDifficulty
>   , eNodes               :: [Address]
>   , eThisNode            :: Address
>   } deriving (Eq, Show)

> type ProofDifficulty = Int
> type IORefEnv = IOR.IORef Env

> initialize :: Address -> IO IORefEnv
> initialize thisNode =
>   IOR.newIORef (Env [] [genesisBlock] 4 [] thisNode)

> -- | Add a new node to the list of nodes
> --   Example argument: "http://192.168.0.5:5000"
> registerNodeIO :: IORefEnv -> Address -> IO ()
> registerNodeIO env address =
>   A.atomicModifyIORefCAS_ env $ \e ->
>     e { eNodes = address:eNodes e }

> -- | Determine if a given blockchain is valid
> isValidChain :: ProofDifficulty -> Chain -> Either T.Text ()
> isValidChain pd bc = do
>   CM.when (null bc)                                   (Left "empty blockchain")
>   CM.when (length bc == 1 && head bc /= genesisBlock) (Left "invalid genesis block")
>   -- `sequence_` causes function to return on/with first `Left` value
>   sequence_ (map (isValidBlock pd) (Prelude.zip3 [1 .. ] bc (Prelude.tail bc)))
>   return ()

> -- | Given a valid previous block and a block to check.
> --   Returns `Just ()` if valid.
> --   Otherwise `Left reason`.
> isValidBlock :: ProofDifficulty -> (Int, Block, Block) -> Either T.Text ()
> isValidBlock pd (i, validBlock, checkBlock) = do
>   CM.when   (hashBlock validBlock /= bPreviousHash checkBlock)
>             (Left ("invalid bPrevHash at " <> T.pack (show i)))
>   CM.unless (validProof pd (bProof validBlock) (bPreviousHash checkBlock) (bProof checkBlock))
>             (Left ("invalid bProof at "    <> T.pack (show i)))
>   return ()

> -- | CONSENSUS ALGORITHM
> --   Resolves conflicts by replacing chain with longest one in the network.
> --   Returns True if chain was replaced.
> resolveConflictsIO :: IORefEnv -> IO Bool
> resolveConflictsIO env = do
>   e <- IOR.readIORef env
>   chains <- CM.forM (eNodes e) $ \n -> do
>     (status, body) <- httpRequest ("http://" <> T.unpack n <> "/chain-only")
>     return $
>       if status == 200 then
>         read (BSLC8.unpack body)
>       else
>         []
>   (b, err) <- IOR.atomicModifyIORef' env $ \e0 -> resolveConflicts e0 chains
>   if b then return b else do Log.infoM lBC err; return b

> resolveConflicts :: Env -> [Chain] -> (Env, (Bool, String))
> resolveConflicts e chains = go
>  where
>   -- TODO : check that foldr looks at all results
>   go = let chain' = foldr (\a b -> if length a > length b then a else b) (eChain e) chains
>        in if eChain e /= chain' then
>             case isValidChain (eProofDifficulty e) chain' of
>               Right _  ->
>                 ( e { eChain = chain'
>                     , eCurrentTransactions = resolveTXs e chain'
>                     }
>                 , (True, ""))
>               Left err ->
>                 ( e , (False, "resolveConflicts: invalid chain " <> T.unpack err))
>           else  ( e , (False,  ""))
>   txsInChain :: Chain -> [Transaction]
>   txsInChain = foldl (\txs b -> txs ++ bTransactions b) []
>   resolveTXs :: Env -> Chain -> [Transaction]
>   resolveTXs myEnv theirChain =
>     let myPool   = eCurrentTransactions myEnv
>         myTXs    = txsInChain (eChain myEnv)
>         theirTXs = txsInChain theirChain
>     in (myPool \\ theirTXs) ++ -- remove TXs from my pool that are in their chain
>        (myTXs  \\ theirTXs)    -- add TXs from my chain that are not in their chain

> -- | Create a new Block and add it to the Chain
> --   previousHash: Hash of previous Block
> --   proof: The proof given by the Proof of Work algorithm
> addBlockIO :: IORefEnv -> BHash -> Proof -> IO Block
> addBlockIO env pHash proof = do
>   A.atomicModifyIORefCAS_ env $ \e -> do
>     let b = newBlock pHash
>                      (length (eChain e))
>                      "timestamp" -- TODO
>                      (eCurrentTransactions e)
>                      proof
>     e { eCurrentTransactions = [], eChain = eChain e ++ [b] } -- TODO
>   getLastBlockIO env

> newBlock :: BHash -> BIndex -> BTimestamp -> [Transaction] -> Proof -> Block
> newBlock pHash index ts txs proof = Block
>   { bPreviousHash = pHash
>   , bIndex        = index
>   , bTimestamp    = ts
>   , bTransactions = txs
>   , bProof        = proof
>   }

> -- | Creates a new transaction to go into the next mined Block
> --   Returns index of the Block that will hold the transaction. -- TODO : necessary?
> addTransactionIO :: IORefEnv -> Transaction -> IO BIndex
> addTransactionIO env tx = do
>   A.atomicModifyIORefCAS_ env $ \e ->
>     e { eCurrentTransactions = eCurrentTransactions e ++ [tx] } -- TODO
>   c <- IOR.readIORef env
>   return (length (eChain c))

> -- | Creates a SHA-256 hash of a Block
> hash :: BS.ByteString -> BHash
> hash = SHA.hash
>
> hashBlock :: Block -> BHash
> hashBlock = hash . BSC8.pack . show

> -- | Simple Proof of Work Algorithm:
> --   - Find a number p' such that hash(pp') contains leading 4 zeroes -- TODO
> --   - Where p is the previous proof, and p' is the new proof
> proofOfWork :: ProofDifficulty -> Block -> Proof
> proofOfWork proofDifficulty lastBlock =
>   let lastProof = bProof    lastBlock
>       lastHash  = hashBlock lastBlock
>       go proof next =
>         if validProof proofDifficulty lastProof lastHash proof
>         then proof
>         else next
>   in foldr go 0 [0 .. ]

> -- | Validates the Proof
> --   lastProof : proof of previous block
> --   proof     : current proof
> --   lastHash  : hash of previous block
> --   Returns   : True if valid
> validProof :: ProofDifficulty -> Proof -> BHash -> Proof-> Bool
> validProof proofDifficulty lastProof lastHash proof =
>  let guess = evidence lastProof lastHash proof
>   in BS.take proofDifficulty guess == BSC8.replicate proofDifficulty '0'

> evidence :: Proof -> BHash -> Proof -> BHash
> evidence lastProof lastHash proof =
>  let guess = BSC8.pack (show lastProof) <> BSC8.pack (show proof) <> lastHash
>  in Hex.hex (hash guess)

> getLastBlockIO :: IORefEnv -> IO Block
> getLastBlockIO env = do
>   c <- IOR.readIORef env
>   return (last (eChain c))

> mineIO :: IORefEnv -> IO Block
> mineIO env = do
>   lastBlock <- getLastBlockIO env
>   pd <- fmap eProofDifficulty (IOR.readIORef env)
>   let proof = proofOfWork pd lastBlock
>   tn <- fmap eThisNode (IOR.readIORef env)
>   -- miner receives a reward for finding the proof.
>   -- sender "0" signifies minted a new coin.
>   let tx = "sender=0;recipient=" <> tn <> ";amount=1"
>   addTransactionIO env tx
>   -- add new Block to chain
>   let pHash = hashBlock lastBlock
>   addBlockIO env pHash proof

> run :: [String] -> IO ()
> run args = do
>   Log.updateGlobalLogger lBC (Log.setLevel Log.DEBUG)
>   let port = case args of
>        ("-p":p:_) -> read p
>        ("-h":_)   -> error "'-p', '--port', default=5000, 'port to listen on'"
>        _          -> 3000
>   env <- initialize (T.pack (show port))
>   run' port env

> run' :: Int -> IORefEnv -> IO ()
> run' httpPort env = do
>   Log.infoM lBC ("starting httpServer on port " <> show httpPort)
>   tn <- fmap (T.unpack . eThisNode) (IOR.readIORef env)
>   Wai.run httpPort $ Wai.logStdoutDev $
>     \req s -> do
>       Log.infoM lBC (tn <> " received request " <> show req)
>       case Wai.rawPathInfo req of
>         "/mine" -> do
>           b <- fmap show (mineIO env)
>           let rsp = "mine " <> b
>           send s tn H.status200 rsp
>         "/tx" -> -- POST
>           case getQ req of
>             Right tx -> do
>               i <- addTransactionIO env (TE.decodeUtf8 tx)
>               sendTxToPeers env (BSL.fromStrict tx)
>               let rsp = "/tx " <> show tx <> " " <> show i
>               send s tn H.status200 rsp
>             Left x ->
>               badQ s tn "/tx" x
>         "/tx-no-forward" -> -- POST
>           case getQ req of
>             Right tx -> do
>               i <- addTransactionIO env (TE.decodeUtf8 tx)
>               let rsp = "/tx-no-forward " <> show tx <> " " <> show i
>               send s tn H.status200 rsp
>             Left x ->
>               badQ s tn "/tx-no-forward" x
>         "/chain" -> do
>           e <- IOR.readIORef env
>           let chain = eChain e
>               len   = length chain
>               rsp = "chain " <> show len <> " " <> show chain
>           send s tn H.status200 rsp
>         "/chain-only" -> do
>           e <- IOR.readIORef env
>           send' s H.status200 (show (eChain e))
>         "/register" ->
>           case getQ req of
>             Right n -> do
>               registerNodeIO env (TE.decodeUtf8 n)
>               let rsp = "/register " <> show n
>               send s tn H.status200 rsp
>             Left x ->
>               badQ s tn "/register" x
>         "/resolve" -> do
>           b <- resolveConflictsIO env
>           let rsp = "/resolve " <> show b
>           send s tn H.status200 rsp
>         "/env" -> do
>           e <- IOR.readIORef env
>           send s tn H.status200 (show e)
>         x -> do
>           let rsp = "received unknown " <> BSC8.unpack x
>           send s tn H.status400 rsp
>  where
>   send s tn sc r = do
>     let rsp = tn <> " " <> r
>     send' s sc rsp
>   send' s sc rsp = do
>     Log.infoM lBC rsp
>     s $ Wai.responseBuilder sc [] (BSB.byteString (BSC8.pack rsp))
>   getQ r =
>     case Wai.queryString r of ((q,_):_) -> Right q; x -> Left x
>   badQ s tn msg q = do
>     let rsp = tn <> " " <> msg <> " with bad query" <> show q
>     Log.infoM lBC rsp
>     send s tn H.status400 rsp

> httpRequest :: String -> IO (Int, BSL.ByteString)
> httpRequest url = do
>   manager  <- H.newManager H.defaultManagerSettings
>   request  <- H.parseRequest url
>   response <- H.httpLbs request manager
>   return ( H.statusCode (H.responseStatus response)
>          , H.responseBody response )

> sendTxToPeers :: IORefEnv -> BSL.ByteString -> IO ()
> sendTxToPeers env tx = do
>   e <- IOR.readIORef env
>   CM.forM_ (eNodes e) $ \n ->
>     httpRequest ("http://" <> T.unpack n <> "/tx-no-forward?" <> BSLC8.unpack tx)
