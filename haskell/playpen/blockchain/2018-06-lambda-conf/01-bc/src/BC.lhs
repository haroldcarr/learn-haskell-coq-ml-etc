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

> ------------------------------------------------------------------------------
> data Block = Block
>   { bPrevHash :: BHash
>   , bIndex    :: BIndex
>   , bTXs      :: [Transaction]
>   , bProof    :: Proof
>   } deriving (Eq, Read, Show)

> type BHash       = BS.ByteString
> type BIndex      = Int
> type Transaction = T.Text
> type Proof       = Integer

> genesisBlock :: Block
> genesisBlock = Block "1" 0 [] 100

> ------------------------------------------------------------------------------
> addTransactionIO :: IOEnv -> Transaction -> IO ()
> addTransactionIO env tx =
>   A.atomicModifyIORefCAS_ env $ \e ->
>     e { eTXPool = eTXPool e ++ [tx] } -- TODO

> data Env = Env
>   { eTXPool          :: [Transaction]
>   , eChain           :: Chain
>   , eProofDifficulty :: ProofDifficulty
>   , eNodes           :: [Address]
>   , eThisNode        :: Address
>   } deriving (Eq, Show)

> type Chain           = [Block]
> type ProofDifficulty = Int
> type Address         = T.Text

> ------------------------------------------------------------------------------
> mineIO :: IOEnv -> IO Block
> mineIO env = do
>   lastBlock <- getEnvIO (last . eChain) env
>   pd <- getEnvIO eProofDifficulty env
>   let proof = proofOfWork pd lastBlock
>       pHash = hashBlock lastBlock
>   addBlockIO env pHash proof

> -- | add new block containing all TXs in pool to Chain
> addBlockIO :: IOEnv -> BHash -> Proof -> IO Block
> addBlockIO env pHash proof = do
>   A.atomicModifyIORefCAS_ env $ \e -> do
>     let b = Block pHash
>                   (length (eChain e))
>                   (eTXPool e)
>                   proof
>     e { eTXPool = [], eChain = eChain e ++ [b] } -- TODO
>   getEnvIO (last . eChain) env

> ------------------------------------------------------------------------------
> -- | Creates a SHA-256 hash of a Block
> hash :: BS.ByteString -> BHash
> hash = SHA.hash
>
> hashBlock :: Block -> BHash
> hashBlock = hash . BSC8.pack . show

> ------------------------------------------------------------------------------
> -- | Proof of Work Algorithm:
> --   - Find a number p' such that
> --         hash(p <> p' <> hlb)
> --     contains leading 4 zeroes
> --     where
> --       p is the previous proof
> --       p' is the new proof
> --       hlb is hash(lastBlock)
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
> --   lastHash  : hash of previous block
> --   proof     : current proof
> validProof :: ProofDifficulty -> Proof -> BHash -> Proof-> Bool
> validProof proofDifficulty lastProof lastHash proof =
>  let guess = evidence lastProof lastHash proof
>   in BS.take proofDifficulty guess == BSC8.replicate proofDifficulty '0'

> evidence :: Proof -> BHash -> Proof -> BHash
> evidence lastProof lastHash proof =
>  let guess = BSC8.pack (show lastProof) <> BSC8.pack (show proof) <> lastHash
>  in Hex.hex (hash guess)

> ------------------------------------------------------------------------------
> -- | CONSENSUS ALGORITHM
> --   Resolves conflicts by replacing chain with longest one in the network.
> --   Returns (updated-environment, (True, "") if chain was replaced.
> --   Returns (given-environment, (False, "") if chain was NOT replaced.
> --   Returns (given-environment, (False, failure-reason) if the new chain was not valid
> resolveConflicts :: Env -> [Chain] -> (Env, (Bool, String))
> resolveConflicts e chains = go
>  where
>   -- TODO : check that foldr looks at all results
>   go = let chain' = foldr (\a b -> if length a > length b then a else b) (eChain e) chains
>        in if eChain e /= chain' then
>             case isValidChain (eProofDifficulty e) chain' of
>               Right _  ->
>                 ( e { eChain = chain'
>                     , eTXPool = resolveTXs e chain'
>                     }
>                 , (True, ""))
>               Left err ->
>                 ( e , (False, "resolveConflicts: invalid chain " <> T.unpack err))
>           else  ( e , (False,  ""))
>   txsInChain :: Chain -> [Transaction]
>   txsInChain = foldl (\txs b -> txs ++ bTXs b) []
>   resolveTXs :: Env -> Chain -> [Transaction]
>   resolveTXs myEnv theirChain =
>     let myPool   = eTXPool myEnv
>         myTXs    = txsInChain (eChain myEnv)
>         theirTXs = txsInChain theirChain
>     in (myPool \\ theirTXs) ++ -- remove TXs from my pool that are in their chain
>        (myTXs  \\ theirTXs)    -- add TXs from my chain that are not in their chain

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
>   CM.when   (hashBlock validBlock /= bPrevHash checkBlock)
>             (Left ("invalid bPrevHash at " <> T.pack (show i)))
>   CM.unless (validProof pd (bProof validBlock) (bPrevHash checkBlock) (bProof checkBlock))
>             (Left ("invalid bProof at "    <> T.pack (show i)))
>   return ()

------------------------------------------------------------------------------
-- IO

> type IOEnv = IOR.IORef Env

> getEnvIO :: (Env -> a) -> IOEnv -> IO a
> getEnvIO f = fmap f . IOR.readIORef

> initialize :: Address -> IO IOEnv
> initialize thisNode =
>   IOR.newIORef (Env [] [genesisBlock] 4 [] thisNode)

> -- | Add a new node to the list of nodes
> --   Example argument: "http://192.168.0.5:5000"
> registerNodeIO :: IOEnv -> Address -> IO ()
> registerNodeIO env address =
>   A.atomicModifyIORefCAS_ env $ \e ->
>     e { eNodes = address:eNodes e }

> resolveConflictsIO :: IOEnv -> IO Bool
> resolveConflictsIO env = do
>   nodes  <- getEnvIO eNodes env
>   chains <- CM.forM nodes $ \n -> do
>     (status, body) <- httpRequest ("http://" <> T.unpack n <> "/chain-only")
>     return $ if status == 200 then read (BSLC8.unpack body)
>              else []
>   (b, err) <- IOR.atomicModifyIORef' env $ \e -> resolveConflicts e chains
>   if b then return b else do Log.infoM lBC err; return b

> run :: [String] -> IO ()
> run args = do
>   Log.updateGlobalLogger lBC (Log.setLevel Log.DEBUG)
>   let port = case args of
>        ("-p":p:_) -> read p
>        ("-h":_)   -> error "'-p', '--port', default=5000, 'port to listen on'"
>        _          -> 3000
>   env <- initialize (T.pack (show port))
>   run' port env

> run' :: Int -> IOEnv -> IO ()
> run' httpPort env = do
>   Log.infoM lBC ("starting httpServer on port " <> show httpPort)
>   tn <- fmap (T.unpack . eThisNode) (IOR.readIORef env)
>   Wai.run httpPort $ Wai.logStdoutDev $
>     \req s -> do
>       Log.infoM lBC (tn <> " received request " <> show req)
>       case Wai.rawPathInfo req of
>         "/mine" -> do
>           b <- fmap show (mineIO env)
>           send s tn H.status200 ("mine " <> b)
>         "/tx" -> -- POST
>           case getQ req of
>             Right tx -> do
>               addTransactionIO env (TE.decodeUtf8 tx)
>               sendTxToPeers env (BSL.fromStrict tx)
>               send s tn H.status200 ("/tx " <> show tx)
>             Left x ->
>               badQ s tn "/tx" x
>         "/tx-no-forward" -> -- POST
>           case getQ req of
>             Right tx -> do
>               addTransactionIO env (TE.decodeUtf8 tx)
>               send s tn H.status200 ("/tx-no-forward " <> show tx)
>             Left x ->
>               badQ s tn "/tx-no-forward" x
>         "/chain" -> do
>           chain <- getEnvIO eChain env
>           send s tn H.status200 ("chain " <> show (length chain) <> " " <> show chain)
>         "/chain-only" -> do
>           e <- getEnvIO id env
>           send' s H.status200 (show (eChain e))
>         "/register" ->
>           case getQ req of
>             Right n -> do
>               registerNodeIO env (TE.decodeUtf8 n)
>               send s tn H.status200 ("/register " <> show n)
>             Left x ->
>               badQ s tn "/register" x
>         "/resolve" -> do
>           b <- resolveConflictsIO env
>           send s tn H.status200 ("/resolve " <> show b)
>         "/env" -> do
>           e <- getEnvIO id env
>           send s tn H.status200 (show e)
>         x ->
>           send s tn H.status400 ("received unknown " <> BSC8.unpack x)
>  where
>   send s tn sc r = send' s sc (tn <> " " <> r)
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

> sendTxToPeers :: IOEnv -> BSL.ByteString -> IO ()
> sendTxToPeers env tx = do
>   nodes <- getEnvIO eNodes env
>   CM.forM_ nodes $ \n ->
>     httpRequest ("http://" <> T.unpack n <> "/tx-no-forward?" <> BSLC8.unpack tx)
