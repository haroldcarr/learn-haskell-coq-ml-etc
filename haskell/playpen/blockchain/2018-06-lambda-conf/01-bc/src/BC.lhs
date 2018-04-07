> {-# OPTIONS_GHC -fno-warn-missing-signatures #-}
> {-# OPTIONS_GHC -fno-warn-unused-do-bind     #-}
> {-# LANGUAGE NoImplicitPrelude #-}
> {-# LANGUAGE OverloadedStrings #-}
>
> module BC where
>
> import qualified Control.Monad                        as CM
> import qualified Crypto.Hash.SHA256                   as SHA
> import qualified Data.ByteString                      as BS
> import qualified Data.ByteString.Lazy                 as BSL
> import qualified Data.ByteString.Builder              as BSB
> import qualified Data.ByteString.Char8                as BSC8
> import qualified Data.ByteString.Lazy.Char8           as BSLC8
> import           Data.List                            ((\\), last)
> import qualified Data.Hex                             as Hex
> import qualified Data.IORef                           as IOR
> import           Data.Monoid                          ((<>))
> import qualified Data.Text                            as T
> import qualified Network.HTTP.Types                   as H
> import qualified Network.HTTP.Client                  as H
> import qualified Network.Wai                          as Wai
> import qualified Network.Wai.Handler.Warp             as Wai
> import qualified Network.Wai.Middleware.RequestLogger as Wai
> import qualified Prelude                              as P
> import           Prelude                              ((!!))
> import           Protolude                            hiding (hash)
> import qualified System.Log.Logger                    as Log
> import           Test.Hspec

> lBC :: P.String
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
> type Transaction = BS.ByteString
> type Proof       = Integer

> genesisBlock :: Block
> genesisBlock = Block "1" 0 [] 100

> ------------------------------------------------------------------------------
> addTransaction :: BCState -> Transaction -> BCState
> addTransaction e tx =
>   e { bcTXPool = bcTXPool e ++ [tx] } -- TODO

> data BCState = BCState
>   { bcTXPool          :: [Transaction]
>   , bcChain           :: Chain
>   , bcProofDifficulty :: ProofDifficulty
>   } deriving (Eq, Show)

> type Chain           = [Block]
> type ProofDifficulty = Int

> initialBCState :: BCState
> initialBCState = BCState [] [genesisBlock] 4

> testAddTransaction =
>   describe "testAddTransaction" $do
>     it "empty bcTXPool in initialState" $
>       initialBCState
>       `shouldBe`
>       BCState { bcTXPool = []
>               , bcChain = [Block {bPrevHash = "1", bIndex = 0, bTXs = [], bProof = 100}]
>               , bcProofDifficulty = 4}
>     it "adds to bcTXPool" $
>       addTransaction initialBCState "TX1"
>       `shouldBe`
>       BCState { bcTXPool = ["TX1"]
>               , bcChain = [Block {bPrevHash = "1", bIndex = 0, bTXs = [], bProof = 100}]
>               , bcProofDifficulty = 4}

> ------------------------------------------------------------------------------
> mine :: BCState -> (BCState, Block)
> mine e =
>   let lastBlock = last (bcChain e)
>       pd        = bcProofDifficulty e
>       proof     = proofOfWork pd lastBlock
>       pHash     = hashBlock lastBlock
>    in addBlock e pHash proof

> -- | add new block containing all TXs in pool to Chain
> addBlock :: BCState -> BHash -> Proof -> (BCState, Block)
> addBlock e pHash proof =
>   let b = Block pHash
>                 (length (bcChain e))
>                 (bcTXPool e)
>                 proof
>       e' = e { bcTXPool = [], bcChain = bcChain e ++ [b] } -- TODO
>    in (e', last (bcChain e'))

> testMine =
>   describe "testMine" $do
>     it "addBlock" $
>       let (s, _) = mine (addTransaction (addTransaction initialBCState "TX1") "TX2")
>        in s `shouldBe`
>           BCState { bcTXPool = []
>                   , bcChain = [Block { bPrevHash = "1", bIndex = 0, bTXs = [], bProof = 100}
>                               ,Block { bPrevHash = "\202\169KU\139\ETX\212\&3W\145`\229\224S\159\177\253\nF\167\158\227\250\255\244\v\207\228z\233\171\237"
>                                      , bIndex = 1
>                                      , bTXs = ["TX1","TX2"]
>                                      , bProof = 134530}]
>                   , bcProofDifficulty = 4}

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

> (bcTX1,_) = mine (addTransaction initialBCState "TX1")
>
> testProofOfWork =
>   describe "proofOfWork" $ do
>     it "from genesisBlock" $
>       proofOfWork 4 genesisBlock `shouldBe`  bProof (bcChain bcTX1 !! 1)
>     it "from genesisBlock + 1" $
>       proofOfWork 4 (bcChain bcTX1 !! 1) `shouldBe` 52668

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

> testEvidence =
>   describe "evidence" $ do
>     it "from genesisBlock" $
>       evidence (bProof genesisBlock) (hashBlock genesisBlock) (proofOfWork 4 genesisBlock)
>       `shouldBe`
>      "0000DDF9FB09F9A9C5A0EF57DC3E2916633BEDB95B38D54BDBFFF0B7D4D6E515"
>     it "from genesisBlock + 1" $
>       evidence (bProof (bcChain bcTX1 !! 1)) (hashBlock (bcChain bcTX1 !! 1))  52668
>       `shouldBe`
>       "000072456DC7CC3975C0CC3543B6BA201E6F4D056679970C3644D2DDEB4EEA67"

> ------------------------------------------------------------------------------
> -- | CONSENSUS ALGORITHM
> --   Resolves conflicts by replacing chain with longest one in the network.
> --   Returns (updated-environment, (True, "") if chain was replaced.
> --   Returns (given-environment, (False, "") if chain was NOT replaced.
> --   Returns (given-environment, (False, failure-reason) if the new chain was not valid
> resolveConflicts :: BCState -> [Chain] -> (BCState, (Bool, P.String))
> resolveConflicts e chains = go
>  where
>   -- TODO : check that foldr looks at all results
>   go = let chain' = foldr (\a b -> if length a > length b then a else b) (bcChain e) chains
>        in if bcChain e /= chain' then
>             case isValidChain (bcProofDifficulty e) chain' of
>               Right _  ->
>                 ( e { bcChain = chain'
>                     , bcTXPool = resolveTXs e chain'
>                     }
>                 , (True, ""))
>               Left err ->
>                 ( e , (False, "resolveConflicts: invalid chain " <> T.unpack err))
>           else  ( e , (False,  ""))
>   txsInChain :: Chain -> [Transaction]
>   txsInChain = foldl (\txs b -> txs ++ bTXs b) []
>   resolveTXs :: BCState -> Chain -> [Transaction]
>   resolveTXs myBCState theirChain =
>     let myPool   = bcTXPool myBCState
>         myTXs    = txsInChain (bcChain myBCState)
>         theirTXs = txsInChain theirChain
>     in (myPool \\ theirTXs) ++ -- remove TXs from my pool that are in their chain
>        (myTXs  \\ theirTXs)    -- add TXs from my chain that are not in their chain


> eTX0 :: BCState
> (eTX0,_) = mine (addTransaction initialBCState "TX-0")
>
> eLongerChainAndPoolUpdateIn :: BCState
> eLongerChainAndPoolUpdateIn = initialBCState { bcTXPool = ["TX-0","TX-should-stay"] }
>
> e1LongerChainAndPoolUpdateOut :: BCState
> e1LongerChainAndPoolUpdateOut = eTX0 { bcTXPool = ["TX-should-stay"] }
>
> e1BadPHash :: BCState
> e1BadPHash = eTX0 { bcChain = makeChain "X" 658 }
>
> e1BadProof :: BCState
> e1BadProof = eTX0 { bcChain = makeChain (hashBlock genesisBlock) 0 }
>
> makeChain :: BHash -> Proof -> Chain
> makeChain ph p =
>   [genesisBlock
>   ,Block { bPrevHash = ph
>          , bIndex = 1, bTXs = ["TX-0"]
>          , bProof = p}]
>
> e1NotLost :: BCState
> (e1NotLost,_) = mine (addTransaction (addTransaction initialBCState "TX1") "TX2")
>
> e2NotLost :: BCState
> e2NotLost =
>   let (etx1,_) = mine (addTransaction initialBCState "TX1")
>       (etx2,_) = mine (addTransaction etx1       "TX3")
>    in  etx2
>
> e1NotLastAfterResolve :: BCState
> e1NotLastAfterResolve = e2NotLost { bcTXPool = ["TX2"] }
>
> testResolveConflicts =
>   describe "ResolveConflicts" $ do
>     it "found longer chain" $
>       resolveConflicts initialBCState  [bcChain eTX0]
>       `shouldBe` (eTX0 , (True , ""))
>     it "no    longer chain" $
>       resolveConflicts eTX0          [bcChain initialBCState]
>       `shouldBe` (eTX0 , (False, ""))
>     it "found longer chain and pool update" $
>       resolveConflicts eLongerChainAndPoolUpdateIn  [bcChain eTX0]
>       `shouldBe` (e1LongerChainAndPoolUpdateOut, (True , ""))
>     it "invalid previous hash" $
>       resolveConflicts initialBCState  [bcChain e1BadPHash]
>       `shouldBe` (initialBCState
>                  , (False, "resolveConflicts: invalid chain invalid bPrevHash at 1"))
>     it "invalid proof of work" $
>       resolveConflicts initialBCState  [bcChain e1BadProof]
>       `shouldBe` (initialBCState
>                  , (False, "resolveConflicts: invalid chain invalid bProof at 1"))
>     it "should not drop TX" $
>       resolveConflicts e1NotLost   [bcChain e2NotLost]
>       `shouldBe` (e1NotLastAfterResolve
>                  , (True, ""))

> -- | Determine if a given blockchain is valid
> isValidChain :: ProofDifficulty -> Chain -> Either T.Text ()
> isValidChain pd bc = do
>   CM.when (null bc)                                     (Left "empty blockchain")
>   CM.when (length bc == 1 && P.head bc /= genesisBlock) (Left "invalid genesis block")
>   -- `sequence_` causes function to return on/with first `Left` value
>   sequence_ (map (isValidBlock pd) (P.zip3 [1 .. ] bc (P.tail bc)))
>   return ()

> testIsValidChain =
>   describe "isValidChain" $ do
>     it "invalid empty" $
>       isValidChain 4 [] `shouldBe` Left "empty blockchain"
>     it "valid genesis" $
>       isValidChain 4 [genesisBlock] `shouldBe` Right ()
>     it "invalid genesis" $
>       let bg = genesisBlock { bIndex = 6 }
>       in isValidChain 4 [bg] `shouldBe` Left "invalid genesis block"
>     it "valid eTX0" $
>       isValidChain 4 (bcChain eTX0) `shouldBe` Right ()
>     it "invalid previous hash" $
>       isValidChain 4 (bcChain e1BadPHash) `shouldBe` Left "invalid bPrevHash at 1"
>     it "invalid proof" $
>       isValidChain 4 (bcChain e1BadProof) `shouldBe` Left "invalid bProof at 1"

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

> testIsValidBlock =
>   describe "isValidBlock" $ do
>     it "valid eTX0" $
>       isValidBlock 4 (1, genesisBlock, bcChain eTX0 !! 1)
>       `shouldBe` Right ()
>     it "invalid previous hash" $
>       isValidBlock 4 (1, genesisBlock, bcChain e1BadPHash !! 1)
>       `shouldBe` Left "invalid bPrevHash at 1"
>     it "invalid proof" $
>       isValidBlock 4 (1, genesisBlock, bcChain e1BadProof !! 1)
>       `shouldBe` Left "invalid bProof at 1"

------------------------------------------------------------------------------
-- IO

> data IOState = IOState
>   { eBCState  :: BCState
>   , eNodes    :: [Address]
>   , eThisNode :: Address
>   } deriving (Eq, Show)

> type Address = P.String
> type IOEnv   = IOR.IORef IOState

> initializeIOEnv :: Address -> IO IOEnv
> initializeIOEnv thisNode =
>   IOR.newIORef (IOState initialBCState [] thisNode)

> getIOState :: (IOState -> a) -> IOEnv -> IO a
> getIOState f = fmap f . IOR.readIORef

> getBCState :: (BCState -> a) -> IOEnv -> IO a
> getBCState f = fmap (f . eBCState) . IOR.readIORef

> setBCState :: IOState -> BCState -> IOState
> setBCState i b = i { eBCState = b }

> addTransactionIO :: IOEnv -> Transaction -> IO ()
> addTransactionIO env tx =
>   atomicModifyIORef_ env $ \e ->
>     setBCState e (addTransaction (eBCState e) tx)

> -- | add new block containing all TXs in pool to Chain
> mineIO :: IOEnv -> IO Block
> mineIO env =
>   IOR.atomicModifyIORef' env $ \e ->
>     let (s,b) = mine (eBCState e)
>      in (setBCState e s, b)

> resolveConflictsIO :: IOEnv -> IO Bool
> resolveConflictsIO env = do
>   nodes  <- getIOState eNodes env
>   chains <- CM.forM nodes $ \n -> do
>     (status, body) <- httpRequest ("http://" <> n <> "/chain-only")
>     return $ if status == 200
>              then P.read (BSLC8.unpack body)
>              else []
>   (b, err) <- IOR.atomicModifyIORef' env $ \e ->
>     let (s, be) = resolveConflicts (eBCState e) chains
>      in (setBCState e s, be)
>   if b then return b else do Log.infoM lBC err; return b

> -- | Add a new node address to the list of nodes.
> --   e.g., "192.168.0.5:5000"
> registerNodeIO :: IOEnv -> Address -> IO ()
> registerNodeIO env address =
>   atomicModifyIORef_ env $ \e ->
>     e { eNodes = address:eNodes e }

> run :: [P.String] -> IO ()
> run args = do
>   Log.updateGlobalLogger lBC (Log.setLevel Log.DEBUG)
>   let port = case args of
>        ("-p":p:_) -> P.read p
>        ("-h":_)   -> P.error "'-p', '--port', default=5000, 'port to listen on'"
>        _          -> 3000
>   env <- initializeIOEnv (show port)
>   run' port env

> run' :: Int -> IOEnv -> IO ()
> run' httpPort env = do
>   Log.infoM lBC ("starting httpServer on port " <> show httpPort)
>   tn <- fmap eThisNode (IOR.readIORef env)
>   Wai.run httpPort $ Wai.logStdoutDev $
>     \req s -> do
>       Log.infoM lBC (tn <> " received request " <> show req)
>       case Wai.rawPathInfo req of
>         "/tx" -> -- POST
>           case getQ req of
>             Right tx -> do
>               addTransactionIO env tx
>               sendTxToPeers env tx
>               send200 s tn ("/tx " <> show tx)
>             Left x ->
>               badQ s tn "/tx" x
>         "/tx-no-forward" -> -- POST
>           case getQ req of
>             Right tx -> do
>               addTransactionIO env tx
>               send200 s tn ("/tx-no-forward " <> show tx)
>             Left x ->
>               badQ s tn "/tx-no-forward" x
>         "/mine" -> do
>           b <- fmap show (mineIO env)
>           send200 s tn ("mine " <> b)
>         "/resolve" -> do
>           b <- resolveConflictsIO env
>           send200 s tn ("/resolve " <> show b)
>         "/chain-only" -> do
>           chain <- getBCState bcChain env
>           send' s H.status200 (show chain)
>         "/chain" -> do
>           chain <- getBCState bcChain env
>           send200 s tn ("chain " <> show (length chain) <> " " <> show chain)
>         "/register" ->
>           case getQ req of
>             Right n -> do
>               registerNodeIO env (BSC8.unpack n)
>               send s tn H.status200 ("/register " <> show n)
>             Left x ->
>               badQ s tn "/register" x
>         "/env" -> do
>           e <- getBCState P.id env
>           send200 s tn (show e)
>         x ->
>           send s tn H.status400 ("received unknown " <> BSC8.unpack x)
>  where
>   send200 s tn = send s tn H.status200
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

> httpRequest :: P.String -> IO (Int, BSL.ByteString)
> httpRequest url = do
>   manager  <- H.newManager H.defaultManagerSettings
>   request  <- H.parseRequest url
>   response <- H.httpLbs request manager
>   return ( H.statusCode (H.responseStatus response)
>          , H.responseBody response )

> sendTxToPeers :: IOEnv -> BS.ByteString -> IO ()
> sendTxToPeers env tx = do
>   nodes <- getIOState eNodes env
>   CM.forM_ nodes $ \n ->
>     httpRequest ("http://" <> n <> "/tx-no-forward?" <> BSC8.unpack tx)

> atomicModifyIORef_ :: IOR.IORef a -> (a -> a) -> IO ()
> atomicModifyIORef_ i f =
>   IOR.atomicModifyIORef' i (\a -> (f a, ()))
