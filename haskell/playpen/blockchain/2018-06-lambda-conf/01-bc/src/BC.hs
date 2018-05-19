{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
{-# OPTIONS_GHC -fno-warn-unused-do-bind     #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module BC where

import qualified Control.Monad                        as CM
import qualified Crypto.Hash.SHA256                   as SHA
import qualified Data.ByteString                      as BS
import qualified Data.ByteString.Lazy                 as BSL
import qualified Data.ByteString.Builder              as BSB
import qualified Data.ByteString.Char8                as BSC8
import qualified Data.ByteString.Lazy.Char8           as BSLC8
import           Data.List                            ((\\), last)
import qualified Data.Hex                             as Hex
import qualified Data.IORef                           as IOR
import qualified Data.Map                             as M
import           Data.Monoid                          ((<>))
import qualified Data.Text                            as T
import qualified Network.HTTP.Types                   as H
import qualified Network.HTTP.Client                  as H
import qualified Network.Wai                          as Wai
import qualified Network.Wai.Handler.Warp             as Wai
import qualified Network.Wai.Middleware.RequestLogger as Wai
import qualified Prelude                              as P
import           Prelude                              ((!!))
import           Protolude                            hiding (hash)
import qualified System.Log.Logger                    as Log
import           Test.Hspec
------------------------------------------------------------------------------
import           Crypto -- for test
import           GCoin

debug = False
lBC   = "BC" :: P.String

-- https://hackernoon.com/learn-blockchains-by-building-one-117428612f46
-- https://github.com/dvf/blockchain

------------------------------------------------------------------------------
data Block = Block
  { bPrevHash :: BHash
  , bIndex    :: BIndex
  , bTXs      :: TXs
  , bProof    :: Proof
  } deriving (Eq, Read, Show)

type BHash       = BS.ByteString
type BIndex      = Int
type Transaction = BS.ByteString
type TXs         = [Transaction] -- TODO
type Proof       = Integer

genesisBlock :: Block
genesisBlock = Block "1" 0 [] 100

------------------------------------------------------------------------------
data BCState = BCState
  { bcTXPool          :: TXs
  , bcChain           :: Chain
  , bcProofDifficulty :: ProofDifficulty
  } deriving (Eq, Show)

type Chain           = [Block] -- TODO
type ProofDifficulty = Int

initialBCState :: BCState
initialBCState = BCState [] [genesisBlock] 4

------------------------------------------------------------------------------
addTxToPool :: BCState -> Transaction -> BCState
addTxToPool s tx =
  if txInPoolOrChain tx s then s
  else s { bcTXPool = bcTXPool s ++ [tx] } -- TODO

searchPool
  :: (Transaction -> Bool)
  -> Transaction -- fake "zero" TX
  -> TXs
  -> (Bool, Transaction)
searchPool f z = foldr (\tx txs -> if f tx then (True, tx) else txs)
                       (False, z)
searchChain
  :: (Transaction -> Bool)
  -> Transaction -- fake "zero" TX
  -> Chain
  -> (Bool, Transaction)
searchChain f z = foldr go (False, z)
 where
  go block blocks = let r@(b,_) = searchPool f z (bTXs block)
                     in if b then r else blocks

searchPoolAndChain
  :: (Transaction -> Bool)
  -> Transaction -- fake "zero" TX
  -> BCState
  -> (Bool, Transaction)
searchPoolAndChain f tx s =
  let r@(b, _) = searchPool  f tx (bcTXPool s)
  in if b then r
     else        searchChain f tx (bcChain  s)

txInPoolOrChain :: Transaction -> BCState -> Bool
txInPoolOrChain tx = fst . searchPoolAndChain (==tx) "JUNK"

testAddTxToPool =
  describe "testAddTxToPool" $ do
    it "initialState has an empty pool" $
      initialBCState
      `shouldBe`
      BCState { bcTXPool = []
              , bcChain = [Block {bPrevHash = "1", bIndex = 0, bTXs = [], bProof = 100}]
              , bcProofDifficulty = 4}
    it "adds new to pool" $
      addTxToPool (addTxToPool initialBCState "TX1") "TX2"
      `shouldBe`
      BCState { bcTXPool = ["TX1","TX2"]
              , bcChain = [Block {bPrevHash = "1", bIndex = 0, bTXs = [], bProof = 100}]
              , bcProofDifficulty = 4}
    it "does not add duplicates to pool" $ do
      let s  = addTxToPool (addTxToPool initialBCState "TX1") "TX2"
      let s' = addTxToPool (addTxToPool s              "TX1") "TX2"
       in s'
          `shouldBe`
          BCState
              { bcTXPool = ["TX1","TX2"]
              , bcChain = [Block {bPrevHash = "1", bIndex = 0, bTXs = [], bProof = 100}]
              , bcProofDifficulty = 4}

------------------------------------------------------------------------------
mine :: BCState -> (BCState, Block)
mine s =
  let lastBlock = last (bcChain s)
      pd        = bcProofDifficulty s
      proof     = proofOfWork pd lastBlock
      pHash     = hashBlock lastBlock
   in addBlock s pHash proof

-- | add new block containing all TXs in pool to Chain
addBlock :: BCState -> BHash -> Proof -> (BCState, Block)
addBlock s pHash proof =
  let b = Block pHash
                (length (bcChain s))
                (bcTXPool s)
                proof
      s' = s { bcTXPool = [], bcChain = bcChain s ++ [b] } -- TODO
   in (s', last (bcChain s'))

testMine =
  describe "testMine" $do
    it "addBlock" $
      let (s, _) = mine (addTxToPool (addTxToPool initialBCState "TX1") "TX2")
       in s `shouldBe`
         BCState { bcTXPool = []
                 , bcChain = [Block { bPrevHash = "1", bIndex = 0, bTXs = [], bProof = 100}
                             ,Block { bPrevHash = "\202\169KU\139\ETX\212\&3W\145`\229\224S\159\177\253\nF\167\158\227\250\255\244\v\207\228z\233\171\237"
                                    , bIndex = 1
                                    , bTXs = ["TX1","TX2"]
                                    , bProof = 134530}]
                 , bcProofDifficulty = 4}
    it "TXs in chain are not added to pool" $ do
      let (s, _) = mine (addTxToPool (addTxToPool initialBCState "TX1") "TX2")
      let  s'    =       addTxToPool (addTxToPool s              "TX1") "TX2"
       in s' `shouldBe`
         BCState { bcTXPool = []
                 , bcChain = [Block { bPrevHash = "1", bIndex = 0, bTXs = [], bProof = 100}
                             ,Block { bPrevHash = "\202\169KU\139\ETX\212\&3W\145`\229\224S\159\177\253\nF\167\158\227\250\255\244\v\207\228z\233\171\237"
                                    , bIndex = 1
                                    , bTXs = ["TX1","TX2"]
                                    , bProof = 134530}]
                 , bcProofDifficulty = 4}

------------------------------------------------------------------------------
-- | SHA-256 hash of a Block
hash :: BS.ByteString -> BHash
hash = SHA.hash
hashBlock :: Block -> BHash
hashBlock = BC.hash . BSC8.pack . show

------------------------------------------------------------------------------
-- | Proof of Work Algorithm:
--   - Find a number p' such that
--         hash(p <> p' <> hlb)
--     contains leading 4 zeroes
--     where
--       p is the previous proof
--       p' is the new proof
--       hlb is hash(lastBlock)
proofOfWork :: ProofDifficulty -> Block -> Proof
proofOfWork proofDifficulty lastBlock =
  let lastProof = bProof    lastBlock
      lastHash  = hashBlock lastBlock
      go proof next =
        if validProof proofDifficulty lastProof lastHash proof
        then proof
        else next
  in foldr go 0 [0 .. ]

(bcTX1,_) = mine (addTxToPool initialBCState "TX1")

testProofOfWork =
  describe "proofOfWork" $ do
    it "from genesisBlock" $
      proofOfWork 4 genesisBlock `shouldBe`  bProof (bcChain bcTX1 !! 1)
    it "from genesisBlock + 1" $
      proofOfWork 4 (bcChain bcTX1 !! 1) `shouldBe` 52668

-- | Validates the Proof
--   lastProof : proof of previous block
--   lastHash  : hash of previous block
--   proof     : current proof
validProof :: ProofDifficulty -> Proof -> BHash -> Proof-> Bool
validProof proofDifficulty lastProof lastHash proof =
 let guess = evidence lastProof lastHash proof
  in BS.take proofDifficulty guess == BSC8.replicate proofDifficulty '0'

evidence :: Proof -> BHash -> Proof -> BHash
evidence lastProof lastHash proof =
 let guess = BSC8.pack (show lastProof) <> BSC8.pack (show proof) <> lastHash
 in Hex.hex (BC.hash guess)

testEvidence =
  describe "evidence" $ do
    it "from genesisBlock" $
      evidence (bProof genesisBlock) (hashBlock genesisBlock) (proofOfWork 4 genesisBlock)
      `shouldBe`
     "0000DDF9FB09F9A9C5A0EF57DC3E2916633BEDB95B38D54BDBFFF0B7D4D6E515"
    it "from genesisBlock + 1" $
      evidence (bProof (bcChain bcTX1 !! 1)) (hashBlock (bcChain bcTX1 !! 1))  52668
      `shouldBe`
      "000072456DC7CC3975C0CC3543B6BA201E6F4D056679970C3644D2DDEB4EEA67"

------------------------------------------------------------------------------
-- | CONSENSUS ALGORITHM
--   Chooses longest chain in the network.
--   Returns (updated-environment, (True , "") if chain was replaced.
--   Returns (given-environment  , (False, "") if chain was NOT replaced.
--   Returns (given-environment  , (False, failure-reason) if the new chain was not valid
longestChain :: BCState -> [Chain] -> (BCState, (Bool, P.String))
longestChain s chains = go
 where
  go = let chain' = foldr (\a b -> if length a > length b then a else b) (bcChain s) chains
       in if bcChain s /= chain' then
            case isValidChain (bcProofDifficulty s) chain' of
              Right _  ->
                ( s { bcChain = chain'
                    , bcTXPool = resolveTXs s chain'
                    }
                , (True, ""))
              Left err ->
                ( s , (False, "longestChain: invalid chain " <> T.unpack err))
          else  ( s , (False,  ""))
  txsInChain :: Chain -> TXs
  txsInChain = foldl (\txs b -> txs ++ bTXs b) []
  resolveTXs :: BCState -> Chain -> TXs
  resolveTXs myBCState theirChain =
    let myPool   = bcTXPool myBCState
        myTXs    = txsInChain (bcChain myBCState)
        theirTXs = txsInChain theirChain
    in (myPool \\ theirTXs) ++ -- remove TXs from my pool that are in their chain
       (myTXs  \\ theirTXs)    -- add TXs from my chain that are not in their chain

sTX0 :: BCState
(sTX0,_) = mine (addTxToPool initialBCState "TX-0")

eLongerChainAndPoolUpdateIn :: BCState
eLongerChainAndPoolUpdateIn = initialBCState { bcTXPool = ["TX-0","TX-should-stay"] }

e1LongerChainAndPoolUpdateOut :: BCState
e1LongerChainAndPoolUpdateOut = sTX0 { bcTXPool = ["TX-should-stay"] }

e1BadPHash :: BCState
e1BadPHash = sTX0 { bcChain = makeChain "X" 658 }

e1BadProof :: BCState
e1BadProof = sTX0 { bcChain = makeChain (hashBlock genesisBlock) 0 }

makeChain :: BHash -> Proof -> Chain
makeChain ph p =
  [ genesisBlock
  , Block { bPrevHash = ph
          , bIndex = 1, bTXs = ["TX-0"]
          , bProof = p}]

s1NotLost :: BCState
(s1NotLost,_) = mine (addTxToPool (addTxToPool initialBCState "TX1") "TX2")

s2NotLost :: BCState
s2NotLost =
  let (etx1,_) = mine (addTxToPool initialBCState "TX1")
      (etx2,_) = mine (addTxToPool etx1       "TX3")
   in  etx2

s1NotLastAfterLongestChain :: BCState
s1NotLastAfterLongestChain = s2NotLost { bcTXPool = ["TX2"] }

testLongestChain =
  describe "LongestChain" $ do
    it "found longer chain" $
      longestChain initialBCState  [bcChain sTX0]
      `shouldBe` (sTX0 , (True , ""))
    it "no    longer chain" $
      longestChain sTX0          [bcChain initialBCState]
      `shouldBe` (sTX0 , (False, ""))
    it "found longer chain and pool update" $
      longestChain eLongerChainAndPoolUpdateIn  [bcChain sTX0]
      `shouldBe` (e1LongerChainAndPoolUpdateOut, (True , ""))
    it "invalid previous hash" $
      longestChain initialBCState  [bcChain e1BadPHash]
      `shouldBe` (initialBCState
                 , (False, "longestChain: invalid chain invalid bPrevHash at 1"))
    it "invalid proof of work" $
      longestChain initialBCState  [bcChain e1BadProof]
      `shouldBe` (initialBCState
                 , (False, "longestChain: invalid chain invalid bProof at 1"))
    it "should not drop TX" $
      longestChain s1NotLost   [bcChain s2NotLost]
      `shouldBe` (s1NotLastAfterLongestChain
                 , (True, ""))

-- | Determine if a given blockchain is valid
isValidChain :: ProofDifficulty -> Chain -> Either T.Text ()
isValidChain pd bc = do
  CM.when (null bc)                                     (Left "empty blockchain")
  CM.when (length bc == 1 && P.head bc /= genesisBlock) (Left "invalid genesis block")
  -- `sequence_` causes function to return on/with first `Left` value
  sequence_ (map (isValidBlock pd) (P.zip3 [1 .. ] bc (P.tail bc)))
  return ()

testIsValidChain =
  describe "isValidChain" $ do
    it "invalid empty" $
      isValidChain 4 [] `shouldBe` Left "empty blockchain"
    it "  valid genesis" $
      isValidChain 4 [genesisBlock] `shouldBe` Right ()
    it "invalid genesis" $
      let bg = genesisBlock { bIndex = 6 }
      in isValidChain 4 [bg] `shouldBe` Left "invalid genesis block"
    it "  valid sTX0" $
      isValidChain 4 (bcChain sTX0) `shouldBe` Right ()
    it "invalid previous hash" $
      isValidChain 4 (bcChain e1BadPHash) `shouldBe` Left "invalid bPrevHash at 1"
    it "invalid proof" $
      isValidChain 4 (bcChain e1BadProof) `shouldBe` Left "invalid bProof at 1"

-- | Given a valid previous block and a block to check.
--   Returns `Just ()` if valid.
--   Otherwise `Left reason`.
isValidBlock :: ProofDifficulty -> (Int, Block, Block) -> Either T.Text ()
isValidBlock pd (i, validBlock, checkBlock) = do
  CM.when   (hashBlock validBlock /= bPrevHash checkBlock)
            (Left ("invalid bPrevHash at " <> T.pack (show i)))
  CM.unless (validProof pd (bProof validBlock) (bPrevHash checkBlock) (bProof checkBlock))
            (Left ("invalid bProof at "    <> T.pack (show i)))
  return ()

testIsValidBlock =
  describe "isValidBlock" $ do
    it "  valid sTX0" $
      isValidBlock 4 (1, genesisBlock, bcChain sTX0 !! 1)
      `shouldBe` Right ()
    it "invalid previous hash" $
      isValidBlock 4 (1, genesisBlock, bcChain e1BadPHash !! 1)
      `shouldBe` Left "invalid bPrevHash at 1"
    it "invalid proof" $
      isValidBlock 4 (1, genesisBlock, bcChain e1BadProof !! 1)
      `shouldBe` Left "invalid bProof at 1"

------------------------------------------------------------------------------
-- IO

data IOState = IOState
  { ioBCState  :: BCState
  , ioNodes    :: [Address]
  , ioThisNode :: Address
  } deriving (Eq, Show)

type Address = P.String
type IOEnv   = IOR.IORef IOState

initializeIOEnv :: Address -> IO IOEnv
initializeIOEnv thisNode =
  IOR.newIORef (IOState initialBCState [] thisNode)

getIOState :: (IOState -> a) -> IOEnv -> IO a
getIOState f = fmap f . IOR.readIORef

getBCState :: (BCState -> a) -> IOEnv -> IO a
getBCState f = fmap (f . ioBCState) . IOR.readIORef

setBCState :: IOState -> BCState -> IOState
setBCState i b = i { ioBCState = b }

addTxToPoolIO :: IOEnv -> Transaction -> IO ()
addTxToPoolIO env tx =
  atomicModifyIORef_ env $ \s ->
    setBCState s (addTxToPool (ioBCState s) tx)

-- | add new block containing all TXs in pool to Chain
mineIO :: IOEnv -> IO Block
mineIO env =
  IOR.atomicModifyIORef' env $ \s ->
    let (s',b) = mine (ioBCState s)
     in (setBCState s s', b)

longestChainIO :: IOEnv -> IO Bool
longestChainIO env = do
  nodes  <- getIOState ioNodes env
  chains <- CM.forM nodes $ \n -> do
    (status, body) <- httpRequest ("http://" <> n <> "/chain-only")
    return $ if status == 200
             then P.read (BSLC8.unpack body)
             else []
  (b, err) <- IOR.atomicModifyIORef' env $ \s ->
    let (s', be) = longestChain (ioBCState s) chains
     in (setBCState s s', be)
  if b then return b else do Log.infoM lBC err; return b

-- | Add a new node address to the list of nodes.
--   e.g., "192.168.0.5:5000"
registerNodeIO :: IOEnv -> Address -> IO ()
registerNodeIO env address =
  atomicModifyIORef_ env $ \s ->
    s { ioNodes = address:ioNodes s }

run :: [P.String] -> IO ()
run args = do
  Log.updateGlobalLogger lBC (Log.setLevel Log.DEBUG)
  let port = case args of
       ("-p":p:_) -> P.read p
       ("-h":_)   -> P.error "'-p', '--port', default=3001, 'port to listen on'"
       _          -> 3001
  env <- initializeIOEnv (show port)
  run' port env

run' :: Int -> IOEnv -> IO ()
run' httpPort env = do
  Log.infoM lBC ("starting httpServer on port " <> show httpPort)
  tn <- fmap ioThisNode (IOR.readIORef env)
  Wai.run httpPort $ Wai.logStdoutDev $
    \req s -> do
      CM.when debug $ Log.infoM lBC (tn <> " received request " <> show req)
      case Wai.rawPathInfo req of
        "/tx" -> -- POST
          case getQ req of
            Right tx -> do
              addTxToPoolIO env tx
              sendTxToPeers env tx
              send200 s tn ("/tx " <> show tx)
            Left x ->
              badQ s tn "/tx" x
        "/tx-no-forward" -> -- POST
          case getQ req of
            Right tx -> do
              addTxToPoolIO env tx
              send200 s tn ("/tx-no-forward " <> show tx)
            Left x ->
              badQ s tn "/tx-no-forward" x
        "/mine" -> do
          b <- fmap show (mineIO env)
          send200 s tn ("mine " <> b)
        "/longest-chain" -> do
          b <- longestChainIO env
          send200 s tn ("/longest-chain " <> show b)
        "/chain-only" -> do
          chain <- getBCState bcChain env
          send' s H.status200 (show chain)
        "/chain" -> do
          chain <- getBCState bcChain env
          send200 s tn ("chain " <> show (length chain) <> " " <> show chain)
        "/register" ->
          case getQ req of
            Right n -> do
              registerNodeIO env (BSC8.unpack n)
              send s tn H.status200 ("/register " <> show n)
            Left x ->
              badQ s tn "/register" x
        "/state" -> do
          st <- getBCState P.id env
          send200 s tn (show st)
        x ->
          send s tn H.status400 ("received unknown " <> BSC8.unpack x)
 where
  send200 s tn = send s tn H.status200
  send s tn sc r = send' s sc (tn <> " " <> r)
  send' s sc rsp = do
    CM.when debug $ Log.infoM lBC rsp
    s $ Wai.responseBuilder sc [] (BSB.byteString (BSC8.pack rsp))
  getQ r =
    case Wai.queryString r of ((q,_):_) -> Right q; x -> Left x
  badQ s tn msg q = do
    let rsp = tn <> " " <> msg <> " with bad query" <> show q
    CM.when debug $ Log.infoM lBC rsp
    send s tn H.status400 rsp

httpRequest :: P.String -> IO (Int, BSL.ByteString)
httpRequest url = do
  manager  <- H.newManager H.defaultManagerSettings
  request  <- H.parseRequest url
  response <- H.httpLbs request manager
  return ( H.statusCode (H.responseStatus response)
         , H.responseBody response )

sendTxToPeers :: IOEnv -> BS.ByteString -> IO ()
sendTxToPeers env tx = do
  nodes <- getIOState ioNodes env
  CM.forM_ nodes $ \n ->
    httpRequest ("http://" <> n <> "/tx-no-forward?" <> BSC8.unpack tx)

atomicModifyIORef_ :: IOR.IORef a -> (a -> a) -> IO ()
atomicModifyIORef_ i f =
  IOR.atomicModifyIORef' i (\a -> (f a, ()))

------------------------------------------------------------------------------

isValidTX :: BCState -> SignedTX -> Either Text ()
isValidTX s tx = case tx of
  (SignedTX (CreateCoin l u) _) -> do
    CM.when (ccInPool  u       (bcTXPool s)) $ Left ("CC already in pool: " <> l)
    CM.when (ccInChain u       (bcChain  s)) $ Left ("CC already in chain: "<> l)
    return ()
  (SignedTX (TransferCoin l stxHash _) _) -> do
    CM.when (tcInPool  stxHash (bcTXPool s)) $ Left ("TC already spent in pool: " <> l)
    CM.when (tcInChain stxHash (bcChain  s)) $ Left ("TC already spent in chain: "<> l)
    return ()
 where
  ccInPool  u = fst . searchPool  (ccTest u) "JUNK"
  ccInChain u = fst . searchChain (ccTest u) "JUNK"
  ccTest u x  = case decodeSTX x of
    (SignedTX (CreateCoin _ u') _) -> u == u' -- SAME UUID
    _                              -> False
  -- does pool have TransferCoin containing same STXHash as given stx?
  tcInPool stxHash  = fst . searchPool  (tcTest stxHash) "JUNK"
  -- does chain have STX whose STXHash field matches given STXHash?
  tcInChain stxHash = fst . searchChain (tcTest stxHash) "JUNK"
  tcTest stxHash x = case decodeSTX x of
    (SignedTX (TransferCoin _ stxHash' _) _) -> stxHash == stxHash' -- already spent
    _                                        -> False


testIsValidTX = do
  u                      <- runIO createUUID
  (_creatorPK, creatorSK)<- runIO generatePKSKIO
  (alicePK  , _aliceSK)  <- runIO generatePKSKIO
  (bobPK    , _bobSK)    <- runIO generatePKSKIO
  let Right cc        = createCoin   "cc"   u creatorSK
  let Right cToA      = transferCoin "cToA" cc  creatorSK alicePK
  let Right cToB      = transferCoin "cToB" cc  creatorSK bobPK
  let sCCInPool       = addTxToPool initialBCState (encodeSTX cc)
  let (sCCInChain, _) = mine sCCInPool
  let sTCInPool       = addTxToPool sCCInChain (encodeSTX cToA)
  let (sTCInChain, _) = mine sTCInPool
  describe "isValidTX" $do
    it "  valid : CC not in pool nor chain" $
      isValidTX initialBCState cc `shouldBe` Right ()
    it "invalid : CC already pool" $
      isValidTX sCCInPool      cc `shouldBe` Left "CC already in pool: cc"
    it "invalid : CC already chain" $
      isValidTX sCCInChain     cc `shouldBe` Left "CC already in chain: cc"
    --------------------------------------------------
    it "  valid : TC not in pool nor chain" $
      isValidTX initialBCState cToA `shouldBe` Right ()
    it "invalid : TC already pool" $
      isValidTX sTCInPool      cToA `shouldBe` Left "TC already spent in pool: cToA"
    it "invalid : TC already chain" $
      isValidTX sTCInChain     cToB `shouldBe` Left "TC already spent in chain: cToB"

isValidCoin
  :: BCState   -- TODO: this is BCState (instead of Chain) because of addToChain in tests
               --       could pass one more function to test to convert test view of chain
               --       to what is expected by the particular 'isValidCoin' impl
  -> PK        -- ^ creator public key
  -> SignedTX  -- ^ TX to verify
  -> Either Text ()
isValidCoin c = isValidCoinBase lookup
 where
  lookup :: STXHash -> Either Text SignedTX
  lookup stxHash =
    let (b, stx) = searchChain (\x -> stxHash == hashSignedTX (decodeSTX x)) "JUNK" (bcChain c)
     in if b then return (decodeSTX stx)
        else Left (isValidCoinErrMsg <> show stxHash)

-- for test
addToChain :: BCState -> [Transaction] -> BCState
addToChain = foldr (\tx bcs -> let (s,_) = mine (addTxToPool bcs tx) in s)

addToChainBCSpec :: BCState -> [SignedTX] -> BCState
addToChainBCSpec s = addToChain s . map encodeSTX

-- for test
emptyChainBCSpec = initialBCState

mkUTXO :: Chain -> M.Map STXHash SignedTX
mkUTXO c = go l m
 where
  l = map decodeSTX (concatMap bTXs c)
  m = foldr (\stx a -> M.insert (hashSignedTX stx) stx a) M.empty l
  go    []  m0 = m0
  go (x:xs) m0 = case x of
    (SignedTX CreateCoin{} _) -> go xs m0
    (SignedTX (TransferCoin _ stxHash _) _) -> go xs (M.delete stxHash m0)

testMkUTXO addToChainX emptyChain = do
  u                      <- runIO createUUID
  (_creatorPK, creatorSK) <- runIO generatePKSKIO
  (alicePK  , aliceSK)   <- runIO generatePKSKIO
  (bobPK    , bobSK)     <- runIO generatePKSKIO
  (jimPK    , _jimSK)    <- runIO generatePKSKIO
  let Right cc    = createCoin   "cc"    u     creatorSK
  let Right cToA  = transferCoin "cToA"  cc    creatorSK alicePK
  let Right aToB1 = transferCoin "aToB1" cToA  aliceSK   bobPK
  let Right _aToJ  = transferCoin "aToJ"  cToA  aliceSK   jimPK
  let Right bToA  = transferCoin "bToA"  aToB1 bobSK     alicePK
  let Right aToB2 = transferCoin "aToB2" bToA  aliceSK   bobPK
  let chain       = addToChainX emptyChain [cc, cToA, aToB1, bToA, aToB2]
  let chainBad    = addToChainX emptyChain     [cToA, aToB1, bToA, aToB2]
  describe "mkUTXO" $ do
    it "empty" $
      mkUTXO (bcChain emptyChain) `shouldBe` M.empty
    it "chain" $
      map getLabel (M.elems (mkUTXO (bcChain chain)))    `shouldBe` ["aToB2"]
    -- this shows that it doesn't check validity (i.e., coins "rooted" in CreateCoin)
    -- it just checks what has not been spent
    it "chainBad" $
      map getLabel (M.elems (mkUTXO (bcChain chainBad))) `shouldBe` ["aToB2"]
