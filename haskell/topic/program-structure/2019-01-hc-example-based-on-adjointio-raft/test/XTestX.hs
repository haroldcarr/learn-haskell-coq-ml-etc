{-# OPTIONS_GHC -fno-warn-orphans       #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}

{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TupleSections         #-}
{-# LANGUAGE TypeFamilies          #-}

module XTestX where

import           X0
import           XTestUtils
------------------------------------------------------------------------------
import qualified Data.Map         as Map
import qualified Data.Set         as Set
import           Numeric.Natural
import qualified Prelude
import           Protolude
import qualified Test.Tasty.HUnit as HUnit

type Var = ByteString

data StoreCmd
  = Set  Var Natural
  | Incr Var
  deriving (Eq, Show)

type Store = Map Var Natural

instance RSMP Store StoreCmd where
  data RSMPError Store StoreCmd = StoreError Text deriving (Show)
  type RSMPCtx   Store StoreCmd = ()
  applyCmdRSMP _ store cmd =
    Right $ case cmd of
      Set x n -> Map.insert x    n store
      Incr x  -> Map.adjust succ x store

testVar :: Var
testVar = "test"

testInitVal :: Natural
testInitVal = 1

testSetCmd :: StoreCmd
testSetCmd = Set testVar testInitVal

testIncrCmd :: StoreCmd
testIncrCmd = Incr testVar

data TestState v = TestState
  { testNodeIds              :: NodeIds
  , testNodeLogs             :: Map NodeId (Entries StoreCmd)
  , testNodeSMs              :: Map NodeId Store
  , testNodeXStates          :: Map NodeId (XNodeState v)
  , testNodePersistentStates :: Map NodeId PersistentState
  , testNodeConfigs          :: Map NodeId NodeConfig
  } deriving Show

type Scenario v a = StateT (TestState v) IO a

runScenario :: Scenario v () -> IO ()
runScenario scenario =
  evalStateT scenario $ TestState
    { testNodeIds              = nodeIds
    , testNodeLogs             = Map.fromList $ (, mempty)              <$> Set.toList nodeIds
    , testNodeSMs              = Map.fromList $ (, mempty)              <$> Set.toList nodeIds
    , testNodeXStates          = Map.fromList $ (, initXNodeState)      <$> Set.toList nodeIds
    , testNodePersistentStates = Map.fromList $ (, initPersistentState) <$> Set.toList nodeIds
    , testNodeConfigs          = Map.fromList $ zip (Set.toList nodeIds) testConfigs
    }

updateStateMachine :: NodeId -> Store -> Scenario v ()
updateStateMachine nodeId sm =
  modify $ \testState@TestState{..} ->
    testState { testNodeSMs = Map.insert nodeId sm testNodeSMs }

updatePersistentState :: NodeId -> PersistentState -> Scenario v ()
updatePersistentState nodeId persistentState =
  modify $ \testState@TestState{..} ->
    testState { testNodePersistentStates = Map.insert nodeId persistentState testNodePersistentStates }

updateXNodeState :: NodeId -> XNodeState v -> Scenario v ()
updateXNodeState nodeId xState =
  modify $ \testState@TestState{..} ->
    testState { testNodeXStates = Map.insert nodeId xState testNodeXStates }

getNodeInfo :: NodeId -> Scenario v (NodeConfig, Store, XNodeState v, PersistentState)
getNodeInfo nId = do
  nodeConfigs          <- gets testNodeConfigs
  nodeSMs              <- gets testNodeSMs
  nodeXStates          <- gets testNodeXStates
  nodePersistentStates <- gets testNodePersistentStates
  let Just nodeInfo =
        Map.lookup nId nodeConfigs          >>= \config ->
        Map.lookup nId nodeSMs              >>= \store ->
        Map.lookup nId nodeXStates          >>= \xState ->
        Map.lookup nId nodePersistentStates >>= \persistentState ->
        pure (config, store, xState, persistentState)
  pure nodeInfo

-------------------------------
-- Handle actions and events --
-------------------------------

testHandleEvent
  :: NodeId
  -> Event StoreCmd
  -> Scenario StoreCmd ([Action Store StoreCmd], [LogMsg])
testHandleEvent nodeId event = do
  (nodeConfig', sm, xState, persistentState) <- getNodeInfo nodeId
  let transitionEnv = TransitionEnv nodeConfig' sm xState
  let (newXState, newPersistentState, actions, logMsgs) =
        handleEvent xState transitionEnv persistentState event
  updatePersistentState nodeId newPersistentState
  updateXNodeState      nodeId newXState
  return (actions, logMsgs)

----------------
-- Unit tests --
----------------

unit_full_cycle :: IO ()
unit_full_cycle = runScenario $ do
  r1 <- testHandleEvent node1 (TimeoutEvent HeartbeatTimeout)
  liftIO $ assertActions r1
             [ ResetTimeoutTimer HeartbeatTimeout
             , SendToClient (ClientId "client") CresEnterUsernamePassword
             ]
  liftIO $ assertLogs r1
             ["LoggedOut.handleTimeout; HeartbeatTimeout"]
  -------------------------
  r2 <- testHandleEvent node1
          (MessageEvent
            (ClientRequestEvent (CreqUsernamePassword
                                  (ClientId "client")
                                  (UsernamePassword "foo" "bar"))))
  liftIO $ assertActions r2
             [ ResetTimeoutTimer HeartbeatTimeout
             , SendToClient (ClientId "client") CresEnterPin
             ]
  liftIO $ assertLogs r2
             ["LoggedOut.handleUsernamePassword valid; ClientId \"client\" UsernamePassword {upUsername = \"foo\", upPassword = \"bar\"}"]
  -------------------------
  r3 <- testHandleEvent node1
          (MessageEvent
            (ClientRequestEvent (CreqPin
                                  (ClientId "client")
                                  (Pin "1234"))))
  liftIO $ assertActions r3
             [ ResetTimeoutTimer HeartbeatTimeout
             , SendToClient (ClientId "client") (CresEnterAcctNumOrQuit "1,2,3")
             ]
  liftIO $ assertLogs r3
             ["Candidate.handlePin: valid; ClientId \"client\" Pin {pPin = \"1234\"}"]
  -------------------------
  r4 <- testHandleEvent node1
          (MessageEvent
            (ClientRequestEvent (CreqAcctNumOrQuit
                                  (ClientId "client")
                                  (AccNumOrQuit "1"))))
  liftIO $ assertActions r4
             [ ResetTimeoutTimer HeartbeatTimeout
               -- , SendToClient c (CresAcctBalance _)
             , SendToClient (ClientId "client") (CresEnterAcctNumOrQuit "1,2,3")
             ]
  liftIO $ assertLogs r4
             ["LoggedIn.handleAcctNumOrQuit; ClientId \"client\" AccNumOrQuit {anoqAcctNumOrQuit = \"1\"}"]
  -------------------------
  r5 <- testHandleEvent node1
          (MessageEvent
            (ClientRequestEvent (CreqAcctNumOrQuit
                                  (ClientId "client")
                                  (AccNumOrQuit "Q"))))
  liftIO $ assertActions r5
             [ ResetTimeoutTimer HeartbeatTimeout
             , SendToClient (ClientId "client") CresQuit
             ]
  liftIO $ assertLogs r5
             ["LoggedIn.handleAcctNumOrQuit; ClientId \"client\" AccNumOrQuit {anoqAcctNumOrQuit = \"Q\"}"]

------------------
-- Assert utils --
------------------

assertActions :: ([Action Store StoreCmd], [LogMsg]) -> [Action Store StoreCmd] -> IO ()
assertActions (got,_) expected = do
  assertEqualLength "actions" expected got
  mapM_ (uncurry (HUnit.assertEqual "unexpected Action"))
        (zip expected got)

assertLogs :: ([Action Store StoreCmd], [LogMsg]) -> [Text] -> IO ()
assertLogs (_,got) expected = do
  assertEqualLength "logs" expected got
  mapM_ (\(e,g) -> HUnit.assertEqual "unexpected log msg" e (lmdMsg (lmData g)))
        (zip expected got)

assertEqualLength :: (Show a, Show b) => Text -> [a] -> [b] -> IO ()
assertEqualLength msg expected got =
  HUnit.assertEqual
    ( toS msg <> " : not the same length : "
      <> "expected: " <> toS (Prelude.show expected) <> " "
      <> "got: "      <> toS (Prelude.show got) )
    (length expected)
    (length got)
