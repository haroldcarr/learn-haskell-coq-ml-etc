{-# OPTIONS_GHC -fno-warn-orphans       #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}

{-# LANGUAGE DeriveGeneric         #-}
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
import qualified Data.Map        as Map
import qualified Data.Set        as Set
import           Numeric.Natural
import           Protolude

type Var = ByteString

data StoreCmd
  = Set  Var Natural
  | Incr Var
  deriving (Show, Generic)

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

testHandleLogs :: Maybe [NodeId] -> (Text -> IO ()) -> [LogMsg] -> Scenario v ()
testHandleLogs nIdsM f logs = liftIO $
  case nIdsM of
    Nothing ->
      mapM_ (f . logMsgToText) logs
    Just nIds ->
      mapM_ (f . logMsgToText) $ flip filter logs $ \log' ->
        lmdNodeId (lmData log') `elem` nIds

testHandleActions :: NodeId -> [Action Store StoreCmd] -> Scenario StoreCmd ()
testHandleActions sender' =
  mapM_ (testHandleAction sender')

testHandleAction  :: NodeId ->  Action Store StoreCmd  -> Scenario StoreCmd ()
testHandleAction sender' action =
  case action of
    SendRPC _ _ ->
      panic "should not happen"
    SendToClient cid resp -> do
      print (cid, resp)
      case resp of
        -- CresStateMachine s
        CresEnterUsernamePassword ->
          testHandleEvent sender'
          (MessageEvent (ClientRequestEvent (CreqUsernamePassword cid (UsernamePassword "foo" "bar"))))
        CresInvalidUserNamePassword ->
          testHandleEvent sender'
          (MessageEvent (ClientRequestEvent (CreqUsernamePassword cid (UsernamePassword "foo" "bar"))))
        CresEnterPin ->
          testHandleEvent sender'
          (MessageEvent (ClientRequestEvent (CreqPin cid (Pin "1234"))))
        CresInvalidPin ->
          testHandleEvent sender'
          (MessageEvent (ClientRequestEvent (CreqPin cid (Pin "1234"))))
        CresEnterAcctNumOrQuit _t ->
          testHandleEvent sender'
          (MessageEvent (ClientRequestEvent (CreqAcctNumOrQuit cid (AccNumOrQuit "Q"))))
        CresInvalidAcctNum ->
          testHandleEvent sender'
          (MessageEvent (ClientRequestEvent (CreqAcctNumOrQuit cid (AccNumOrQuit "Q"))))
        CresQuit ->
          pure ()
        _ -> panic "fall through"
    ResetTimeoutTimer _ -> pure ()

testHandleEvent   :: NodeId -> Event StoreCmd          -> Scenario StoreCmd ()
testHandleEvent nodeId event = do
  (nodeConfig', sm, xState, persistentState) <- getNodeInfo nodeId
  let transitionEnv = TransitionEnv nodeConfig' sm xState
  let (newXState, newPersistentState, actions, logMsgs) =
        handleEvent xState transitionEnv persistentState event
  updatePersistentState nodeId newPersistentState
  updateXNodeState      nodeId newXState
  testHandleActions     nodeId actions
  testHandleLogs (Just [nodeId]) print logMsgs
  -- applyLogEntries   nodeId sm
  return ()

----------------
-- Unit tests --
----------------

unit_logged_out_username_password_valid :: IO ()
unit_logged_out_username_password_valid = runScenario $
  testHandleEvent node1
  (MessageEvent
   (ClientRequestEvent (CreqUsernamePassword (ClientId "client") (UsernamePassword "foo" "bar"))))
