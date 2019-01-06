{-# OPTIONS_GHC -fno-warn-orphans       #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}

{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
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
import qualified Prelude
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
testHandleAction _sender' _action =
  Prelude.undefined

testHandleEvent   :: NodeId -> Event StoreCmd          -> Scenario StoreCmd ()
testHandleEvent nodeId event = do
  (nodeConfig', sm, xState, persistentState) <- getNodeInfo nodeId
  let transitionEnv = TransitionEnv nodeConfig' sm xState
  let (newXState, newPersistentState, actions, logMsgs) =
        handleEvent xState transitionEnv persistentState event
  print " ----------------------------"
  print newXState
  print newPersistentState
  print actions
  print logMsgs
  -- testHandleActions nodeId newXState
  -- testHandleLogs Nothing (const $ pure ()) logMsgs
  return ()

----------------
-- Unit tests --
----------------

unit_follower_username_password_valid :: IO ()
unit_follower_username_password_valid = runScenario $
  testHandleEvent node1
  (MessageEvent
   (RPCMessageEvent (RPCMessage "client" (UsernamePasswordRPC (UsernamePassword "foo" "bar")))))

unit_follower_username_password_invalid :: IO ()
unit_follower_username_password_invalid = runScenario $
  testHandleEvent node1
  (MessageEvent
   (RPCMessageEvent (RPCMessage "client" (UsernamePasswordRPC (UsernamePassword "" "")))))

unit_follower_heartbeat_timeout :: IO ()
unit_follower_heartbeat_timeout = runScenario $
  testHandleEvent node1 (TimeoutEvent HeartbeatTimeout)
