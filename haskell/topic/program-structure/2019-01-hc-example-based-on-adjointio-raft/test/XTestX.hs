{-# OPTIONS_GHC -fno-warn-orphans       #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}

{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedLists       #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TupleSections         #-}
{-# LANGUAGE TypeFamilies          #-}

module XTestX where

import           X0
import           XTestUtils
import           XUtil
------------------------------------------------------------------------------
import qualified Data.Map         as Map
import qualified Data.Set         as Set
import           Numeric.Natural
import           Protolude
import qualified Test.Tasty.HUnit as HUnit

type Var = ByteString

data StoreCmd
  = Set  Var Natural
  | Incr Var
  | Noop
  deriving (Eq, Show)

type Store = Map Var Natural

instance XSMP Store StoreCmd where
  data XSMPError Store StoreCmd = StoreError Text deriving (Show)
  type XSMPCtx   Store StoreCmd = ()
  applyCmdXSMP _ store cmd =
    Right $ case cmd of
      Set  x n -> Map.insert x    n store
      Incr x   -> Map.adjust succ x store
      Noop     -> store

data TestState v = TestState
  { testNodeIds              :: NodeIds
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
  (nodeConfig', sm, inXNodeState, inPersistentState) <- getNodeInfo nodeId
  let transitionEnv = TransitionEnv nodeConfig' sm inXNodeState
  let (outXNodeState, outPersistentState, actions, logMsgs) =
        handleEvent inXNodeState transitionEnv inPersistentState event
  updatePersistentState nodeId outPersistentState
  updateXNodeState      nodeId outXNodeState
  xns <- applyCmds nodeId sm
  updateXNodeState      nodeId xns
  return (actions, logMsgs)
 where
  applyCmds :: NodeId -> Store -> Scenario StoreCmd (XNodeState StoreCmd)
  applyCmds nid inSm  = do
    (_, _, xns@(XNodeState nodeState), _) <- getNodeInfo nid
    case getCmd nodeState of
      Nothing -> return xns
      Just v  ->
        case applyCmdXSMP () inSm v of
          Left      _ -> return xns
          Right outSm -> do
            updateStateMachine nid outSm
            return (XNodeState (setCmd Nothing nodeState))

  getCmd :: NodeState ns v -> Maybe v
  getCmd = \case
    NodeLoggedInState s -> lsCmd s
    _                   -> Nothing

  setCmd :: Maybe v -> NodeState ns v -> NodeState ns v
  setCmd mv = \case
    NodeLoggedInState s -> NodeLoggedInState (s { lsCmd = mv })
    s                   -> s

testHeartbeat :: Text -> Scenario StoreCmd ()
testHeartbeat mode = do
  r1 <- testHandleEvent node1 (TimeoutEvent HeartbeatTimeout)
  liftIO $ assertActions r1
             [ ResetTimeoutTimer HeartbeatTimeout
             , SendToClient (ClientId "client") CresEnterUsernamePassword
             ]
  liftIO $ assertLogs r1
             [mode <> ".handleTimeout; HeartbeatTimeout"]

testUsernamePassword :: ClientId -> UsernamePassword -> Scenario StoreCmd ()
testUsernamePassword c up = do
  r2 <- testHandleEvent node1
          (MessageEvent (ClientRequestEvent (CreqUsernamePassword c up)))
  liftIO $ assertActions r2
             [ ResetTimeoutTimer HeartbeatTimeout
             , SendToClient c CresEnterPin
             ]
  liftIO $ assertLogs r2
             [fields ["LoggedOut.handleUsernamePassword valid", pshow c, pshow up]]

testPin :: ClientId -> Pin -> Scenario StoreCmd ()
testPin c p = do
  r3 <- testHandleEvent node1
          (MessageEvent (ClientRequestEvent (CreqPin c p)))
  liftIO $ assertActions r3
             [ ResetTimeoutTimer HeartbeatTimeout
             , SendToClient c CresEnterCommandOrQuit
             ]
  liftIO $ assertLogs r3
             [fields ["Candidate.handlePin: valid", pshow c, pshow p]]

testCommandOrQuit :: ClientId -> CommandOrQuit StoreCmd -> Store -> Scenario StoreCmd ()
testCommandOrQuit c a@(CommandOrQuit t _) store = do
  r4 <- testHandleEvent node1 (MessageEvent (ClientRequestEvent (CreqCommandOrQuit c a)))
  liftIO $ assertActions r4 $
             [ResetTimeoutTimer HeartbeatTimeout]
             ++ [SendToClient c (CresStateMachine store) | t == "R"]
             ++ [ SendToClient c $ if t == "Q"
                                     then CresQuit
                                     else CresEnterCommandOrQuit ]
  liftIO $ assertLogs r4
             [fields ["LoggedIn.handleCommandOrQuit", pshow c, pshow a]]

----------------
-- Unit tests --
----------------

unit_full_cycle :: IO ()
unit_full_cycle = runScenario $ do
  testHeartbeat "LoggedOut"
  testUsernamePassword (ClientId "client") (UsernamePassword "foo" "bar")
  testPin              (ClientId "client") (Pin "1234")
  testCommandOrQuit    (ClientId "client") (CommandOrQuit "R" Noop)         []
  testCommandOrQuit    (ClientId "client") (CommandOrQuit "W" (Set "x" 3))  []
  testCommandOrQuit    (ClientId "client") (CommandOrQuit "R" Noop)         [("x",3)]
  testCommandOrQuit    (ClientId "client") (CommandOrQuit "Q" Noop)         []

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
      <> "expected: " <> pshow expected <> " "
      <> "got: "      <> pshow got )
    (length expected)
    (length got)
