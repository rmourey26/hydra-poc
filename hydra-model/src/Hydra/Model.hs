{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-deferred-type-errors #-}
{-# OPTIONS_GHC -Wno-deprecations #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

-- | A high-level model for a cluster of Hydra nodes
module Hydra.Model where

import Cardano.Prelude hiding (Async, async, atomically, threadDelay, throwIO)
import Control.Monad.Class.MonadAsync (Async, MonadAsync, async)
import Control.Monad.Class.MonadSTM (MonadSTM (atomically), TVar, modifyTVar, newTVar, readTVar, writeTVar)
import Control.Monad.Class.MonadThrow (MonadThrow, throwIO)
import Control.Monad.Class.MonadTimer (threadDelay)
import Control.Monad.IOSim (runSimOrThrow)
import Data.Default (def)
import Hydra.Ledger.MaryTest (MaryTest, noUTxO, mkLedgersEnv)
import Hydra.Logic (Event (OnChainEvent), OnChainTx (CollectComTx, InitTx))
import Hydra.Node (ClientSide (..), EventQueue (putEvent), HydraNetwork (..), Node (..), OnChain (..), createEventQueue)
import Hydra.Node.Run (emptyHydraHead, runNode)
import qualified Hydra.Node.Run as Run
import qualified Shelley.Spec.Ledger.API as Ledger
import qualified Shelley.Spec.Ledger.API as Shelley
import Hydra.Ledger (globals)

-- * Ledger Dependent Types

type Utxo = Shelley.UTxO MaryTest
type Transaction = Shelley.Tx MaryTest
type Ledger = Shelley.LedgerState MaryTest

makeLedger :: Utxo -> Ledger
makeLedger u = Ledger.LedgerState (Ledger.UTxOState u (Ledger.Coin 0) (Ledger.Coin 0) def) (Ledger.DPState def def)

-- |A single `Action` to run on a specific node
data Action = Action {targetNode :: NodeId, request :: Request}
  deriving (Eq, Show)

-- | An opaque identifier of a node to run a `Request` on
newtype NodeId = NodeId Natural
  deriving newtype (Eq, Show, Num)

-- |All possible requests a client can make to a `Node`
data Request
  = -- |Initialises a new head
    -- TODO: This is a simplification over the actual Hydra Head's dance of Init/Commit/CollectCom
    -- process.
    Init Utxo
  | -- |Submit a new transaction to the head at given slot
    NewTx Word64 Transaction
  | -- | Close the Head
    Close
  deriving (Eq, Show)

-- |A cluster of Hydra `Node`s that is managed by a given `Model`
newtype HydraNodes m = HydraNodes
  {nodes :: [HydraNode m]}

-- | An instance of a Hydra node
data HydraNode m = HydraNode {nodeId :: NodeId, runningNode :: RunningNode m}

data RunningNode m = RunningNode {node :: Node m Transaction, thread :: Async m ()}

-- |The `Model` which "drives" the nodes and maintains expected state.
data Model m = Model
  { -- |The nodes currently part of this `Model`
    cluster :: HydraNodes m
  , -- |The current expected consensus state of the ledger
    modelState :: TVar m ModelState
  }

selectNode :: NodeId -> HydraNodes m -> Maybe (HydraNode m)
selectNode target (HydraNodes nodes) = find ((== target) . nodeId) nodes

-- | The state of the system, including the expected `HeadState` and the nodes' state.
data ModelState = ModelState
  { nodeLedgers :: [Utxo]
  , currentState :: HeadState
  }
  deriving (Eq, Show)

data HeadState
  = Closed (Maybe Utxo)
  | Open Ledger
  | Failed Text
  deriving (Eq, Show)

expectedUtxo :: HeadState -> Utxo
expectedUtxo (Closed Nothing) = noUTxO
expectedUtxo (Closed (Just u)) = u
expectedUtxo (Open l) = ledgerUtxo l

ledgerUtxo :: Ledger -> Utxo
ledgerUtxo = Shelley._utxo . Shelley._utxoState

readTVarIO ::
  MonadSTM m => TVar m a -> m a
readTVarIO = atomically . readTVar

-- | Run a sequence of actions on a new `Model`
-- Returns the `Model` after it's been updated
runModel :: [Action] -> ModelState
runModel acts =
  runSimOrThrow
    ( do
        initial <- initialiseModel
        model <- foldM runAction initial acts
        threadDelay 3.14e7
        readTVarIO (modelState model)
    )

-- | Collect the UTXOs from all nodes
-- TODO: This is not the right way to do it probably
collectLedgers :: MonadSTM m => Model m -> m (Model m)
collectLedgers m@Model{modelState} = do
  l <- catMaybes <$> mapM (Run.getConfirmedLedger . node . runningNode) (nodes . cluster $ m)
  atomically $ modifyTVar modelState $ \ms -> ms{nodeLedgers = map ledgerUtxo l}
  pure m

-- | Run a single `Action` on the cluster of nodes
runAction ::
  MonadSTM m =>
  MonadThrow m =>
  Model m ->
  Action ->
  m (Model m)
runAction model@Model{cluster, modelState} action =
  (readTVarIO $ modelState)
    >>= \ms -> case (ms, action) of
      (ModelState [] (Closed Nothing), Action target (Init utxo)) ->
        selectNode target cluster & maybe (pure model) (init utxo model)
      (ModelState [] Open{}, Action target (NewTx slot tx)) ->
        selectNode target cluster & maybe (pure model) (newTx slot tx model)
      (ModelState [] Open{}, Action target Close) ->
        selectNode target cluster & maybe (pure model) (close model)

-- TODO: Flesh out errors from the execution
newtype ModelError = ModelError Text
  deriving (Eq, Show)

instance Exception ModelError

init ::
  MonadSTM m =>
  MonadThrow m =>
  Utxo ->
  Model m ->
  HydraNode m ->
  m (Model m)
init utxo m (runningNode -> RunningNode n _) = do
  atomically $ writeTVar (modelState m) $ ModelState [] (Open $ makeLedger utxo)
  Run.init n >>= \case
    Left e -> throwIO (ModelError $ "Failed to init ledger "  <> show e)
    Right () -> pure m

newTx ::
  MonadSTM m =>
  MonadThrow m =>
  Word64 ->
  Transaction ->
  Model m ->
  HydraNode m ->
  m (Model m)
newTx slot tx m (runningNode -> RunningNode n _) = do
  Run.newTx n tx >>= \case
    Left e -> throwIO (ModelError $ "Failed to submit new TX" <> show e)
    Right () -> do
      atomically $
        modifyTVar (modelState m) $
          \ms ->
            case currentState ms of
              Open l ->
                let
                  env = mkLedgersEnv slot
                  l' =
                    case Shelley.applyTxsTransition globals env (pure tx) l of
                      Right lg -> lg
                      Left e -> panic $ "tx " <> show tx <> " is guaranteed to be valid?: " <> show e
                in ms{currentState = Open l'}
              _ -> ms
      pure m



close ::
  MonadSTM m =>
  MonadThrow m =>
  Model m ->
  HydraNode m ->
  m (Model m)
close m@Model{modelState} (runningNode -> RunningNode n _) = do
  void $ collectLedgers m
  Run.close n
  atomically $
    modifyTVar modelState $ \ms ->
      let u = expectedUtxo (currentState ms)
       in ms{currentState = Closed (Just u)}
  pure m

initialiseModel ::
  MonadSTM m =>
  MonadAsync m =>
  MonadThrow m =>
  m (Model m)
initialiseModel = do
  st <- atomically $ newTVar (ModelState [] (Closed Nothing))
  q1 <- createEventQueue
  q2 <- createEventQueue
  onChainClient <- mockChainClient st [q1, q2]
  hydraNetwork <- mockHydraNetwork [q1, q2]
  node1 <- HydraNode 1 <$> runHydraNode q1 onChainClient hydraNetwork
  node2 <- HydraNode 2 <$> runHydraNode q2 onChainClient hydraNetwork
  pure $ Model (HydraNodes [node1, node2]) st

runHydraNode ::
  MonadAsync m =>
  MonadThrow m =>
  EventQueue m (Event Transaction) ->
  OnChain Transaction m ->
  HydraNetwork Transaction m ->
  m (RunningNode m)
runHydraNode eventQueue onChainClient hydraNetwork = do
  hydraHead <- emptyHydraHead
  clientSideRepl <- mockClientSideRepl
  let node = Node{..}
  async (runNode node) >>= \thread -> pure $ RunningNode node thread

mockClientSideRepl :: Applicative m => m (ClientSide m)
mockClientSideRepl =
  pure $ ClientSide $ const $ pure () -- ignore all client side instructions

mockHydraNetwork ::
  Applicative m =>
  [EventQueue m e] ->
  m (HydraNetwork Transaction m)
mockHydraNetwork _ =
  pure $
    HydraNetwork
      { broadcast = const $ pure () -- just drop all messages
      }

mockChainClient ::
  MonadSTM m =>
  TVar m ModelState ->
  -- we need to propagate chain-to-head transactions to all nodes, this is
  -- emulated by poasting the events in each node's queue
  [EventQueue m (Event Transaction)] ->
  m (OnChain Transaction m)
mockChainClient varm qs =
  pure $
    OnChain $ \case
      InitTx ->
        atomically (expectedUtxo . currentState <$> readTVar varm)
          >>= \utxos -> do
            mapM_ (`putEvent` OnChainEvent InitTx) qs
            mapM_ (`putEvent` OnChainEvent (CollectComTx utxos)) qs
      _ -> pure ()
