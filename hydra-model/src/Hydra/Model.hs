{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-deferred-type-errors #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

-- | A high-level model for a cluster of Hydra nodes
module Hydra.Model where

import Cardano.Prelude hiding (Async, throwIO, withAsync)
import Control.Monad.Class.MonadAsync (Async, MonadAsync, withAsync)
import Control.Monad.Class.MonadThrow (MonadThrow, throwIO)
import Control.Monad.IOSim (runSim)
import Data.Default (def)
import Hydra.Ledger.MaryTest (MaryTest, noUTxO)
import Hydra.Node (ClientSide, EventQueue, HydraNetwork, Node (..), OnChain, createEventQueue)
import Hydra.Node.Run (emptyHydraHead, runNode)
import qualified Hydra.Node.Run as Run
import Shelley.Spec.Ledger.API (Coin (..), DPState (..), LedgerState (LedgerState), UTxOState (..))
import qualified Shelley.Spec.Ledger.API as Shelley

-- * Ledger Dependent Types

type Utxo = Shelley.UTxO MaryTest
type Transaction = Shelley.Tx MaryTest
type Ledger = Shelley.LedgerState MaryTest

-- |Initialises a `Ledger` with given `Utxo`
-- The reste of the state is set to `def`ault values.
mkLedger :: Utxo -> Ledger
mkLedger utxos = LedgerState (UTxOState utxos (Coin 0) (Coin 0) def) (DPState def def)

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
  | -- |Submit a new transaction to the head
    NewTx Transaction
  | -- | Close the Head
    Close
  deriving (Eq, Show)

-- |A cluster of Hydra `Node`s that is managed by a given `Model`
newtype HydraNodes m = HydraNodes [HydraNode m]

-- | An instance of a Hydra node
-- TODO: wrap actual `Hydra.Node.Node`
data HydraNode m = HydraNode {nodeId :: NodeId, runningNode :: RunningNode m}

data RunningNode m = RunningNode {node :: Node m Transaction, thread :: Async m ()}

-- |The `Model` which "drives" the nodes and maintains expected state.
data Model m = Model
  { -- |The nodes currently part of this `Model`
    cluster :: HydraNodes m
  , -- |The current expected consensus state of the ledger
    modelState :: ModelState
  }

-- | The state of the system, including the expected `HeadState` and the nodes' state.
data ModelState = ModelState
  { nodeLedgers :: [Utxo]
  , currentState :: HeadState
  }
  deriving (Eq, Show)

data HeadState
  = Closed
  | Open Ledger
  | Failed Text
  deriving (Eq, Show)

expectedUtxo :: HeadState -> Utxo
expectedUtxo Closed = noUTxO
expectedUtxo (Open l) = ledgerUtxo l

ledgerUtxo :: Ledger -> Utxo
ledgerUtxo = Shelley._utxo . Shelley._utxoState

-- | Run a sequence of actions on a new `Model`
-- Returns the `Model` after it's been updated
runModel :: [Action] -> ModelState
runModel acts =
  case runSim
    ( do
        initial <- initialiseModel
        modelState <$> foldM runAction initial acts
    ) of
    Left f -> ModelState [] (Failed $ show f)
    Right m -> m

-- | Run a single `Action` on the cluster of nodes
runAction ::
  MonadThrow m =>
  Model m ->
  Action ->
  m (Model m)
runAction model@Model{cluster = HydraNodes nodes, modelState = ModelState [] Closed} (Action target (Init utxo)) =
  case find ((== target) . nodeId) nodes of
    Nothing -> pure model
    Just node -> init utxo node model
runAction _ _ = panic "not implemented"

-- TODO: Flesh out errors from the execution
newtype ModelError = ModelError Text
  deriving (Eq, Show)

instance Exception ModelError

init ::
  MonadThrow m =>
  Utxo ->
  HydraNode m ->
  Model m ->
  m (Model m)
init utxo (runningNode -> RunningNode n _) m = do
  Run.init n >>= \case
    Left e -> throwIO (ModelError $ show e)
    Right () -> pure m{modelState = ModelState [] $ Open (mkLedger utxo)}

initialiseModel ::
  MonadAsync m =>
  MonadThrow m =>
  m (Model m)
initialiseModel = do
  node1 <- HydraNode 1 <$> runHydraNode
  node2 <- HydraNode 2 <$> runHydraNode
  pure $ Model (HydraNodes [node1, node2]) (ModelState [] Closed)

runHydraNode ::
  MonadAsync m =>
  MonadThrow m =>
  m (RunningNode m)
runHydraNode = do
  eventQueue <- createEventQueue
  hydraHead <- emptyHydraHead
  onChainClient <- mockChainClient eventQueue
  hydraNetwork <- mockHydraNetwork eventQueue
  clientSideRepl <- mockClientSideRepl
  let node = Node{..}
  withAsync (runNode node) $
    \thread -> pure $ RunningNode node thread

mockClientSideRepl :: m (ClientSide m)
mockClientSideRepl = panic "not implemented"

mockHydraNetwork :: EventQueue m e -> m (HydraNetwork m)
mockHydraNetwork = panic "not implemented"

mockChainClient :: EventQueue m e -> m (OnChain m)
mockChainClient = panic "not implemented"
