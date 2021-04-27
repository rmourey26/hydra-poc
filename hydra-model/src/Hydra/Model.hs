{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE UndecidableInstances #-}

-- | A high-level model for a cluster of Hydra nodes
module Hydra.Model where

import Cardano.Prelude
import Control.Monad.IOSim (runSim)
import Hydra.Ledger.MaryTest (MaryTest, noUTxO)
import qualified Shelley.Spec.Ledger.API as Shelley

-- * Ledger Dependent Types

type Utxo = Shelley.UTxO MaryTest
type Transaction = Shelley.Tx MaryTest
type Ledger = Shelley.LedgerState MaryTest

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
    -- TODO: The Utxo set is hardcoded in MaryTest module
    Init
  | -- |Submit a new transaction to the head
    NewTx Transaction
  deriving (Eq, Show)

-- |A cluster of Hydra `Node`s that is managed by a given `Model`
newtype Nodes = Nodes [Node]
  deriving (Eq, Show)

-- | An instance of a Hydra node
-- TODO: wrap actual `Hydra.Node.Node`
data Node = Node {nodeId :: NodeId}
  deriving (Eq, Show)

-- |The `Model` which "drives" the nodes and maintains expected state.
data Model = Model
  { -- |The nodes currently part of this `Model`
    cluster :: Nodes
  , -- |The current expected consensus state of the ledger
    modelState :: ModelState
  }
  deriving (Eq, Show)

data ModelState
  = Closed
  | Open Ledger
  deriving (Eq, Show)

ledger :: Model -> Utxo
ledger Model{modelState = Closed} = noUTxO
ledger Model{modelState = Open l} = Shelley._utxo . Shelley._utxoState $ l

-- | Run a sequence of actions on a new `Model`
-- Returns the `Model` after it's been updated
runModel :: [Action] -> Model
runModel acts =
  case runSim (initialiseModel >>= \model -> foldM runAction model acts) of
    Left _ -> panic "Not implemented"
    Right m -> m

runAction :: Monad m => Model -> Action -> m Model
runAction model@Model{cluster = Nodes nodes, modelState = Closed} (Action target Init) =
  case find ((== target) . nodeId) nodes of
    Nothing -> pure model
    Just node -> init node
runAction _ _ = panic "not implemented"

init :: Node -> m Model
init = panic "not implemented"

initialiseModel :: Monad m => m Model
initialiseModel = do
  node1 <- runNode 1
  node2 <- runNode 2
  pure $ Model (Nodes [node1, node2]) Closed

runNode :: NodeId -> m Node
runNode _ = panic "not implemented"

confirmedLedgerUtxos :: Node -> Utxo
confirmedLedgerUtxos _ = noUTxO
