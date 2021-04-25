{-# LANGUAGE UndecidableInstances #-}

-- | A high-level model for a cluster of Hydra nodes
module Hydra.Model where

import Cardano.Prelude
import Control.Monad.IOSim (runSim)
import Hydra.Ledger.MaryTest (MaryTest, initUTxO, noUTxO)
import qualified Shelley.Spec.Ledger.API as Shelley

-- * Ledger Dependent Types

type Utxo = Shelley.UTxO MaryTest
type Transaction = Shelley.Tx MaryTest

-- |A single `Action` to run on a specific node
data Action = Action {nodeId :: NodeId, request :: Request}
  deriving (Eq, Show)

-- | An opaque identifier of a node to run a `Request` on
newtype NodeId = NodeId Natural
  deriving (Eq, Show)

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
data Node = Node
  deriving (Eq, Show)

-- |The `Model` which "drives" the nodes and maintains expected state.
data Model = Model
  { -- |The nodes currently part of this `Model`
    cluster :: Nodes
  , -- |The current expected consensus state of the ledger
    ledger :: Utxo
  }
  deriving (Eq, Show)

-- | Run a sequence of actions on a new `Model`
-- Returns the `Model` after it's been updated
runModel :: [Action] -> Model
runModel acts =
  let model = initialiseModel
   in case runSim (foldM runAction model acts) of
        Left _ -> panic "Not implemented"
        Right m -> m

runAction :: Model -> Action -> m Model
runAction = panic "not implemented"

initialiseModel :: Model
initialiseModel = Model (Nodes [Node, Node]) initUTxO

confirmedLedgerUtxos :: Node -> Utxo
confirmedLedgerUtxos _ = noUTxO
