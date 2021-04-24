{-# LANGUAGE UndecidableInstances #-}

-- | A high-level model for a cluster of Hydra nodes
module Hydra.Model where

import Cardano.Prelude
import Hydra.Ledger.MaryTest (MaryTest)
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
  = -- |Initialises a new head with a list of UTXOs
    -- TODO: This is a simplification over the actual Hydra Head's dance of Init/Commit/CollectCom
    -- process.
    Init [Utxo]
  | -- |Submit a new transaction to the head
    NewTx Transaction
  deriving (Eq, Show)

-- |A list of `Node`s that are managed by a given `Model`
newtype Nodes = Nodes [Node]

-- | An instance of a Hydra node
-- TODO: wrap actually `Hydra.Node.Node`
data Node = Node

-- |A model's options
data Options = Options {numberOfNodes :: Natural}
  deriving (Eq, Show)

-- | Default model Options
-- Runs with:
-- * 1 node
defaultOptions :: Options
defaultOptions = Options 1

runModel :: Options -> [Action] -> Nodes
runModel = panic "not implemented"

confirmedLedgerUtxos :: Node -> [Utxo]
confirmedLedgerUtxos = panic "not implemented"
