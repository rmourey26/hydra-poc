{-# LANGUAGE UndecidableInstances #-}

-- | A high-level model for a cluster of Hydra nodes
module Hydra.Model where

import Cardano.Prelude
import Hydra.Ledger (Utxo)

-- |A single `Action` to run on a specific node
-- `Action` is parameterized by the underlying `tx `type of ledger
data Action tx = Action {nodeId :: NodeId, request :: Request tx}

deriving instance (Show tx, Show (Utxo tx)) => Show (Action tx)
deriving instance (Eq tx, Eq (Utxo tx)) => Eq (Action tx)

-- | An opaque identifier of a node to run a `Request` on
newtype NodeId = NodeId Natural
  deriving (Eq, Show)

-- |All possible requests a client can make to a `Node`
-- `Request` is parameterized by the underlying `tx `type of ledger
data Request tx
  = -- |Initialises a new head with a list of UTXOs
    -- TODO: This is a simplification over the actual Hydra Head's dance of Init/Commit/CollectCom
    -- process.
    Init [Utxo tx]
  | -- |Submit a new transaction to the head
    NewTx tx

deriving instance (Show tx, Show (Utxo tx)) => Show (Request tx)
deriving instance (Eq tx, Eq (Utxo tx)) => Eq (Request tx)

-- |A list of `Node`s that are managed by a given `Model`
newtype Nodes tx = Nodes [Node tx]

-- | An instance of a Hydra node
data Node tx = Node

-- |A model's options
data Options = Options {numberOfNodes :: Natural}
  deriving (Eq, Show)

-- | Default model Options
-- Runs with:
-- * 1 node
defaultOptions :: Options
defaultOptions = Options 1

runModel :: Options -> [Action tx] -> Nodes tx
runModel = panic "not implemented"

confirmedLedgerUtxos :: Node tx -> [Utxo tx]
confirmedLedgerUtxos = panic "not implemented"
