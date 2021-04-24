{-# LANGUAGE UndecidableInstances #-}

-- | A high-level model for a cluster of Hydra nodes
module Hydra.Model where

import Cardano.Prelude
import Control.Monad.IOSim (runSim)
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

-- | An instance of a Hydra node
-- TODO: wrap actually `Hydra.Node.Node`
data Node = Node

data Model = Model {cluster :: Nodes}

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
initialiseModel = panic "not implemented"

confirmedLedgerUtxos :: Node -> [Utxo]
confirmedLedgerUtxos = panic "not implemented"
