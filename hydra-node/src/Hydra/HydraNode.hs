-- | Build and run a fully working `HydraNode` using a Mary ledger
-- TODO: The types should really be in `Hydra.Node`
module Hydra.HydraNode where

import Cardano.Prelude
import Control.Exception.Safe (MonadThrow)
import Hydra.Ledger (cardanoLedger)
import Hydra.Ledger.MaryTest (MaryTest)
import qualified Hydra.Ledger.MaryTest as MaryTest
import Hydra.Logic (HeadParameters (..), SnapshotStrategy (..), createHeadState)
import Hydra.Node (
  EventQueue (nextEvent),
  HydraHead,
  HydraNode (..),
  createChainClient,
  createClientSideRepl,
  createEventQueue,
  createHydraHead,
  createHydraNetwork,
  handleNextEvent,
 )
import qualified Hydra.Node as Node
import Shelley.Spec.Ledger.API (Tx)
import Shelley.Spec.Ledger.LedgerState ()

type MaryHydraNode m = HydraNode m (Tx MaryTest)

init ::
  MonadThrow m =>
  MaryHydraNode m ->
  m (Either Text ())
init HydraNode{onChainClient, hydraHead, clientSideRepl} =
  bimap show (const ()) <$> Node.init onChainClient hydraHead clientSideRepl

createNode :: IO (MaryHydraNode IO)
createNode = do
  eq <- createEventQueue
  hh <- emptyHydraHead
  oc <- createChainClient eq
  hn <- createHydraNetwork eq
  cs <- createClientSideRepl oc hh hn loadTx
  pure $ HydraNode eq hn oc cs hh
 where
  loadTx fp = panic $ "should load and decode a tx from " <> show fp

emptyHydraHead :: IO (HydraHead (Tx MaryTest) IO)
emptyHydraHead = createHydraHead headState ledger
 where
  headState = createHeadState [] HeadParameters SnapshotStrategy
  ledger = cardanoLedger defaultEnv
  defaultEnv = MaryTest.mkLedgersEnv

runNode :: MaryHydraNode IO -> IO ()
runNode hydraNode@HydraNode{eventQueue} =
  forever $ nextEvent eventQueue >>= handleNextEvent hydraNode
