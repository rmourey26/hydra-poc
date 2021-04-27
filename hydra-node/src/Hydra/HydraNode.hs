-- | Build and run a fully working `HydraNode` using a Mary ledger
-- TODO: The types should really be in `Hydra.Node`
module Hydra.HydraNode where

import Cardano.Prelude
import Control.Exception.Safe (MonadThrow)
import Hydra.Ledger (cardanoLedger)
import Hydra.Ledger.MaryTest (MaryTest)
import qualified Hydra.Ledger.MaryTest as MaryTest
import Hydra.Logic (Event, HeadParameters (..), SnapshotStrategy (..), createHeadState)
import Hydra.Node (
  ClientSide (..),
  EventQueue (nextEvent),
  HydraHead,
  HydraNetwork,
  OnChain,
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

data HydraNode m tx = HydraNode
  { eventQueue :: EventQueue m (Event tx)
  , hydraNetwork :: HydraNetwork m
  , onChainClient :: OnChain m
  , clientSideRepl :: ClientSide m
  , hydraHead :: HydraHead tx m
  }

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
  hh <- createHydraHead headState ledger
  oc <- createChainClient eq
  hn <- createHydraNetwork eq
  cs <- createClientSideRepl oc hh hn loadTx
  pure $ HydraNode eq hn oc cs hh
 where
  headState = createHeadState [] HeadParameters SnapshotStrategy
  ledger = cardanoLedger defaultEnv
  defaultEnv = MaryTest.mkLedgersEnv

  loadTx fp = panic $ "should load and decode a tx from " <> show fp

runNode :: MaryHydraNode IO -> IO ()
runNode HydraNode{eventQueue, hydraNetwork, onChainClient, clientSideRepl, hydraHead} =
  forever $ nextEvent eventQueue >>= handleNextEvent hydraNetwork onChainClient clientSideRepl hydraHead
