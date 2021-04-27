-- | Build and run a fully working `HydraNode` using a Mary ledger
-- TODO: The types should really be in `Hydra.Node`
module Hydra.Node.Run where

import Cardano.Prelude
import Control.Monad.Class.MonadSTM (MonadSTM)
import Control.Monad.Class.MonadThrow (MonadThrow)
import Hydra.Ledger (ValidationResult (Invalid, Valid), cardanoLedger, mkLedger)
import Hydra.Ledger.MaryTest (MaryTest, initUTxO)
import qualified Hydra.Ledger.MaryTest as MaryTest
import Hydra.Logic (HeadParameters (..), SnapshotStrategy (..), createHeadState)
import Hydra.Node (
  EventQueue (nextEvent),
  HydraHead,
  Node (..),
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

type MaryHydraNode m = Node m (Tx MaryTest)

init ::
  MonadThrow m =>
  MaryHydraNode m ->
  m (Either Text ())
init Node{onChainClient, hydraHead, clientSideRepl} =
  bimap show (const ()) <$> Node.init onChainClient hydraHead clientSideRepl

newTx ::
  Monad m =>
  MaryHydraNode m ->
  Tx MaryTest ->
  m (Either Text ())
newTx Node{hydraHead, hydraNetwork} tx =
  Node.newTx hydraHead hydraNetwork tx >>= \case
    Valid -> pure $ Right ()
    Invalid e -> pure $ Left (show e)

createNode :: IO (MaryHydraNode IO)
createNode = do
  eq <- createEventQueue
  hh <- emptyHydraHead
  oc <- createChainClient eq
  hn <- createHydraNetwork eq
  cs <- createClientSideRepl oc hh hn loadTx
  pure $ Node eq hn oc cs hh
 where
  loadTx fp = panic $ "should load and decode a tx from " <> show fp

emptyHydraHead :: MonadSTM m => m (HydraHead (Tx MaryTest) m)
emptyHydraHead = createHydraHead headState ledger
 where
  headState = createHeadState [] HeadParameters SnapshotStrategy
  ledger = cardanoLedger defaultEnv (mkLedger initUTxO)
  defaultEnv = MaryTest.mkLedgersEnv

runNode :: MonadThrow m => MaryHydraNode m -> m ()
runNode hydraNode@Node{eventQueue} =
  forever $ nextEvent eventQueue >>= handleNextEvent hydraNode
