import Cardano.Prelude
import Hydra.Ledger (cardanoLedger, mkCardanoMaryLedgerEnv)
import Hydra.Logic (Event (NetworkEvent, OnChainEvent), HeadParameters (..), SnapshotStrategy (..), createHeadState)
import Hydra.Node

main :: IO ()
main = do
  EventQueue{putEvent, nextEvent} <- createEventQueue
  hh <- createHydraHead headState (cardanoLedger mkCardanoMaryLedgerEnv)
  oc <- createChainClient (putEvent . OnChainEvent)
  hn <- createHydraNetwork (putEvent . NetworkEvent)
  cs <- createClientSideRepl oc hh hn loadTx

  -- NOTE(SN): here we would introduce concurrent head processing, e.g. with
  -- something like 'forM_ [0..1] $ async'
  forever $ nextEvent >>= handleNextEvent hn oc cs hh
 where
  headState = createHeadState [] HeadParameters SnapshotStrategy

  loadTx fp = panic $ "should load and decode a tx from " <> show fp
