import Cardano.Prelude
import Hydra.Ledger (cardanoLedger, mkLedgerEnv)
import Hydra.Logic (HeadParameters (..), SnapshotStrategy (..), createHeadState)
import Hydra.Node

main :: IO ()
main = do
  eq <- createEventQueue
  hh <- createHydraHead headState (cardanoLedger mkCardanoMaryLedgerEnv)
  oc <- createChainClient eq
  hn <- createHydraNetwork eq
  cs <- createClientSideRepl oc hh hn loadTx

  -- NOTE(SN): here we would introduce concurrent head processing, e.g. with
  -- something like 'forM_ [0..1] $ async'
  forever $ nextEvent eq >>= handleNextEvent hn oc cs hh
 where
  headState = createHeadState [] HeadParameters SnapshotStrategy

  loadTx fp = panic $ "should load and decode a tx from " <> show fp
