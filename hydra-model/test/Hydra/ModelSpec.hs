{-# OPTIONS_GHC -Wno-deferred-type-errors #-}
{-# OPTIONS_GHC -Wno-deprecations #-}

module Hydra.ModelSpec where

import Cardano.Prelude
import Hydra.Ledger (mkLedger)
import Hydra.Ledger.MaryTest (MaryTest)
import Hydra.Model (Action (..), HeadState (..), ModelState (..), NodeId (..), Request (..), expectedUtxo, runModel)

-- This is important as it provides some HasField instances which are needed for `applyTxsTransition` to
-- work propertly.

import Test.Cardano.Ledger.Mary ()
import Test.Hspec (Spec, describe, it)
import Test.QuickCheck (Arbitrary (..), Gen, Property, choose, counterexample, elements, property)
import Test.Shelley.Spec.Ledger.Generator.EraGen (genUtxo0)
import Test.Shelley.Spec.Ledger.Generator.Presets (genEnv)

spec :: Spec
spec =
  describe "Hydra Nodes Model" $
    it "checks behavior of a 2 nodes cluster" $ property ledgerIsUpdatedWithNewTxs

ledgerIsUpdatedWithNewTxs ::
  Actions -> Property
ledgerIsUpdatedWithNewTxs Actions{actions} =
  let ModelState{nodeLedgers, currentState} = runModel actions
      msg = "Expected all ledgers to have UTxOs matching " <> show currentState <> " after actions " <> show actions
   in counterexample msg $
        length nodeLedgers == 2
          && and [nodeLedger == expectedUtxo currentState | nodeLedger <- nodeLedgers]

-- |A sequence of `Action` to run.
newtype Actions = Actions {actions :: [Action]}
  deriving (Eq, Show)

instance Arbitrary Actions where
  arbitrary = do
    numActions <- choose (1, 10)
    Actions <$> genActions numActions Closed

-- | Generate a sequence of actions which start with `Init`
-- We generate valid tansactions strating from some initial ledger state and request
-- random nodes to post `NewTx`
genActions :: Int -> HeadState -> Gen [Action]
genActions _ Failed{} = pure []
genActions n Closed = do
  utxos <- genUtxo0 (genEnv @MaryTest Proxy)
  (Action 1 (Init utxos) :) <$> genActions (n -1) (Open $ mkLedger utxos)
genActions _ _ = do
  toNode <- NodeId <$> elements [1, 2]
  pure [Action toNode Close]

-- genActions n (Open l@(LedgerState utxos deleg)) = do
--   tx <- genTx (genEnv @MaryTest Proxy) mkLedgerEnv (utxos, deleg)
--   toNode <- NodeId <$> elements [1, 2]
--   let l' =
--         case applyTxsTransition globals mkLedgersEnv (pure tx) l of
--           Right lg -> lg
--           Left _ -> panic "Should not happen as the tx is guaranteed to be valid?"
--   (Action toNode (NewTx tx) :) <$> genActions (n -1) (Open l')
