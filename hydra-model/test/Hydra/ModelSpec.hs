{-# OPTIONS_GHC -Wno-deferred-type-errors #-}
{-# OPTIONS_GHC -Wno-deprecations #-}

module Hydra.ModelSpec where

import Cardano.Prelude
import Data.Default (def)
import Hydra.Ledger (globals)
import Hydra.Ledger.MaryTest (MaryTest, mkLedgerEnv, mkLedgersEnv)
import Hydra.Model (Action (..), Model (cluster), ModelState (..), NodeId (..), Nodes (..), Request (..), confirmedLedgerUtxos, ledger, runModel)
import Shelley.Spec.Ledger.API (Coin (..), DPState (..), LedgerState (..), UTxOState (..), applyTxsTransition)

-- This is important as it provides some HasField instances which are needed for `applyTxsTransition` to
-- work propertly.
import Shelley.Spec.Ledger.PParams (PParams' (..))
import Test.Cardano.Ledger.Mary ()
import Test.Hspec (Spec, describe, it)
import Test.QuickCheck (Arbitrary (..), Gen, Property, choose, counterexample, elements, property)
import Test.Shelley.Spec.Ledger.Generator.EraGen (genUtxo0)
import Test.Shelley.Spec.Ledger.Generator.Presets (genEnv)
import Test.Shelley.Spec.Ledger.Generator.Utxo (genTx)

spec :: Spec
spec = describe "Hydra Nodes Model" $ do
  it "checks behavior of a 2 nodes cluster" $ property ledgerIsUpdatedWithNewTxs

ledgerIsUpdatedWithNewTxs ::
  Actions -> Property
ledgerIsUpdatedWithNewTxs Actions{actions} =
  let model' = runModel actions
      Nodes nodes = cluster model'
      expectedUtxos = ledger model'
      msg = "Expected all ledgers to have UTxOs matching " <> show expectedUtxos
   in counterexample msg $ and [confirmedLedgerUtxos n == expectedUtxos | n <- nodes]

-- |A sequence of `Action` to run.
newtype Actions = Actions {actions :: [Action]}
  deriving (Eq, Show)

instance Arbitrary Actions where
  arbitrary = do
    numActions <- choose (0, 10)
    Actions <$> genActions numActions Closed

-- | Generate a sequence of actions which start with `Init`
-- We generate valid tansactions strating from some initial ledger state and request
-- random nodes to post `NewTx`
genActions :: Int -> ModelState -> Gen [Action]
genActions 0 _ = pure []
genActions n Closed = do
  ds <- arbitrary
  ps <- arbitrary
  utxos <- genUtxo0 (genEnv @MaryTest Proxy)
  (Action 1 Init :) <$> genActions (n -1) (Open $ LedgerState (UTxOState utxos (Coin 0) (Coin 0) def) (DPState ds ps))
genActions n (Open l@(LedgerState utxos deleg)) = do
  tx <- genTx (genEnv @MaryTest Proxy) mkLedgerEnv (utxos, deleg)
  toNode <- NodeId <$> elements [1, 2]
  let l' =
        case applyTxsTransition globals mkLedgersEnv (pure tx) l of
          Right lg -> lg
          Left _ -> panic "Should not happen as the tx is guaranteed to be valid?"
  (Action toNode (NewTx tx) :) <$> genActions (n -1) (Open l')
