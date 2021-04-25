{-# OPTIONS_GHC -Wno-deferred-type-errors #-}
{-# OPTIONS_GHC -Wno-deprecations #-}

module Hydra.ModelSpec where

import Cardano.Prelude
import Hydra.Ledger.MaryTest (MaryTest)
import Hydra.Model (Action (..), Model (cluster), ModelState (..), Nodes (..), Request (Init), confirmedLedgerUtxos, ledger, runModel)
import Test.Cardano.Ledger.Mary ()
import Test.Hspec (Spec, describe, it)
import Test.QuickCheck (Arbitrary (..), Gen, Property, counterexample, property)
import Test.Shelley.Spec.Ledger.Generator.EraGen (genUtxo0)
import Test.Shelley.Spec.Ledger.Generator.Presets (genEnv)

spec :: Spec
spec = describe "Hydra Nodes Model" $ do
  it "checks behavior of a 2 nodes cluster" $
    property ledgerIsUpdatedWithNewTxs

-- |A sequence of `Action` to run.
newtype Actions = Actions {actions :: [Action]}
  deriving (Eq, Show)

instance Arbitrary Actions where
  arbitrary = Actions <$> genActions Closed

genActions :: ModelState -> Gen [Action]
genActions Closed = do
  utxos <- genUtxo0 (genEnv @MaryTest Proxy)
  (Action 1 Init :) <$> genActions (Open utxos)
genActions (Open _utxos) = undefined

ledgerIsUpdatedWithNewTxs ::
  Actions -> Property
ledgerIsUpdatedWithNewTxs Actions{actions} =
  let model' = runModel actions
      Nodes nodes = cluster model'
      expectedUtxos = ledger model'
      msg = "Expected all ledgers to have UTxOs matching " <> show expectedUtxos
   in counterexample msg $ and [confirmedLedgerUtxos n == expectedUtxos | n <- nodes]
