{-# OPTIONS_GHC -Wno-deferred-type-errors #-}

module Hydra.ModelSpec where

import Cardano.Prelude
import Hydra.Model (Action, Model (cluster), Nodes (..), confirmedLedgerUtxos, runModel)
import Test.Hspec (Spec, describe, it)
import Test.QuickCheck (Arbitrary (..), property)

spec :: Spec
spec = describe "Hydra Nodes Model" $ do
  it "checks behavior of a 2 nodes cluster" $
    property ledgerIsUpdatedWithNewTxs

-- |A sequence of `Action` to run.
newtype Actions = Actions {actions :: [Action]}
  deriving (Eq, Show)

instance Arbitrary Actions where
  arbitrary = pure $ Actions []

ledgerIsUpdatedWithNewTxs ::
  Actions -> Bool
ledgerIsUpdatedWithNewTxs Actions{actions} =
  let Nodes nodes = cluster $ runModel actions
   in and [confirmedLedgerUtxos n == confirmedLedgerUtxos n' | n <- nodes, n' <- nodes]
