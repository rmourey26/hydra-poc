{-# OPTIONS_GHC -Wno-deferred-type-errors #-}
{-# OPTIONS_GHC -Wno-deprecations #-}

module Hydra.ModelSpec where

import Cardano.Prelude
import Hydra.Ledger.MaryTest (MaryTest)
import Hydra.Model (Action (..), HeadState (..), ModelState (..), NodeId (..), Request (..), Utxo, expectedUtxo, runModel)

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
    it "can Init/Close a 2-nodes cluster" $ property ledgerIsInitialisedWithCommittedUTxOs

ledgerIsInitialisedWithCommittedUTxOs ::
  InitAndClose -> Property
ledgerIsInitialisedWithCommittedUTxOs (InitAndClose actions) =
  counterexample msg $
    length nodeLedgers == 2
      && and [nodeLedger == expectedUtxo currentState | nodeLedger <- nodeLedgers]
 where
  ModelState{nodeLedgers, currentState} = runModel actions
  msg =
    "Expected all ledgers to have UTxOs matching "
      <> show currentState
      <> " after Init and Close, got "
      <> show nodeLedgers

newtype InitAndClose = InitAndClose [Action]
  deriving (Eq, Show)

instance Arbitrary InitAndClose where
  arbitrary = do
    utxos <- genUtxo0 (genEnv @MaryTest Proxy)
    i <- genInitAction utxos
    c <- genCloseAction
    pure $ InitAndClose [i, c]

-- |A sequence of `Action` to run.
newtype Actions = Actions {actions :: [Action]}
  deriving (Eq, Show)

instance Arbitrary Actions where
  arbitrary = do
    numActions <- choose (1, 10)
    Actions <$> genActions numActions (Closed Nothing)

chooseNode :: Gen NodeId
chooseNode = NodeId <$> elements [1, 2]

genInitAction :: Utxo -> Gen Action
genInitAction utxos = do
  nid <- chooseNode
  pure $ Action nid (Init utxos)

genCloseAction :: Gen Action
genCloseAction = do
  toNode <- chooseNode
  pure $ Action toNode Close

-- | Generate a sequence of actions which start with `Init`
-- We generate valid tansactions strating from some initial ledger state and request
-- random nodes to post `NewTx`, then `Close` the head
genActions :: Int -> HeadState -> Gen [Action]
genActions _ Failed{} = pure []
genActions n (Closed Nothing) = do
  utxos <- genUtxo0 (genEnv @MaryTest Proxy)
  initAction <- genInitAction utxos
  (initAction :) <$> genActions (n -1) (Open utxos)
genActions _ _ = pure <$> genCloseAction

-- genActions n (Open l@(LedgerState utxos deleg)) = do
--   tx <- genTx (genEnv @MaryTest Proxy) mkLedgerEnv (utxos, deleg)
--   toNode <- NodeId <$> elements [1, 2]
--   let l' =
--         case applyTxsTransition globals mkLedgersEnv (pure tx) l of
--           Right lg -> lg
--           Left _ -> panic "Should not happen as the tx is guaranteed to be valid?"
--   (Action toNode (NewTx tx) :) <$> genActions (n -1) (Open l')
