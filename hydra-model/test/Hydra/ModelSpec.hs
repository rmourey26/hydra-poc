{-# OPTIONS_GHC -Wno-deferred-type-errors #-}
{-# OPTIONS_GHC -Wno-deprecations #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module Hydra.ModelSpec where

import Cardano.Prelude hiding (head)
import Hydra.Ledger (globals)
import Hydra.Ledger.MaryTest (MaryTest, mkLedgerEnv, mkLedgersEnv)
import Hydra.Model (Action (..), HeadState (..), ModelState (..), NodeId (..), Request (..), Utxo, expectedUtxo, makeLedger, runModel)
import Shelley.Spec.Ledger.API (LedgerState (LedgerState), applyTxsTransition)
import Shelley.Spec.Ledger.PParams (PParams' (..))
import Test.Cardano.Ledger.Mary ()
import Test.Hspec (Spec, describe, it, xit)
import Test.QuickCheck (Arbitrary (..), Gen, Property, choose, counterexample, elements, property)
import Test.Shelley.Spec.Ledger.Generator.EraGen (genUtxo0)
import Test.Shelley.Spec.Ledger.Generator.Presets (genEnv)
import Test.Shelley.Spec.Ledger.Generator.Utxo (genTx)

spec :: Spec
spec =
  describe "Hydra Nodes Model" $ do
    it "can Init/Close a 2-nodes cluster" $ property ledgerIsInitialisedWithCommittedUTxOs
    xit "can post NewTx to a 2-nodes cluster" $ property ledgerIsUpdatedWithNewTxs

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

ledgerIsUpdatedWithNewTxs ::
  Actions -> Property
ledgerIsUpdatedWithNewTxs (Actions actions) =
  counterexample msg $
    length nodeLedgers == 2
      && and [nodeLedger == expectedUtxo currentState | nodeLedger <- nodeLedgers]
 where
  ModelState{nodeLedgers, currentState} = runModel actions
  msg =
    "Expected all ledgers to have UTxOs matching "
      <> show currentState
      <> " after some NewTx, got "
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

-- shrink (Actions []) = []
-- shrink (Actions [_]) = []
-- shrink (Actions [_, _]) = []
-- shrink (Actions (i : rest)) =
--   let c = last rest
--       as = take (length rest - 2) (drop 1 rest)
--    in [Actions $ i : as <> [c]]

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
genActions 0 _ = pure <$> genCloseAction
genActions n (Closed Nothing) = do
  utxos <- genUtxo0 (genEnv @MaryTest Proxy)
  initAction <- genInitAction utxos
  (initAction :) <$> genActions (n -1) (Open $ makeLedger utxos)
genActions n (Open l@(LedgerState utxos deleg)) = do
  tx <- genTx (genEnv @MaryTest Proxy) mkLedgerEnv (utxos, deleg)
  toNode <- NodeId <$> elements [1, 2]
  let l' =
        case applyTxsTransition globals mkLedgersEnv (pure tx) l of
          Right lg -> lg
          Left e -> panic $ "Should not happen as the tx " <> show tx <> " is guaranteed to be valid?: " <> show e
  (Action toNode (NewTx tx) :) <$> genActions (n -1) (Open l')
