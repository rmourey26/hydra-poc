{-# OPTIONS_GHC -Wno-deferred-type-errors #-}
{-# OPTIONS_GHC -Wno-deprecations #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module Hydra.ModelSpec where

import Cardano.Prelude hiding (head)
import Data.List(last)
import Hydra.Ledger (globals)
import Hydra.Ledger.MaryTest (MaryTest, mkLedgerEnv, mkLedgersEnv)
import Hydra.Model (Action (..), HeadState (..), ModelState (..), NodeId (..), Request (..), Utxo, expectedUtxo, makeLedger, runModel)
import Shelley.Spec.Ledger.API (LedgerState (LedgerState), applyTxsTransition)
import Shelley.Spec.Ledger.PParams (PParams' (..))
import Test.Cardano.Ledger.Mary ()
import Test.Hspec (Spec, describe, it)
import Test.QuickCheck (Arbitrary (..), Gen, Property, choose, counterexample, elements, property, collect)
import Test.Shelley.Spec.Ledger.Generator.EraGen (genUtxo0)
import Test.Shelley.Spec.Ledger.Generator.Presets (genEnv)
import Test.Shelley.Spec.Ledger.Generator.Utxo (genTx)
import Test.Shelley.Spec.Ledger.Generator.Constants (Constants(..), defaultConstants)
import Test.Shelley.Spec.Ledger.Generator.Core (geConstants, GenEnv)

spec :: Spec
spec =
  describe "Hydra Nodes Model" $ do
    it "can Init/Close a 2-nodes cluster" $ property ledgerIsInitialisedWithCommittedUTxOs
    it "can post NewTx to a 2-nodes cluster" $ property ledgerIsUpdatedWithNewTxs

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
  collect (length actions) $
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
    Actions <$> genActions numActions numActions (Closed Nothing)

  shrink (Actions []) = []
  shrink (Actions [_]) = []
  shrink (Actions [_, _]) = []
  shrink (Actions (i : rest)) =
    let c = last rest
        as = take (length rest - 2) (drop 1 rest)
     in [Actions $ i : as <> [c]]

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

hydraConstants :: Constants
hydraConstants =
  defaultConstants {
   frequencyRegKeyCert = 0,
      frequencyRegPoolCert = 0,
      frequencyDelegationCert = 1,
      frequencyGenesisDelegationCert = 0,
      frequencyDeRegKeyCert = 0,
      frequencyRetirePoolCert = 0,
      frequencyMIRCert = 0,
      frequencyScriptCredReg = 0,
      frequencyKeyCredReg = 0,
      frequencyScriptCredDeReg = 0,
      frequencyKeyCredDeReg = 0,
      frequencyScriptCredDelegation = 0,
      frequencyKeyCredDelegation = 0,
      frequencyTxUpdates = 0,
      frequencyTxWithMetadata = 1}

hydraGenEnv :: GenEnv MaryTest
hydraGenEnv = (genEnv @MaryTest Proxy) {geConstants = hydraConstants}

-- | Generate a sequence of actions which start with `Init`
-- We generate valid tansactions strating from some initial ledger state and request
-- random nodes to post `NewTx`, then `Close` the head
genActions :: Int -> Int -> HeadState -> Gen [Action]
genActions _ _ Failed{} = pure []
genActions _ 0 _ = pure <$> genCloseAction
genActions m n (Closed Nothing) = do
  utxos <- genUtxo0 hydraGenEnv
  initAction <- genInitAction utxos
  (initAction :) <$> genActions m (n -1) (Open $ makeLedger utxos)
genActions m n (Open l@(LedgerState utxos deleg)) = do
  let slot = fromIntegral $ m - n -1
  tx <- genTx hydraGenEnv (mkLedgerEnv slot) (utxos, deleg)
  toNode <- NodeId <$> elements [1, 2]
  let env = mkLedgersEnv slot
      l' =
        case applyTxsTransition globals env (pure tx) l of
          Right lg -> lg
          Left e -> panic $ "tx " <> show tx <> " is guaranteed to be valid?: " <> show e
  (Action toNode (NewTx slot tx) :) <$> genActions m (n -1) (Open l')
