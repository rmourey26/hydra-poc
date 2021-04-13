{-# LANGUAGE TemplateHaskell #-}

module Hydra.ContractStateMachineSpec where

import Cardano.Prelude
import Hydra.CommitContract as Commit
import Hydra.Contract.Types (Eta (..), HeadParameters (..), HydraState (..), MultisigPublicKey (..), OpenState (..), UTXO (..), toDatumHash)
import Hydra.ContractStateMachine as SM
import Hydra.MonetaryPolicy (hydraCurrencySymbol)
import Hydra.Utils (datumAtAddress)
import Ledger (PubKeyHash (..), Tx (..), TxOut, pubKeyHash, txOutValue)
import qualified Ledger.Ada as Ada
import Ledger.Constraints.OffChain (UnbalancedTx (..))
import Ledger.Value (flattenValue)
import Plutus.Contract hiding (runError)
import Plutus.Contract.StateMachine (SMContractError)
import Plutus.Contract.Test
import qualified Plutus.Trace.Emulator as Trace
import Test.Tasty

w1 :: Wallet
w1 = Wallet 1

w2 :: Wallet
w2 = Wallet 2

theHydraContract :: Contract () SM.Schema SMContractError ()
theHydraContract = contract headParameters

theCommitContract :: Contract () Commit.Schema CommitError ()
theCommitContract = commitContract headParameters

pubKey1 :: PubKeyHash
pubKey1 = pubKeyHash $ walletPubKey w1

pubKey2 :: PubKeyHash
pubKey2 = pubKeyHash $ walletPubKey w2

headParameters :: HeadParameters
headParameters =
  HeadParameters
    { verificationKeys = [pubKey1, pubKey2]
    , currencyId = hydraCurrencySymbol 14
    }

tests :: TestTree
tests =
  testGroup
    "Contract StateMachine"
    [ testGroup
        "StateMachine Contract Behaviour"
        [ -- checkCompiledContractPIR "test/Hydra/ContractStateMachine.pir" compiledScript
          checkPredicate
            "Expose 'collectCom' and 'close' endpoints"
            ( endpointAvailable @"collectCom" theHydraContract (Trace.walletInstanceTag w1)
                .&&. endpointAvailable @"close" theHydraContract (Trace.walletInstanceTag w1)
            )
            $ void (Trace.activateContractWallet w1 theHydraContract)
        , checkPredicate
            "Closed state after setup > init > collectCom > close"
            (assertNoFailedTransactions .&&. assertStateIsClosed)
            setupInitCollectAndClose
        , checkPredicate
            "Collecting holds all keys after init"
            (assertNoFailedTransactions .&&. assertState Collecting)
            $ do
              contractHandle <- Trace.activateContractWallet w1 theHydraContract
              Trace.callEndpoint @"setup" contractHandle ()
              void Trace.nextSlot
              Trace.callEndpoint @"init" contractHandle ()
        , checkPredicate
            "External 'Commit' transactions from all parties is acknowledged by CollectCom"
            (assertNoFailedTransactions .&&. assertState (Open $ OpenState{keyAggregate = MultisigPublicKey [], eta = Eta UTXO 0 []}))
            $ do
              hydraDriver <- Trace.activateContractWallet w1 theHydraContract
              committer1 <- Trace.activateContractWallet w1 theCommitContract
              committer2 <- Trace.activateContractWallet w2 theCommitContract
              Trace.callEndpoint @"setup" hydraDriver ()
              void $ Trace.waitNSlots 1
              Trace.callEndpoint @"init" hydraDriver ()
              void $ Trace.waitNSlots 1
              Trace.callEndpoint @"collectCom" hydraDriver ()
              void $ Trace.waitNSlots 1
              Trace.callEndpoint @"commit" committer1 (Committing pubKey1 $ Ada.lovelaceValueOf 10)
              void $ Trace.waitNSlots 1
              Trace.callEndpoint @"commit" committer2 (Committing pubKey2 $ Ada.lovelaceValueOf 15)
              void $ Trace.waitNSlots 10
        ]
    ]

assertState :: HydraState -> TracePredicate
assertState = datumAtAddress (SM.contractAddress headParameters) . toDatumHash

assertStateIsClosed :: TracePredicate
assertStateIsClosed =
  datumAtAddress (SM.contractAddress headParameters) (toDatumHash Closed)

assertInitTxShape :: HeadParameters -> UnbalancedTx -> Bool
assertInitTxShape HeadParameters{verificationKeys} unbalancedTx =
  let outputs = txOutputs (unBalancedTxTx unbalancedTx)
      numberOfParticipants = length verificationKeys
   in length outputs == numberOfParticipants + 1
        && participationTokensAreUnique outputs numberOfParticipants

participationTokensAreUnique :: [TxOut] -> Int -> Bool
participationTokensAreUnique outputs numberOfParticipants =
  length (filter (hasParticipationToken 1) outputs) == numberOfParticipants

--    && Set.size (assetNames policyId tx) == numberOfParticipants

hasParticipationToken :: Int -> TxOut -> Bool
hasParticipationToken numberOfTokens txOut =
  let tokens = filter (\(cur, _, _) -> cur /= Ada.adaSymbol) $ flattenValue $ txOutValue txOut
   in length tokens == numberOfTokens

collectAndClose :: Trace.EmulatorTrace ()
collectAndClose = do
  callCollectCom
  void Trace.nextSlot
  callClose
  void Trace.nextSlot

callCollectCom :: Trace.EmulatorTrace ()
callCollectCom = do
  contractHandle <- Trace.activateContractWallet w1 theHydraContract
  Trace.callEndpoint @"collectCom" contractHandle ()

callClose :: Trace.EmulatorTrace ()
callClose = do
  contractHandle <- Trace.activateContractWallet w1 theHydraContract
  Trace.callEndpoint @"close" contractHandle ()

setupInitCollectAndClose :: Trace.EmulatorTrace ()
setupInitCollectAndClose = do
  alice <- Trace.activateContractWallet w1 theHydraContract
  --bob <- Trace.activateContractWallet w2 theHydraContract
  Trace.callEndpoint @"setup" alice ()
  void Trace.nextSlot
  Trace.callEndpoint @"init" alice ()
  void Trace.nextSlot
  Trace.callEndpoint @"collectCom" alice ()
  void Trace.nextSlot
  Trace.callEndpoint @"close" alice ()
  void Trace.nextSlot
