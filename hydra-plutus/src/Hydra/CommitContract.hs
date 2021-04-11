{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -fno-specialize #-}

-- | A special contract that's used only to post `commit` transactions
module Hydra.CommitContract where

import Control.Lens (makeClassyPrisms)
import Data.Functor (void)
import qualified Data.Map as Map
import Ledger (
  Address,
  Datum (..),
  PubKeyHash (..),
  TxOut,
  Validator,
  ValidatorCtx,
  Value,
  scriptAddress,
  txId,
  unspentOutputsTx,
  validatorHash,
 )
import qualified Ledger.Constraints as Constraints
import qualified Ledger.Typed.Scripts as Scripts
import qualified Ledger.Typed.Tx as Tx
import Playground.Contract
import Plutus.Contract
import Plutus.Contract.StateMachine (waitForUpdate)
import qualified Plutus.Contract.StateMachine as SM
import qualified PlutusTx
import PlutusTx.Prelude
import qualified Prelude

import Control.Monad.Error.Lens (throwing)
import Hydra.Contract.Types
import qualified Hydra.ContractStateMachine as CSM

data Committing = Committing {commitPk :: PubKeyHash, commitValue :: Value}
  deriving (Show, Generic, ToJSON, FromJSON)

PlutusTx.makeLift ''Committing
PlutusTx.unstableMakeIsData ''Committing

data Committed = Committed {committedUtxos :: [TxOut]}
  deriving (Show)

PlutusTx.makeLift ''Committed
PlutusTx.unstableMakeIsData ''Committed

{-# INLINEABLE validate #-}
validate :: () -> Committed -> ValidatorCtx -> Bool
validate () _input _ctx = False

--
-- Boilerplate
--

data Commit

instance Scripts.ScriptType Commit where
  type DatumType Commit = ()
  type RedeemerType Commit = Committed

{- ORMOLU_DISABLE -}
contractInstance :: Scripts.ScriptInstance Commit
contractInstance = Scripts.validator @Commit
    $$(PlutusTx.compile [|| validate ||])
    $$(PlutusTx.compile [|| wrap ||]) where
        wrap = Scripts.wrapValidator @() @Committed
{- ORMOLU_ENABLE -}

-- | The validator script of the contract.
contractValidator :: Validator
contractValidator = Scripts.validatorScript contractInstance

-- | The address of the contract (the hash of its validator script)
contractAddress :: Address
contractAddress = Ledger.scriptAddress contractValidator

data CommitError
  = OutputMissing PubKeyHash
  | CommitContractError ContractError
  | CommitSMContractError SM.SMContractError
  deriving stock (Prelude.Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

makeClassyPrisms ''CommitError

instance AsContractError CommitError where
  _ContractError = _CommitContractError

instance SM.AsSMContractError CommitError where
  _SMContractError = _CommitSMContractError

commit ::
  (AsContractError e, SM.AsSMContractError e, AsCommitError e) =>
  SM.StateMachineClient HydraState HydraInput ->
  Contract () Schema e ()
commit client = do
  st <- waitForUpdate client
  case st of
    Just (scriptOut, _)
      | isCollecting (Tx.tyTxOutData scriptOut) -> doCommit
    _ -> commit client
 where
  isCollecting (Collecting CollectingState{}) = True
  isCollecting _ = False

doCommit ::
  (AsContractError e, AsCommitError e) =>
  Contract () Schema e ()
doCommit = do
  Committing pk val <- endpoint @"commit" @Committing
  logInfo @String ("'creating' UTXO with " <> show pk <> ", " <> show val)
  let fundTx = Constraints.mustPayToPubKey pk val
  tx <- submitTxConstraints contractInstance fundTx
  _ <- awaitTxConfirmed (txId tx)
  let output = Map.toList $ unspentOutputsTx tx
  (outRef, txout) <-
    case output of
      [] -> throwing _OutputMissing pk
      ((outRef, outTxOut) : _) -> pure (outRef, outTxOut)

  logInfo @String ("Posting commit tx with" <> show outRef <> ", " <> show txout)

  let datum = Datum $ PlutusTx.toData $ Committed [txout]
      commitTx =
        Constraints.mustSpendPubKeyOutput outRef
          <> Constraints.mustPayToOtherScript (validatorHash CSM.contractValidator) datum val

  tx' <- submitTxConstraints contractInstance commitTx
  void $ awaitTxConfirmed (txId tx')

type Schema =
  BlockchainActions
    .\/ Endpoint "commit" Committing

commitContract ::
  (AsContractError e, SM.AsSMContractError e, AsCommitError e) =>
  -- | Contract needs a SM client to be able to wait on the SM making
  -- progress and reach some state
  SM.StateMachineClient HydraState HydraInput ->
  Contract () Schema e ()
commitContract = commit
