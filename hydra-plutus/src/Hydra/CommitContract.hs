{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -fno-specialize #-}

-- | A special contract that's used only to post `commit` transactions
module Hydra.CommitContract where

import Control.Lens (makeClassyPrisms)
import Control.Monad (void)
import Control.Monad.Error.Lens (throwing)
import qualified Data.Map as Map
import Hydra.Contract.Types
import Ledger (
  Address,
  PubKeyHash (..),
  TxOut,
  TxOutTx (..),
  Validator,
  ValidatorCtx,
  Value,
  scriptAddress,
  txId,
  txOutTxOut,
  unspentOutputsTx,
 )
import qualified Ledger.Ada as Ada
import qualified Ledger.Constraints as Constraints
import qualified Ledger.Typed.Scripts as Scripts
import Playground.Contract
import Plutus.Contract
import qualified Plutus.Contract.StateMachine as SM
import qualified PlutusTx
import PlutusTx.Prelude
import qualified Prelude

data Committing = Committing {commitPk :: PubKeyHash, commitValue :: Value}
  deriving (Show, Generic, ToJSON, FromJSON)

data Committed = Committed TxOut
  deriving (Show, Generic, ToJSON, FromJSON)

PlutusTx.makeLift ''Committing
PlutusTx.unstableMakeIsData ''Committing

PlutusTx.makeLift ''Committed
PlutusTx.unstableMakeIsData ''Committed

{-# INLINEABLE validate #-}
validate :: HeadParameters -> Committed -> () -> ValidatorCtx -> Bool
validate (HeadParameters pubKeys _) (Committed txOut) () _ =
  txOut `isIn` pubKeys

--
-- Boilerplate
--

data Commit

instance Scripts.ScriptType Commit where
  type DatumType Commit = Committed
  type RedeemerType Commit = ()

{- ORMOLU_DISABLE -}
contractInstance :: HeadParameters  -> Scripts.ScriptInstance Commit
contractInstance = Scripts.validatorParam @Commit
    $$(PlutusTx.compile [|| validate ||])
    $$(PlutusTx.compile [|| wrap ||]) where
        wrap = Scripts.wrapValidator
{- ORMOLU_ENABLE -}

-- | The validator script of the contract.
contractValidator :: HeadParameters -> Validator
contractValidator = Scripts.validatorScript . contractInstance

-- | The address of the contract (the hash of its validator script)
contractAddress :: HeadParameters -> Address
contractAddress = Ledger.scriptAddress . contractValidator

data CommitError
  = OutputMissing PubKeyHash
  | FailedToCommit String
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
  (AsContractError e, AsCommitError e) =>
  HeadParameters ->
  Contract () Schema e ()
commit params = do
  Committing pk val <- endpoint @"commit" @Committing
  (outRef, txOut) <- createUTXOToBeLocked params pk val
  logInfo @String ("Created UTXO: " <> show txOut <> " @ " <> show outRef)
  -- we must wait some 'time' before creating the actual commit tx, apparently
  -- otherwise the outRef cannot be found
  void $ waitNSlots 3
  createCommitTx params (outRef, txOut)

createCommitTx ::
  (AsContractError e) =>
  HeadParameters ->
  (TxOutRef, TxOutTx) ->
  Contract () Schema e ()
createCommitTx params (outRef, txOutTx) = do
  let inst = contractInstance params
      ctx =
        Constraints.mustSpendPubKeyOutput outRef
          <> Constraints.mustPayToTheScript (Committed $ txOutTxOut txOutTx) (Ada.lovelaceValueOf 1)
      lookups =
        Constraints.scriptInstanceLookups inst
          <> Constraints.unspentOutputs (Map.fromList [(outRef, txOutTx)])

  utx <- either (throwing _ConstraintResolutionError) pure (Constraints.mkTx lookups ctx)
  tx <- submitUnbalancedTx utx
  awaitTxConfirmed (txId tx)

createUTXOToBeLocked ::
  (AsContractError e, AsCommitError e) =>
  HeadParameters ->
  PubKeyHash ->
  Value ->
  Contract () Schema e (TxOutRef, TxOutTx)
createUTXOToBeLocked params pk val = do
  logInfo @String ("Committing UTXO with " <> show pk <> ", " <> show val)
  let fundTx = Constraints.mustPayToPubKey pk val
  tx <- submitTxConstraints (contractInstance params) fundTx
  awaitTxConfirmed (txId tx)
  let output = Map.toList $ unspentOutputsTx tx
  case output of
    [_, (outRef, outTxOut)] -> pure (outRef, TxOutTx tx outTxOut)
    _ -> throwing _OutputMissing pk

type Schema =
  BlockchainActions
    .\/ Endpoint "commit" Committing

commitContract ::
  (AsContractError e, AsCommitError e) =>
  -- | Contract needs is individualised by the specific `HeadParameters` used
  HeadParameters ->
  Contract () Schema e ()
commitContract = commit
