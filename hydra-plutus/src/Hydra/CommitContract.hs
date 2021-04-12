{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -fno-specialize #-}

-- | A special contract that's used only to post `commit` transactions
module Hydra.CommitContract where

import Control.Lens (makeClassyPrisms)
import Control.Monad.Error.Lens (throwing)
import qualified Data.Map as Map
import Hydra.Contract.Types
import Ledger (
  Address,
  PubKeyHash (..),
  TxOut,
  Validator,
  ValidatorCtx,
  Value,
  scriptAddress,
  txId,
  unspentOutputsTx,
 )
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

PlutusTx.makeLift ''Committing
PlutusTx.unstableMakeIsData ''Committing

data Committed = Committed {committedUtxos :: [TxOut]}
  deriving (Show)

PlutusTx.makeLift ''Committed
PlutusTx.unstableMakeIsData ''Committed

{-# INLINEABLE validate #-}
validate :: () -> Committed -> ValidatorCtx -> Bool
validate () _input _ctx = True

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
  (AsContractError e, SM.AsSMContractError e, AsCommitError e) =>
  SM.StateMachineClient HydraState HydraInput ->
  Contract () Schema e ()
commit client = do
  Committing pk val <- endpoint @"commit" @Committing
  txOut <- doCommit pk val
  logInfo @String ("Committed UTXO: " <> show txOut)
  tr <- SM.runStep client (Commit txOut)
  case tr of
    SM.TransitionSuccess _ -> pure ()
    SM.TransitionFailure f -> throwing _FailedToCommit (show f)

doCommit ::
  (AsContractError e, AsCommitError e) =>
  PubKeyHash ->
  Value ->
  Contract () Schema e TxOut
doCommit pk val = do
  logInfo @String ("Committing UTXO with " <> show pk <> ", " <> show val)
  let fundTx = Constraints.mustPayToPubKey pk val
  tx <- submitTxConstraints contractInstance fundTx
  _ <- awaitTxConfirmed (txId tx)
  let output = Map.toList $ unspentOutputsTx tx
  case output of
    [_, (_, outTxOut)] -> pure outTxOut
    _ -> throwing _OutputMissing pk

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
