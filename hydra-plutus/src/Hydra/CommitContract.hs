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
  Address (PubKeyAddress),
  PubKeyHash (..),
  TxOut (..),
  TxOutTx (..),
  TxOutType (PayToPubKey),
  Validator,
  ValidatorCtx,
  Value,
  pubKeyHash,
  scriptAddress,
  txId,
  txOutTxOut,
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

--

-- * On-Chain Part

data Committed = Committed TxOut
  deriving (Show, Generic, ToJSON, FromJSON)

PlutusTx.makeLift ''Committed
PlutusTx.unstableMakeIsData ''Committed

{-# INLINEABLE validate #-}

-- | The commit output consumer validator.
-- This is called `ν_com` in the Hydra paper(s) and its address is used by the Hydra SM
-- to collect the `Committed` UTXOs to form the initial state of the Head.
validate ::
  -- | The `HeadParameters` for a specific Hydra Head instance
  -- The purpose of this parameter is twofold:
  --  * It provides a way to ensure each Head instance uses a different validator function
  --    and script address,
  --  * It fixes the set of public keys which are allowed to commit UTXOs into the head,
  --    and this function validates that fact.
  HeadParameters ->
  -- | The `Datum` associated to the transaction output which needs to be "redeemed" by
  -- the consuming transaction
  Committed ->
  -- | The `Redeemer` provided by the consuming transaction.
  -- In this case, there's nothing specific to prove beyond the fact the transaction
  -- can consume outputs with this script's instance address, which is predicated on
  -- knowledge of the right set of `HeadParameters`.
  () ->
  ValidatorCtx ->
  Bool
validate (HeadParameters pubKeys _) (Committed txOut) () _ =
  -- nothing proves the `txOut` was actually consumed by the transaction, this should be
  -- verified in the nu_validator function when constructing the commit transction itself
  txOut `isIn` pubKeys

{-# INLINEABLE isIn #-}

-- | An auxiliary function to check some output is in some list of public keys.
-- This function is written using as few other functions as possible as the set of covered
-- functions from `PlutusTx.Prelude` is pretty small. In particular, there's no `List.elem`.
isIn :: TxOut -> [PubKeyHash] -> Bool
isIn (TxOut (PubKeyAddress p) _ PayToPubKey) pubkeys =
  case uniqueElement (filter (== p) pubkeys) of
    Just _ -> True
    Nothing -> False
isIn _ _ = False

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

-- | Possible errors raisable by the `Commit` contract.
data CommitError
  = OutputMissing PubKeyHash
  | CommitContractError ContractError
  | CommitSMContractError SM.SMContractError
  deriving stock (Prelude.Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

-- ??
-- This seem to be important within the machinery to raise errors inside `Contract`
-- monad.
makeClassyPrisms ''CommitError

instance AsContractError CommitError where
  _ContractError = _CommitContractError

instance SM.AsSMContractError CommitError where
  _SMContractError = _CommitSMContractError

-- * Off-Chain Part

data Committing = Committing {commitPk :: PubKeyHash, commitValue :: Value}
  deriving (Show, Generic, ToJSON, FromJSON)

PlutusTx.makeLift ''Committing
PlutusTx.unstableMakeIsData ''Committing

-- | commit endpoint handler.
-- The @commit@ endpoint expects and argument `Committing` which should normally
-- contains a list of pairs of `TxOutRef` and `TxOutTx` representing the UTXOs to
-- be committed in the head. For the sake of simplicity, we just take a `Value`
-- and pays that `Value` to the `PubKeyHash` given, then actually create the
-- Hydra commit transaction.
commit ::
  (AsContractError e, AsCommitError e) =>
  HeadParameters ->
  Contract () Schema e ()
commit params = do
  Committing pk val <- endpoint @"commit" @Committing
  (outRef, txOut) <- createUTXOToBeLocked params pk val
  logInfo @String ("Created UTXO: " <> show txOut <> " @ " <> show outRef)
  createCommitTx params (outRef, txOut)

-- | A function that creates the UTXO to be locked within Hydra Head.
-- Obviously, this function should not exist and the user should be
-- able to commit any UTXO she's able to spend. It's here for
-- convenience in the testing process, so that the interface to
-- `commit` is simply a `Value` to be committed.
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
    -- this is gross, we 'guess' the output we are interested in is the second
    -- one, the first one being the balance from the wallet used to create the
    -- transaction
    [_, (outRef, outTxOut)] -> pure (outRef, TxOutTx tx outTxOut)
    _ -> throwing _OutputMissing pk

createCommitTx ::
  (AsContractError e) =>
  HeadParameters ->
  (TxOutRef, TxOutTx) ->
  Contract () Schema e ()
createCommitTx params (outRef, txOutTx) = do
  pk <- ownPubKey
  let inst = contractInstance params
      val = txOutValue $ txOutTxOut txOutTx
      constraints =
        -- we want to consume the given UTXO reference
        Constraints.mustSpendPubKeyOutput outRef
          -- ... and send the value to the ν_com script validation, eg. the commit contract
          <> Constraints.mustPayToTheScript (Committed $ txOutTxOut txOutTx) val
      -- Lookups are needed for the transaction to be succesfully created and valid, to
      -- prove the inputs and outputs are valid
      lookups =
        -- This is the script that is a witness to the address of the `mustPayToTheScript`
        -- constraint
        Constraints.scriptInstanceLookups inst
          -- the actual map from references consumed to transaction and output which produced
          -- them. This is needed to witness the output reference are valid
          Prelude.<> Constraints.unspentOutputs (Map.fromList [(outRef, txOutTx)])
          -- the signing key for the transaction
          Prelude.<> Constraints.ownPubKeyHash (pubKeyHash pk)

  utx <- either (throwing _ConstraintResolutionError) pure (Constraints.mkTx lookups constraints)
  tx <- submitUnbalancedTx utx
  awaitTxConfirmed (txId tx)

type Schema =
  BlockchainActions
    .\/ Endpoint "commit" Committing

commitContract ::
  (AsContractError e, AsCommitError e) =>
  -- | Contract is individualised by the specific `HeadParameters` used.
  HeadParameters ->
  Contract () Schema e ()
commitContract = commit
