{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -fno-specialize #-}

-- | A special contract that's used only to post `commit` transactions
module Hydra.CommitContract where

import Control.Monad (forever, guard, void)
import Ledger (Address, PubKeyHash (..), TxId, Validator, ValidatorCtx, Value, scriptAddress)
import qualified Ledger.Ada as Ada
import qualified Ledger.Typed.Scripts as Scripts
import Playground.Contract
import Plutus.Contract
import Plutus.Contract.StateMachine (State (..), Void)
import qualified Plutus.Contract.StateMachine as SM
import qualified PlutusTx
import PlutusTx.Prelude
import qualified Prelude

import Hydra.Contract.Types

{-# INLINEABLE validate #-}
validate :: () -> Committed -> ValidatorCtx -> Bool
validate () _input _ctx = False

data Committed = Committed {committedUtxos :: [(TxId, Integer)]}
  deriving (Show)

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

commit ::
  (AsContractError e, SM.AsSMContractError e) =>
  Contract () Schema e ()
commit = do
  valueCommitted <- endpoint @"commit" @Value
  logInfo @String "commitEndpoint"
  void $ SM.runStep client (Commit pk utxos)

data CollectComParams = CollectComParams
  { amount :: Value
  }
  deriving stock (Prelude.Eq, Prelude.Show, Generic)
  deriving anyclass (FromJSON, ToJSON, ToSchema, ToArgument)

type Schema =
  BlockchainActions
    .\/ Endpoint "commit" Value

contract ::
  (AsContractError e, SM.AsSMContractError e) =>
  Contract () Schema e ()
contract = commit
