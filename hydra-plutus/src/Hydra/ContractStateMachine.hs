{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-deferred-type-errors #-}
{-# OPTIONS_GHC -fno-specialize #-}

module Hydra.ContractStateMachine where

import Control.Monad (forever, guard, void)
import qualified Hydra.CommitContract as Commit
import Hydra.Contract.Types
import Ledger (Address, TxOut, Validator, scriptAddress, txOutAddress, txOutPubKey, txOutputs)
import qualified Ledger.Ada as Ada
import qualified Ledger.Typed.Scripts as Scripts
import qualified Ledger.Typed.Tx as Tx
import Playground.Contract
import Plutus.Contract
import Plutus.Contract.StateMachine (State (..), Void)
import qualified Plutus.Contract.StateMachine as SM
import Plutus.Contract.Util (loopM)
import qualified PlutusTx
import PlutusTx.Prelude

{-# INLINEABLE transition #-}
transition ::
  HeadParameters ->
  State HydraState ->
  HydraInput ->
  Maybe (SM.TxConstraints Void Void, State HydraState)
transition params s i = case (s, i) of
  (state@State{stateData = Initial}, Init _params) ->
    Just (mempty, state{stateData = Collecting})
  (state@State{stateData = Collecting}, CollectCom utxos) ->
    -- TODO actually add the utxos to the state ?
    if length (map txOutPubKey utxos) == length (verificationKeys params)
      then Just (mempty, state{stateData = Open openState})
      else Nothing
  (state@State{stateData = Open OpenState{eta, keyAggregate}}, Close xi) ->
    case close keyAggregate eta xi of
      Just{} -> Just (mempty, state{stateData = Closed})
      Nothing -> Nothing
  (_, _) -> Nothing
 where
  openState =
    OpenState{keyAggregate = MultisigPublicKey [], eta = Eta UTXO 0 []}

{-# INLINEABLE close #-}
close :: MultisigPublicKey -> Eta -> Xi -> Maybe Eta
close kAgg eta xi = do
  let (Xi u s sigma txs) = xi
  guard (all (verifyMultisignature kAgg) txs)
  guard (s == 0 || verifySnapshot kAgg u s sigma)
  let realU = if s == 0 then utxos eta else u
      mainchainTxs = map tx txs
  guard (isJust $ applyTransactions realU mainchainTxs)
  pure $ Eta realU s mainchainTxs

{-# INLINEABLE verifyMultisignature #-}
verifyMultisignature :: MultisigPublicKey -> TransactionObject -> Bool
verifyMultisignature kAgg TransactionObject{sigma, tx} =
  msAVerify kAgg (hash $ serialize tx) sigma

{-# INLINEABLE verifySnapshot #-}
verifySnapshot ::
  MultisigPublicKey -> UTXO -> Integer -> MultiSignature -> Bool
verifySnapshot kAgg u s sigma =
  msAVerify kAgg (hash $ serialize u <> serialize s) sigma

-- | This is only about folding the transactions onto a UTXO and no evaluation
-- whatsoever.
applyTransactions :: UTXO -> [Transaction] -> Maybe UTXO
applyTransactions u _ = Just u -- TODO

--
-- Primitives we need
--

serialize :: a -> ByteString
serialize = const "reuse plutus tx's isData stuff" -- TODO

hash :: ByteString -> ByteString
hash = const "hashed bytestring" -- TODO

msAVerify :: MultisigPublicKey -> ByteString -> MultiSignature -> Bool
msAVerify _ _ _ = True -- TODO

--
-- Boilerplate
--

{-# INLINEABLE machine #-}
machine :: HeadParameters -> SM.StateMachine HydraState HydraInput
machine params = SM.mkStateMachine (transition params) isFinal where isFinal _ = False

{-# INLINEABLE validatorSM #-}
validatorSM :: HeadParameters -> Scripts.ValidatorType (SM.StateMachine HydraState HydraInput)
validatorSM = SM.mkValidator . machine

{- ORMOLU_DISABLE -}
contractInstance
  :: HeadParameters -> Scripts.ScriptInstance (SM.StateMachine HydraState HydraInput)
contractInstance = Scripts.validatorParam @(SM.StateMachine HydraState HydraInput)
    $$(PlutusTx.compile [|| validatorSM ||])
    $$(PlutusTx.compile [|| wrap ||])
    where
        wrap = Scripts.wrapValidator @HydraState @HydraInput
{- ORMOLU_ENABLE -}

-- | The 'SM.StateMachineInstance' of the hydra state machine contract. It uses
-- the functions in 'PlutusTx.StateMachine'.
machineInstance :: HeadParameters -> SM.StateMachineInstance HydraState HydraInput
machineInstance params = SM.StateMachineInstance (machine params) (contractInstance params)

client :: HeadParameters -> SM.StateMachineClient HydraState HydraInput
client = SM.mkStateMachineClient . machineInstance

-- | The validator script of the contract.
contractValidator :: HeadParameters -> Validator
contractValidator = Scripts.validatorScript . contractInstance

-- | The address of the contract (the hash of its validator script)
contractAddress :: HeadParameters -> Address
contractAddress = Ledger.scriptAddress . contractValidator

setupEndpoint ::
  (AsContractError e, SM.AsSMContractError e) => HeadParameters -> Contract () Schema e ()
setupEndpoint params = do
  endpoint @"setup" @()
  logInfo @String $ "setupEndpoint"
  void $ SM.runInitialise (client params) Initial (Ada.lovelaceValueOf 1)

currentState ::
  (SM.AsSMContractError e) =>
  SM.StateMachineClient HydraState HydraInput ->
  Contract () Schema e (Maybe HydraState)
currentState cli =
  SM.getOnChainState cli >>= \case
    Just ((Tx.TypedScriptTxOut{Tx.tyTxOutData = s}, _), _) -> do
      pure (Just s)
    _ -> do
      pure Nothing

-- | Our mocked "init" endpoint
initEndpoint ::
  (AsContractError e, SM.AsSMContractError e) =>
  HeadParameters ->
  Contract () Schema e ()
initEndpoint params = do
  endpoint @"init" @()
  logInfo @String $ "initEndpoint"
  void $ SM.runStep (client params) input
 where
  input = Init params

-- | Our mocked "collectCom" endpoint
collectComEndpoint ::
  (AsContractError e, SM.AsSMContractError e) => HeadParameters -> Contract () Schema e ()
collectComEndpoint params = do
  endpoint @"collectCom" @()
  commitOuts <- loopM (waitAllCommits params (length $ verificationKeys params)) []
  -- consume the commits in a CollectCom
  void $ SM.runStep (client params) (CollectCom commitOuts)

-- | 'Collect' all commit transactions as they are posted.
-- We wait for every change request to the nu_com script (eg. the commit contract's script) address,
-- and collect the corresponding transactions' outputs.
-- Normally, we should make sure we do this before some timeout happens
waitAllCommits ::
  (AsContractError e) =>
  HeadParameters ->
  Integer ->
  [TxOut] ->
  Contract () Schema e (Either [TxOut] [TxOut])
waitAllCommits params numParties accUtxos = do
  s <- currentSlot
  let addr = Commit.contractAddress params
  logInfo @String $ "Waiting for change at " <> show addr <> " to happen, current slot is " <> show s
  AddressChangeResponse{acrTxns} <- addressChangeRequest AddressChangeRequest{acreqSlot = s, acreqAddress = addr}
  case acrTxns of
    [] -> pure (Left accUtxos) -- continue waiting
    txns -> do
      let scrUtxos = filter ((== addr) . txOutAddress) $ concatMap txOutputs txns
          acc = scrUtxos <> accUtxos
      logInfo @String $ "Collected UTXOs " <> show acc
      if length acc == numParties
        then pure (Right acc)
        else pure (Left acc)

-- | Our "close" endpoint to trigger a close
closeEndpoint ::
  (AsContractError e, SM.AsSMContractError e) =>
  HeadParameters ->
  Contract () Schema e ()
closeEndpoint params = do
  endpoint @"close" @()
  logInfo @String $ "closeEndpoint"
  void $ SM.runStep (client params) input
 where
  input = Close $ Xi UTXO 0 MultiSignature []

type Schema =
  BlockchainActions
    .\/ Endpoint "setup" ()
    .\/ Endpoint "init" ()
    .\/ Endpoint "collectCom" ()
    .\/ Endpoint "close" ()

contract ::
  (AsContractError e, SM.AsSMContractError e) =>
  HeadParameters ->
  Contract () Schema e ()
contract params = forever endpoints
 where
  endpoints =
    setupEndpoint params
      `select` initEndpoint params
      `select` collectComEndpoint params
      `select` closeEndpoint params
