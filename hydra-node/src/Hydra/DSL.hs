{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TypeApplications #-}

module Hydra.DSL where

import Cardano.Prelude hiding (wait)

import Control.Exception.Safe (MonadThrow)
import Hydra.Logic (ClientInstruction (..), HydraMessage (..))
import Hydra.Node

--
-- DSL
--

class Hydra (impl :: Type) where
  type Ledger impl :: Type
  type Transaction impl :: Type
  type ValidationError impl :: Type

  initLedger :: Ledger impl
  validateTransaction :: Transaction impl -> Ledger impl -> Maybe (ValidationError impl)
  recordTransaction :: Transaction impl -> Ledger impl -> Ledger impl

data InitState = InitState

data OpenState impl = OpenState
  { confirmedLedger :: Ledger impl
  }

{- ORMOLU_DISABLE -}
data HydraPgrm impl err result where
  MulticastTransaction
    :: Transaction impl
    -> HydraPgrm impl err result
    -> HydraPgrm impl err result

  WaitForConfirmation
    :: Transaction impl
    -> HydraPgrm impl err result
    -> HydraPgrm impl err result

  NotifySeen
    :: Transaction impl
    -> HydraPgrm impl err result
    -> HydraPgrm impl err result

  WithState
    :: (OpenState impl -> HydraPgrm impl err (OpenState impl))
    -> HydraPgrm impl err result

  Yield
    :: result
    -> HydraPgrm impl err result

  FailWith
    :: err
    -> HydraPgrm impl err result
{- ORMOLU_ENABLE -}

--
-- Programs
--

-- - Main state transition from Init to Open
init :: forall impl. Hydra impl => OpenState impl
init =
  OpenState (initLedger @impl)

-- - Validate tx
-- - Needs feedback to client
-- - Modifies the ledger state
newTx ::
  forall impl.
  Hydra impl =>
  Transaction impl ->
  HydraPgrm impl (ValidationError impl) Void
newTx tx = WithState $ \st@OpenState{confirmedLedger} ->
  case validateTransaction @impl tx confirmedLedger of
    Nothing ->
      MulticastTransaction tx (Yield st)
    Just err ->
      FailWith err

--  - Wait for tx on the confirmed ledger
--  - Confirms tx directly (no intermediate 'seen' ledger)
reqTx ::
  forall impl.
  (Hydra impl) =>
  Transaction impl ->
  HydraPgrm impl (ValidationError impl) Void
reqTx tx = WithState $ \st@OpenState{confirmedLedger} ->
  case validateTransaction @impl tx confirmedLedger of
    Just err ->
      FailWith err
    Nothing ->
      WaitForConfirmation tx $
        NotifySeen tx $
          Yield $
            st
              { confirmedLedger = recordTransaction @impl tx confirmedLedger
              }

--
-- Interpreter
--

interpretHydraPgrm ::
  forall m err result impl.
  (MonadThrow m, MonadIO m, Hydra impl) =>
  HydraNetwork m ->
  OnChain m ->
  ClientSide m ->
  HydraHead (Transaction impl) m ->
  HydraPgrm impl err result ->
  m (Either err result)
interpretHydraPgrm hn oc cs hh = \case
  MulticastTransaction _tx continue -> do
    broadcast ReqTx
    interpretHydraPgrm hn oc cs hh continue
  WaitForConfirmation tx continue -> do
    wait tx
    interpretHydraPgrm hn oc cs hh continue
  NotifySeen _tx continue -> do
    showInstruction AcceptingTx
    interpretHydraPgrm hn oc cs hh continue
  WithState handler -> do
    modifyHeadState $ \st -> do
      interpretHydraPgrm hn oc cs hh (handler st) >>= \case
        Right st' -> (Right (), st')
        Left err -> (Left err, st)
  Yield result ->
    pure (Right result)
  FailWith err ->
    pure (Left err)
 where
  HydraNetwork{broadcast} = hn
  HydraHead{modifyHeadState} = hh
  ClientSide{showInstruction} = cs

  -- TODO :: Probably want to have that in a handle 'Scheduler{wait}' ?
  wait :: Transaction impl -> m ()
  wait _ = return ()
