{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TypeApplications #-}

module Hydra.DSL where

import Cardano.Prelude hiding (wait)

import Control.Exception.Safe (MonadThrow)
import Control.Monad.Freer
import Hydra.Logic (ClientInstruction (..), HeadState (..), HydraMessage (..))
import Hydra.Node

--
-- DSL
--

class Hydra (impl :: Type) where
  type Ledger impl :: Type
  type Transaction impl :: Type
  type ValidationError impl :: Type

  initLedger ::
    Ledger impl
  validateTransaction ::
    Transaction impl ->
    Ledger impl ->
    Maybe (ValidationError impl)
  recordTransaction ::
    Transaction impl ->
    Ledger impl ->
    Ledger impl

{- ORMOLU_DISABLE -}
data HydraPgrm impl err result where
  MulticastTransaction
    :: Transaction impl
    -> HydraPgrm impl err ()

  WaitForConfirmation
    :: Transaction impl
    -> HydraPgrm impl err ()

  NotifySeen
    :: Transaction impl
    -> HydraPgrm impl err ()

  WithLedger
    :: forall impl err result. (Ledger impl -> (result, Ledger impl))
    -> HydraPgrm impl err result

  FailWith
    :: err
    -> HydraPgrm impl err ()
{- ORMOLU_ENABLE -}

multicastTransaction ::
  forall impl err eff.
  (Member (HydraPgrm impl err) eff) =>
  Transaction impl ->
  Eff eff ()
multicastTransaction =
  send . MulticastTransaction @impl @err

waitForConfirmation ::
  forall impl err eff.
  (Member (HydraPgrm impl err) eff) =>
  Transaction impl ->
  Eff eff ()
waitForConfirmation =
  send . WaitForConfirmation @impl @err

notifySeen ::
  forall impl err eff.
  (Member (HydraPgrm impl err) eff) =>
  Transaction impl ->
  Eff eff ()
notifySeen =
  send . NotifySeen @impl @err

withLedger ::
  forall impl err result eff.
  (Member (HydraPgrm impl err) eff) =>
  (Ledger impl -> (result, Ledger impl)) ->
  Eff eff result
withLedger = send . WithLedger @impl @err @result

failWith ::
  forall impl err eff.
  (Member (HydraPgrm impl err) eff) =>
  err ->
  Eff eff ()
failWith = send . FailWith @err @impl

--
-- Programs
--

-- - Main state transition from Init to Open
init :: forall impl. Hydra impl => Ledger impl
init =
  initLedger @impl

-- - Validate tx
-- - Needs feedback to client
-- - Modifies the ledger state
newTx ::
  forall impl err eff.
  (Hydra impl, err ~ ValidationError impl, Member (HydraPgrm impl err) eff) =>
  Transaction impl ->
  Eff eff ()
newTx tx = do
  result <- withLedger @impl @err $ \ledger ->
    ( validateTransaction @impl tx ledger
    , ledger
    )
  maybe (multicastTransaction @impl @err tx) (failWith @impl @err) result

--  - Wait for tx on the confirmed ledger
--  - Confirms tx directly (no intermediate 'seen' ledger)
reqTx ::
  forall impl err eff.
  (Hydra impl, err ~ ValidationError impl, Member (HydraPgrm impl err) eff) =>
  Transaction impl ->
  Eff eff ()
reqTx tx = do
  result <- withLedger @impl @err $ \ledger ->
    ( validateTransaction @impl tx ledger
    , ledger
    )
  maybe (waitForConfirmation @impl @err tx) (failWith @impl @err) result
  notifySeen @impl @err tx
  withLedger @impl @err $ \ledger ->
    ( ()
    , recordTransaction @impl tx ledger
    )

--
-- Interpreter
--

interpretHydraPgrm ::
  forall m a impl err result eff.
  (MonadThrow m, MonadIO m, Exception err) =>
  (Ledger impl ~ HeadState (Transaction impl)) =>
  (Member m eff) =>
  HydraNetwork m ->
  OnChain m ->
  ClientSide m ->
  HydraHead (Transaction impl) m ->
  HydraPgrm impl err result ->
  (result -> Eff eff a) ->
  Eff eff a
interpretHydraPgrm hn _oc cs hh pgrm ret =
  ret
    =<< case pgrm of
      MulticastTransaction _tx -> do
        send $ broadcast ReqTx
      WaitForConfirmation tx -> do
        send $ wait tx
      NotifySeen _tx -> do
        send $ showInstruction AcceptingTx
      WithLedger fn -> do
        send $ modifyHeadState fn
      FailWith err ->
        send $ throwIO @m err
 where
  HydraNetwork{broadcast} = hn
  HydraHead{modifyHeadState} = hh
  ClientSide{showInstruction} = cs

  -- TODO :: Probably want to have that in a handle 'Scheduler{wait}' ?
  wait :: Transaction impl -> m ()
  wait _ = return ()
