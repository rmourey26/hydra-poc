{-# LANGUAGE UndecidableInstances #-}

module Hydra.Logic where

import Cardano.Prelude

import Hydra.Ledger (Ledger (Ledger, canApply), ValidationError, ValidationResult (Invalid, Valid))
import qualified Hydra.Logic.SimpleHead as SimpleHead

data Event tx
  = ClientEvent (ClientRequest tx)
  | NetworkEvent (HydraMessage tx)
  | OnChainEvent OnChainTx
  deriving (Eq, Show)

data Effect tx
  = ClientEffect ClientInstruction
  | NetworkEffect (HydraMessage tx)
  | OnChainEffect OnChainTx
  | -- | Wait effect should be interpreted as a non-blocking interruption which
    -- retries on every state changes until the continuation returns Just{}.
    Wait (HeadState tx -> Maybe (HeadState tx, [Effect tx]))
  | ErrorEffect (LogicError tx) -- NOTE(SN): this feels weird, maybe an Either on the 'update' fits better

data ClientRequest tx
  = Init
  | Commit
  | NewTx tx
  | Close
  | Contest
  deriving (Eq, Show)

data ClientInstruction
  = ReadyToCommit
  | AcceptingTx
  deriving (Eq, Show)

data HydraMessage tx
  = MsgReqTx (ReqTx tx)
  | AckTx
  | ConfTx
  | ReqSn
  | AckSn
  | ConfSn
  deriving (Eq, Show)

newtype ReqTx tx = ReqTx tx deriving (Eq, Show)

data OnChainTx
  = InitTx
  | CommitTx
  | CollectComTx
  | CloseTx
  | ContestTx
  | FanoutTx
  deriving (Eq, Show)

data HeadState tx
  = InitState
  | OpenState (SimpleHead.State tx)
  | ClosedState

deriving instance Eq (SimpleHead.State tx) => Eq (HeadState tx)
deriving instance Show (SimpleHead.State tx) => Show (HeadState tx)

-- | Verification used to authenticate main chain transactions that are
-- restricted to members of the Head protocol instance, i.e. the commit
-- transaction. This key is named k_i in the paper and for Cardano, this is
-- currently a Ed25519 verification key
data OnChainVerificationKey

-- | Verification key to the signing key used for signing / acking transactions
-- off chain. This key is named K_i in the paper and can be aggregated with
-- other party member's 'HydraVerificationKey' to K_agg.
data HydraVerificationKey

-- | Identifes a party in a Hydra head.
type Party = (OnChainVerificationKey, HydraVerificationKey)

-- | Contains at least the contestation period and other things.
data HeadParameters = HeadParameters

-- | Decides when, how often and who is in charge of creating snapshots.
data SnapshotStrategy = SnapshotStrategy

-- | Assume: We know the party members and their verification keys. These need
-- to be exchanged somehow, eventually.
createHeadState :: [Party] -> HeadParameters -> SnapshotStrategy -> HeadState tx
createHeadState _ _ _ = InitState

data LogicError tx
  = InvalidEvent (Event tx) (HeadState tx)
  | InvalidState (HeadState tx)
  | LedgerError ValidationError

deriving instance (Eq (HeadState tx), Eq (Event tx)) => Eq (LogicError tx)
deriving instance (Show (HeadState tx), Show (Event tx)) => Show (LogicError tx)

-- | The heart of the Hydra head logic, a handler of all kinds of 'Event' in the
-- Hydra head. This may also be split into multiple handlers, i.e. one for hydra
-- network events, one for client events and one for main chain events, or by
-- sub-'State'.
update :: Ledger tx -> HeadState tx -> Event tx -> (HeadState tx, Either (LogicError tx) [Effect tx])
update Ledger{canApply} st ev = case (st, ev) of
  _ -> (st, Right [ErrorEffect $ InvalidEvent ev st])
