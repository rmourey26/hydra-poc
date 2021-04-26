{-# LANGUAGE UndecidableInstances #-}

module Hydra.Logic where

import Cardano.Prelude
import Hydra.Ledger (LedgerState)

data Event tx
  = ClientEvent (ClientRequest tx)
  | NetworkEvent (HydraMessage tx)
  | OnChainEvent OnChainTx
  deriving (Eq, Show)

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
  = HSInit InitState
  | HSOpen (OpenState tx)
  | HSClosed ClosedState

deriving instance Eq (OpenState tx) => Eq (HeadState tx)
deriving instance Show (OpenState tx) => Show (HeadState tx)

data InitState = InitState deriving (Eq, Show)

data OpenState tx = OpenState
  { confirmedLedger :: LedgerState tx
  , transactions :: Transactions
  , snapshots :: Snapshots
  }

mkOpenState :: LedgerState tx -> OpenState tx
mkOpenState mkLedgerState = OpenState mkLedgerState Transaction Snapshots

deriving instance Eq (LedgerState tx) => Eq (OpenState tx)
deriving instance Show (LedgerState tx) => Show (OpenState tx)

data Transactions = Transaction deriving (Eq, Show)
data Snapshots = Snapshots deriving (Eq, Show)

data ClosedState = ClosedState deriving (Eq, Show)

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
createHeadState _ _ _ = HSInit InitState
