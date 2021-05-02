{-# LANGUAGE UndecidableInstances #-}

module Hydra.Logic.SimpleHead where

import Cardano.Prelude hiding (State)

import Hydra.Ledger (LedgerState)

data Event tx
  = InitState (LedgerState tx)
  | NewTxFromClient tx
  | ReqTxFromPeer tx
  | AckTxFromPeer
  | ConfTxFromPeer
  | ReqSnFromPeer
  | AckSnFromPeer
  | ConfSnFromPeer

data State tx = State
  { confirmedLedger :: LedgerState tx
  , transactions :: Transactions
  , snapshots :: Snapshots
  }

deriving instance Eq (LedgerState tx) => Eq (State tx)
deriving instance Show (LedgerState tx) => Show (State tx)

mkState :: LedgerState tx -> State tx
mkState mkLedgerState = State mkLedgerState Transaction Snapshots

data Transactions = Transaction
  deriving (Eq, Show)
data Snapshots = Snapshots
  deriving (Eq, Show)

data Effect tx
  = MulticastReqTx tx
  | MulticastReqSn
  | MulticastConfTx
  | SendAckTx
  | Wait (State tx -> Maybe (State tx, [Effect tx]))

update :: State tx -> Event tx -> (State tx, [Effect tx])
update st = \case
  InitState l -> (st{confirmedLedger = l}, [])
  NewTxFromClient tx -> (st, [MulticastReqTx tx])
  ReqTxFromPeer _tx -> panic "TODO: apply requested transaction"
  _ -> panic "SimpleHead.TODO"
