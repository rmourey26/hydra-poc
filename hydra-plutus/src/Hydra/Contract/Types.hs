{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -fno-specialize #-}

module Hydra.Contract.Types where

import Data.Aeson (FromJSON, ToJSON)
import Ledger (
  CurrencySymbol,
  Datum (Datum),
  DatumHash,
  PubKeyHash,
  TxOut,
  datumHash,
  txOutPubKey,
 )
import PlutusPrelude (Generic)
import qualified PlutusTx
import PlutusTx.Prelude
import qualified Prelude

data HydraState
  = Initial
  | Collecting
  | Open OpenState
  | Closed
  deriving stock (Prelude.Eq, Show)

data HydraInput
  = Init HeadParameters
  | CollectCom [TxOut]
  | Close Xi -- Pi
  deriving (Show, Generic)

data OpenState = OpenState
  { keyAggregate :: MultisigPublicKey
  , eta :: Eta
  -- hMT :: MerkleTreeRoot,
  -- numberOfMembers :: Integer,
  -- contestationPeriod :: Integer
  }
  deriving (Prelude.Eq, Show)

data MultisigPublicKey = MultisigPublicKey [VerificationKey]
  deriving (Prelude.Eq, Show, Generic)

newtype VerificationKey = VerificationKey
  { unverificationKey :: ByteString
  }
  deriving (Prelude.Eq, Show)

data Eta = Eta
  { utxos :: UTXO -- u
  , snapshotNumber :: Integer -- s
  , transactions :: [Transaction] -- morally a Set
  }
  deriving (Prelude.Eq, Show)

data UTXO = UTXO
  deriving (Prelude.Eq, Show, Generic, ToJSON, FromJSON)

-- | The transaction as handled in the hydra head, i.e. the tx which we have put
-- into Hydra. According to isomorphism property of Hydra, it could also have
-- been put on the main chain.
data Transaction = Transaction
  deriving (Prelude.Eq, Show)

data TransactionObject = TransactionObject
  { sigma :: MultiSignature
  , tx :: Transaction
  }
  deriving (Show)

data MultiSignature = MultiSignature
  deriving (Show)

data MerkleTreeRoot = MerkleTreeRoot

data Pi

data Xi = Xi
  { xiUtxos :: UTXO
  , xiSnapshotNumber :: Integer
  , signatures :: MultiSignature
  , confirmedTransactions :: [TransactionObject] -- morally a Set
  }
  deriving (Show)

data HeadParameters = HeadParameters
  { verificationKeys :: [PubKeyHash]
  , currencyId :: CurrencySymbol
  }
  deriving (Prelude.Eq, Show)

isIn :: Foldable t => TxOut -> t PubKeyHash -> Bool
isIn txout pubkeys =
  maybe False (`elem` pubkeys) $ txOutPubKey txout

toDatumHash :: PlutusTx.IsData a => a -> DatumHash
toDatumHash = datumHash . Datum . PlutusTx.toData

PlutusTx.makeLift ''HydraState
PlutusTx.makeLift ''HydraInput
PlutusTx.makeLift ''OpenState
PlutusTx.makeLift ''MultisigPublicKey
PlutusTx.makeLift ''VerificationKey
PlutusTx.makeLift ''Eta
PlutusTx.makeLift ''MerkleTreeRoot
PlutusTx.makeLift ''TransactionObject
PlutusTx.makeLift ''Transaction
PlutusTx.makeLift ''UTXO
PlutusTx.makeLift ''MultiSignature
PlutusTx.makeLift ''Xi
PlutusTx.makeLift ''HeadParameters

PlutusTx.unstableMakeIsData ''HydraState
PlutusTx.unstableMakeIsData ''HydraInput
PlutusTx.unstableMakeIsData ''OpenState
PlutusTx.unstableMakeIsData ''MultisigPublicKey
PlutusTx.unstableMakeIsData ''VerificationKey
PlutusTx.unstableMakeIsData ''Eta
PlutusTx.unstableMakeIsData ''MerkleTreeRoot
PlutusTx.unstableMakeIsData ''TransactionObject
PlutusTx.unstableMakeIsData ''Transaction
PlutusTx.unstableMakeIsData ''UTXO
PlutusTx.unstableMakeIsData ''MultiSignature
PlutusTx.unstableMakeIsData ''Xi
PlutusTx.unstableMakeIsData ''HeadParameters
