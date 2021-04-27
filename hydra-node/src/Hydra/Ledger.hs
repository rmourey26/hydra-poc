module Hydra.Ledger where

import Cardano.Prelude hiding (undefined)

import Cardano.Ledger.Core (EraRule)
import Cardano.Slotting.EpochInfo (fixedSizeEpochInfo)
import qualified Control.State.Transition.Extended as SmallSteps
import Data.Default (Default, def)
import Shelley.Spec.Ledger.API (Globals (..), Network (Testnet))
import qualified Shelley.Spec.Ledger.API as Ledger
import Shelley.Spec.Ledger.BaseTypes (UnitInterval, mkActiveSlotCoeff, mkUnitInterval)
import Shelley.Spec.Ledger.Slot (EpochSize (EpochSize))

type family LedgerState tx

-- | The concrete type of UTXOs for a given ledger type
type family Utxo tx

data Ledger tx = Ledger
  { canApply :: LedgerState tx -> tx -> ValidationResult
  , -- |Initialises a `Ledger` with given `Utxo`
    -- The reste of the state is set to `def`ault values.
    mkLedger :: Utxo tx -> LedgerState tx
  , initLedgerState :: LedgerState tx
  }

-- | Either valid or an error which we get from the ledger-specs tx validation.
data ValidationResult
  = Valid
  | Invalid ValidationError
  deriving (Eq, Show)

data ValidationError = ValidationError deriving (Eq, Show)

--
-- Cardano ledger
--

type instance LedgerState (Ledger.Tx era) = Ledger.LedgerState era

type instance Utxo (Ledger.Tx era) = Ledger.UTxO era

cardanoLedger ::
  Default (SmallSteps.State (EraRule "PPUP" era)) =>
  Ledger.ApplyTx era =>
  Ledger.LedgersEnv era ->
  Ledger.UTxO era ->
  Ledger (Ledger.Tx era)
cardanoLedger env utxos =
  Ledger
    { canApply = validateTx env
    , mkLedger
    , initLedgerState = mkLedger utxos
    }
 where
  mkLedger u = Ledger.LedgerState (Ledger.UTxOState u (Ledger.Coin 0) (Ledger.Coin 0) def) (Ledger.DPState def def)

validateTx ::
  Ledger.ApplyTx era =>
  Ledger.LedgersEnv era ->
  Ledger.LedgerState era ->
  Ledger.Tx era ->
  ValidationResult
validateTx env ls tx =
  either (Invalid . toValidationError) (const Valid) $
    Ledger.applyTxsTransition globals env (pure tx) ls
 where
  -- toValidationError :: ApplyTxError -> ValidationError
  toValidationError = const ValidationError

--
-- From: shelley/chain-and-ledger/shelley-spec-ledger-test/src/Test/Shelley/Spec/Ledger/Utils.hs
--

-- TODO(SN): not hard-code these obviously
globals :: Globals
globals =
  Globals
    { epochInfo = fixedSizeEpochInfo $ EpochSize 100
    , slotsPerKESPeriod = 20
    , stabilityWindow = 33
    , randomnessStabilisationWindow = 33
    , securityParameter = 10
    , maxKESEvo = 10
    , quorum = 5
    , maxMajorPV = 1000
    , maxLovelaceSupply = 45 * 1000 * 1000 * 1000 * 1000 * 1000
    , activeSlotCoeff = mkActiveSlotCoeff . unsafeMkUnitInterval $ 0.9
    , networkId = Testnet
    }

-- | You vouch that argument is in [0; 1].
unsafeMkUnitInterval :: Ratio Word64 -> UnitInterval
unsafeMkUnitInterval r =
  fromMaybe (panic "could not construct unit interval") $ mkUnitInterval r
