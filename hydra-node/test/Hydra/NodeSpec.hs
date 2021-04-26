{-# OPTIONS_GHC -Wno-deferred-type-errors #-}

module Hydra.NodeSpec where

import Cardano.Prelude
import Hydra.Node

import Data.IORef (atomicModifyIORef', newIORef, readIORef)
import Data.String (String)
import Hydra.Ledger (Ledger (Ledger, canApply, initLedgerState), LedgerState, ValidationError (ValidationError), ValidationResult (..))
import Hydra.Logic (
  ClientInstruction (..),
  HeadState (..),
  HydraMessage (..),
  OnChainTx (..),
  ReqTx (..),
 )
import qualified Hydra.Logic.SimpleHead as SimpleHead
import Test.Hspec (
  Expectation,
  Spec,
  describe,
  expectationFailure,
  it,
  shouldBe,
  shouldNotBe,
  shouldReturn,
  shouldSatisfy,
 )

spec :: Spec
spec = describe "Hydra Node" $ do
  it "does something" $ do
    hh <- createHydraHead InitState mockLedger
    res <- init (expectOnChain InitTx) hh (expectClientSide AcceptingTx)
    res `shouldBe` Nothing
    queryHeadState hh >>= flip shouldSatisfy isOpen

  it "does send transactions received from client onto the network" $ do
    hh <- createHydraHead (OpenState $ SimpleHead.mkState ()) mockLedger
    (n, queryNetworkMsgs) <- recordNetwork
    newTx hh n ValidTx
      `shouldReturn` Right Valid
    queryHeadState hh >>= flip shouldSatisfy isOpen
    queryNetworkMsgs `shouldReturn` [MsgReqTx $ ReqTx ValidTx]

  it "does not forward invalid transactions received from client" $ do
    hh <- createHydraHead (OpenState $ SimpleHead.mkState ()) mockLedger
    newTx hh mockNetwork InvalidTx
      `shouldReturn` Right (Invalid ValidationError)
    queryHeadState hh >>= flip shouldSatisfy isOpen

  describe "handleReqTx" $ do
    it "does send ackTx on a valid reqTx transaction" $ do
      hh <- createHydraHead (OpenState $ SimpleHead.mkState ()) mockLedger
      handleReqTx hh (expectNetwork AckTx) (ReqTx ValidTx)
        `shouldReturn` (Right $ Right ())
      queryHeadState hh >>= flip shouldSatisfy isOpen

    it "does nothing with an invalid reqTx transaction" $ do
      hh <- createHydraHead (OpenState $ SimpleHead.mkState ()) mockLedger
      handleReqTx hh mockNetwork (ReqTx InvalidTx)
        `shouldReturn` Left InvalidTransaction
      queryHeadState hh >>= flip shouldSatisfy isOpen

data MockTx = ValidTx | InvalidTx
  deriving (Eq, Show)

type instance LedgerState MockTx = ()

mockLedger :: Ledger MockTx
mockLedger =
  Ledger
    { canApply = \st tx -> case st `seq` tx of
        ValidTx -> Valid
        InvalidTx -> Invalid ValidationError
    , initLedgerState = ()
    }

isOpen :: HeadState tx -> Bool
isOpen OpenState{} = True
isOpen _ = False

recordNetwork :: IO (HydraNetwork tx IO, IO [HydraMessage tx])
recordNetwork = do
  ref <- newIORef []
  pure (HydraNetwork{broadcast = recordMsg ref}, queryMsgs ref)
 where
  recordMsg ref x = atomicModifyIORef' ref $ \old -> (old <> [x], ())

  queryMsgs = readIORef

mockNetwork :: Show tx => HydraNetwork tx IO
mockNetwork =
  HydraNetwork
    { broadcast = \x -> shouldNotBeCalled $ "broadcast(" <> show x <> ")"
    }

-- TODO(SN): provide a means to check whether it was really broadcast
expectNetwork :: (Eq tx, Show tx) => HydraMessage tx -> HydraNetwork tx IO
expectNetwork expected =
  HydraNetwork
    { broadcast = (`shouldBe` expected)
    }

mockChain :: OnChain IO
mockChain =
  OnChain
    { postTx = \x -> shouldNotBeCalled $ "postTx(" <> show x <> ")"
    }

expectOnChain :: OnChainTx -> OnChain IO
expectOnChain expected =
  OnChain
    { postTx = (`shouldBe` expected)
    }

mockClientSide :: ClientSide IO
mockClientSide =
  ClientSide
    { showInstruction = \x -> shouldNotBeCalled $ "showInstruction(" <> show x <> ")"
    }

expectClientSide :: ClientInstruction -> ClientSide IO
expectClientSide ins =
  ClientSide
    { showInstruction = (`shouldBe` ins)
    }

shouldNotBeCalled :: String -> Expectation
shouldNotBeCalled name = expectationFailure $ name <> " should not have been called"
