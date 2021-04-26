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
  InitState (..),
  OnChainTx (..),
  OpenState (..),
  ReqTx (..),
  mkOpenState,
 )
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
spec = describe "Hydra Node business logic" $ do
  describe "init" $ do
    it "does transition to open state" $ do
      init (expectOnChain InitTx) mockLedger (expectClientSide AcceptingTx) InitState
        `shouldReturn` mkOpenState ()

  describe "newTx" $ do
    it "does send transactions received from client onto the network" $ do
      (n, queryNetworkMsgs) <- recordNetwork
      (st, res) <- newTx mockLedger n ValidTx (mkOpenState ())
      st `shouldBe` mkOpenState ()
      res `shouldBe` Valid
      queryNetworkMsgs `shouldReturn` [MsgReqTx $ ReqTx ValidTx]

    it "does not forward invalid transactions received from client" $ do
      (st, res) <- newTx mockLedger mockNetwork InvalidTx (mkOpenState ())
      st `shouldBe` mkOpenState ()
      res `shouldBe` Invalid ValidationError

  describe "onReqTx" $ do
    it "does send ackTx on a valid reqTx transaction" $ do
      (n, queryNetworkMsgs) <- recordNetwork
      (st, res) <- onReqTx mockLedger n (ReqTx ValidTx) (mkOpenState ())
      st `shouldBe` mkOpenState ()
      res `shouldBe` Nothing
      queryNetworkMsgs `shouldReturn` [AckTx]

    it "does nothing with an invalid reqTx transaction" $ do
      (st, res) <- onReqTx mockLedger mockNetwork (ReqTx InvalidTx) (mkOpenState ())
      st `shouldBe` mkOpenState ()
      res `shouldBe` Just InvalidTransaction

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
isOpen HSOpen{} = True
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
