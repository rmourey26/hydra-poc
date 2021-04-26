{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TypeApplications #-}

-- | Top-level module to run a single Hydra node.
module Hydra.Node where

import Cardano.Prelude
import Control.Concurrent.STM (
  newTQueueIO,
  readTQueue,
  writeTQueue,
 )
import Control.Exception.Safe (MonadThrow)
import Hydra.Ledger
import Hydra.Logic (
  ClientInstruction (..),
  ClosedState (..),
  Event (NetworkEvent),
  HeadState (..),
  HydraMessage (AckSn, AckTx, ConfSn, ConfTx, MsgReqTx, ReqSn),
  InitState,
  OnChainTx (..),
  OpenState (confirmedLedger),
  ReqTx (ReqTx),
  mkOpenState,
 )
import System.Console.Repline (CompleterStyle (Word0), ExitDecision (Exit), evalRepl)

--
-- General handlers of client commands or events.
--

-- | Monadic interface around 'Hydra.Logic.update'.
handleNextEvent ::
  Show tx =>
  MonadThrow m =>
  HydraNetwork tx m ->
  OnChain m ->
  ClientSide m ->
  HydraHead tx m ->
  Ledger tx ->
  Event tx ->
  m ()
handleNextEvent hn _oc _cs hh ledger = \case
  NetworkEvent (MsgReqTx reqTx) ->
    onReqTx ledger hn hh reqTx >>= \case
      ReqTxNotInOpenState -> panic "received reqTx while not in open state?"
      ReqTxInvalidTransaction -> panic "how are invalid txs handled? simply log?"
      ReqTxSuccess -> pure ()
  e -> panic $ "unhandled event: " <> show e

data NotInInitState = NotInInitState deriving (Eq, Show)

withInitState_ :: Applicative m => HydraHead tx m -> (InitState -> m (HeadState tx)) -> m (Maybe NotInInitState)
withInitState_ HydraHead{modifyHeadStateM} action =
  modifyHeadStateM $ \case
    HSInit is -> (,Nothing) <$> action is
    s -> pure (s, Just NotInInitState)

init ::
  MonadThrow m =>
  OnChain m ->
  Ledger tx ->
  ClientSide m ->
  InitState ->
  m (OpenState tx)
init OnChain{postTx} Ledger{initLedgerState} ClientSide{showInstruction} _st = do
  postTx InitTx
  showInstruction AcceptingTx
  pure $ mkOpenState initLedgerState

data NotInOpenState = NotInOpenState deriving (Eq, Show)

fromOpenState :: Applicative m => HydraHead tx m -> (OpenState tx -> m (HeadState tx, a)) -> m (Either NotInOpenState a)
fromOpenState HydraHead{modifyHeadStateM} action =
  modifyHeadStateM $ \case
    HSOpen is -> second Right <$> action is
    s -> pure (s, Left NotInOpenState)

fromOpenState_ :: Applicative m => HydraHead tx m -> (OpenState tx -> m (HeadState tx)) -> m (Maybe NotInOpenState)
fromOpenState_ HydraHead{modifyHeadStateM} action =
  modifyHeadStateM $ \case
    HSOpen is -> (,Nothing) <$> action is
    s -> pure (s, Just NotInOpenState)

toOpenState :: Functor m => m (OpenState tx, a) -> m (HeadState tx, a)
toOpenState = fmap (first HSOpen)

-- NOTE(SN): does not modify OpenState right now, but it could
newTx ::
  Monad m =>
  Ledger tx ->
  HydraNetwork tx m ->
  tx ->
  OpenState tx ->
  m (OpenState tx, ValidationResult)
newTx ledger HydraNetwork{broadcast} tx st =
  -- TODO(SN): distinguish between 'valid-tx' and 'canApply' and multiple error results
  case canApply ledger (confirmedLedger st) tx of
    Valid -> do
      broadcast (MsgReqTx $ ReqTx tx) $> (st, Valid)
    invalid -> pure (st, invalid)

data InvalidTransaction = InvalidTransaction
  deriving (Eq, Show)

-- TODO(SN): remove this again
data OnReqTxResult
  = ReqTxNotInOpenState
  | ReqTxInvalidTransaction
  | ReqTxSuccess
  deriving (Eq, Show)

-- NOTE(SN): does not modify OpenState right now, but it could
onReqTx ::
  Monad m =>
  Ledger tx ->
  HydraNetwork tx m ->
  HydraHead tx m ->
  ReqTx tx ->
  m OnReqTxResult
onReqTx Ledger{canApply, applyTx} HydraNetwork{broadcast} hh@HydraHead{waitUntil} (ReqTx tx) = do
  waitUntil canApplyToConfirmed $ \_s -> do
    withHeadStateT hh $ do
      s <- get
      -- TODO(SN): MonadError / ExceptT?
      case confLedger s of
        Nothing -> pure ReqTxNotInOpenState
        Just conf -> do
          case applyTx conf tx of
            Left err -> panic "smell: \"this should not happend\""
            Right conf' -> do
              modifyOpen (\os -> os{confirmedLedger = conf'})
              lift $ broadcast ConfTx -- NOTE(SN): Directly confirm
              pure ReqTxSuccess
 where
  canApplyToConfirmed s =
    Just Valid == (flip canApply tx <$> confLedger s)

  confLedger = \case
    HSOpen os -> Just $ confirmedLedger os
    _ -> Nothing

  modifyOpen f = modify $ \case
    HSOpen os -> HSOpen (f os)
    s -> s

close ::
  MonadThrow m =>
  OnChain m ->
  OpenState tx ->
  m ClosedState
close OnChain{postTx} _os = do
  postTx CloseTx
  pure ClosedState

--
-- Some general event queue from which the Hydra head is "fed"
--

-- | The single, required queue in the system from which a hydra head is "fed".
-- NOTE(SN): this probably should be bounded and include proper logging
-- NOTE(SN): handle pattern, but likely not required as there is no need for an
-- alternative implementation
data EventQueue m e = EventQueue
  { putEvent :: e -> m ()
  , nextEvent :: m e
  }

createEventQueue :: IO (EventQueue IO e)
createEventQueue = do
  q <- newTQueueIO
  pure
    EventQueue
      { putEvent = atomically . writeTQueue q
      , nextEvent = atomically $ readTQueue q
      }

--
-- HydraHead handle to manage a single hydra head state concurrently
--

-- | Handle to access and modify a Hydra Head's state.
data HydraHead tx m = HydraHead
  { modifyHeadStateM :: forall a. Applicative m => (HeadState tx -> m (HeadState tx, a)) -> m a
  , -- | Wait until a condition on the 'HeadState' to becomes 'True' and invoke
    -- the given continuation.
    waitUntil :: forall a. (HeadState tx -> Bool) -> (HeadState tx -> m a) -> m a
  }

modifyHeadState :: Applicative m => HydraHead tx m -> (HeadState tx -> (HeadState tx, a)) -> m a
modifyHeadState hh f = modifyHeadStateM hh $ \s -> pure $ f s

withHeadStateT :: Applicative m => HydraHead tx m -> StateT (HeadState tx) m a -> m a
withHeadStateT hh (StateT action) = modifyHeadStateM hh (fmap swap . action)

queryHeadState :: Applicative m => HydraHead tx m -> m (HeadState tx)
queryHeadState = (`modifyHeadState` \s -> (s, s))

putState :: Applicative m => HydraHead tx m -> HeadState tx -> m ()
putState hh new = modifyHeadState hh $ const (new, ())

createHydraHead :: HeadState tx -> IO (HydraHead tx IO)
createHydraHead initialState = do
  mv <- newMVar initialState
  pure
    HydraHead
      { modifyHeadStateM = modifyMVar mv
      , waitUntil = panic "not implemented"
      }

--
-- HydraNetwork handle to abstract over network access
--

-- | Handle to interface with the hydra network and send messages "off chain".
newtype HydraNetwork tx m = HydraNetwork
  { -- | Send a 'HydraMessage' to the whole hydra network.
    broadcast :: HydraMessage tx -> m ()
  }

-- | Connects to a configured set of peers and sets up the whole network stack.
createHydraNetwork :: Show tx => (HydraMessage tx -> IO ()) -> IO (HydraNetwork tx IO)
createHydraNetwork onMsg = do
  -- NOTE(SN): obviously we should connect to a known set of peers here and do
  -- really broadcast messages to them, as well as call onMsg on incoming msgs
  pure HydraNetwork{broadcast = simulatedBroadcast}
 where
  simulatedBroadcast msg = do
    putStrLn @Text $ "[Network] should broadcast " <> show msg
    onMsg msg -- NOTE(SN): assume broadcast is stupid and we see our own messages
    let ma = case msg of
          MsgReqTx _ -> Just AckTx
          AckTx -> Just ConfTx
          ConfTx -> Nothing
          ReqSn -> Just AckSn
          AckSn -> Just ConfSn
          ConfSn -> Nothing
    case ma of
      Just answer -> do
        threadDelay 100000
        putStrLn @Text $ "[Network] simulating answer " <> show answer
        onMsg answer
      Nothing -> pure ()

--
-- OnChain handle to abstract over chain access
--

data ChainError = ChainError
  deriving (Exception, Show)

-- | Handle to interface with the main chain network
newtype OnChain m = OnChain
  { -- | Construct and send a transaction to the main chain corresponding to the
    -- given 'OnChainTx' event.
    -- Does at least throw 'ChainError'.
    postTx :: MonadThrow m => OnChainTx -> m ()
  }

-- | Connects to a cardano node and sets up things in order to be able to
-- construct actual transactions using 'OnChainTx' and send them on 'postTx'.
createChainClient :: (OnChainTx -> IO ()) -> IO (OnChain IO)
createChainClient onChainTx = do
  -- NOTE(SN): obviously we should construct and send transactions, e.g. using
  -- plutus instead, as well as call onChainTx when the according tx is observed
  -- on chain
  pure OnChain{postTx = simulatedPostTx}
 where
  simulatedPostTx tx = do
    putStrLn @Text $ "[OnChain] should post tx for " <> show tx
    let ma = case tx of
          InitTx -> Nothing
          CommitTx -> Just CollectComTx -- simulate other peer collecting
          CollectComTx -> Nothing
          CloseTx -> Just ContestTx -- simulate other peer contesting
          ContestTx -> Nothing
          FanoutTx -> Nothing
    case ma of
      Just answer -> void . async $ do
        threadDelay 1000000
        putStrLn @Text $ "[OnChain] simulating  " <> show answer
        onChainTx answer
      Nothing -> pure ()

--
-- ClientSide handle to abstract over the client side.. duh.
--

newtype ClientSide m = ClientSide
  { showInstruction :: ClientInstruction -> m ()
  }

-- | A simple command line based read-eval-process-loop (REPL) to have a chat
-- with the Hydra node.
--
-- NOTE(SN): This clashes a bit when other parts of the node do log things, but
-- spreading \r and >>> all over the place is likely not what we want
createClientSideRepl ::
  OnChain IO ->
  HydraHead tx IO ->
  HydraNetwork tx IO ->
  Ledger tx ->
  (FilePath -> IO tx) ->
  IO (ClientSide IO)
createClientSideRepl oc hh hn ledger loadTx = do
  link =<< async runRepl
  pure cs
 where
  prettyInstruction = \case
    ReadyToCommit -> "Head initialized, commit funds to it using 'commit'"
    AcceptingTx -> "Head is open, now feed the hydra with your 'newtx'"

  runRepl = evalRepl (const $ pure prompt) replCommand [] Nothing Nothing (Word0 replComplete) replInit (pure Exit)

  cs =
    ClientSide
      { showInstruction = \ins -> putStrLn @Text $ "[ClientSide] " <> prettyInstruction ins
      }

  prompt = ">>> "

  -- TODO(SN): avoid redundancy
  commands = ["init", "commit", "newtx", "close", "contest"]

  replCommand c
    | c == "init" =
      liftIO $
        withInitState_ hh (fmap HSOpen . init oc ledger cs) >>= \case
          Just NotInInitState -> putStrLn @Text $ "You dummy, can't init now"
          Nothing -> pure ()
    | c == "close" =
      liftIO $
        fromOpenState_ hh (fmap HSClosed . close oc) >>= \case
          Just NotInOpenState -> putText "You dummy.. can't close now"
          Nothing -> pure ()
    -- c == "commit" =
    | c == "newtx" = liftIO $ do
      tx <- loadTx "hardcoded/file/path"
      fromOpenState hh (toOpenState . newTx ledger hn tx) >>= \case
        Left _ -> putText "You dummy, not accepting txs now"
        Right (Invalid _) -> putText "Transaction invalid, not going to accept it"
        Right Valid -> putText "Tx accepted, do you have more?"

    -- c == "contest" =
    | otherwise = liftIO $ putStrLn @Text $ "Unknown command, use any of: " <> show commands

  replComplete n = pure $ filter (n `isPrefixOf`) commands

  replInit = liftIO $ putStrLn @Text "Welcome to the Hydra Node REPL, you can even use tab completion! (Ctrl+D to exit)"
