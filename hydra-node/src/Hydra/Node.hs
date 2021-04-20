{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-deferred-type-errors #-}

-- | Top-level module to run a single Hydra node.
module Hydra.Node where

import Cardano.Prelude
import Control.Concurrent.STM (
  newTQueueIO,
  newTVarIO,
  readTQueue,
  readTVarIO,
  stateTVar,
  writeTQueue,
 )
import Hydra.Logic (
  ClientCommand (Close, Commit, Contest, Init, NewTx),
  ClientInstruction (..),
  Effect (ClientEffect, NetworkEffect, OnChainEffect, Wait),
  Event (ClientEvent, NetworkEvent, OnChainEvent),
  HeadState,
  HydraMessage (AckSn, AckTx, ConfSn, ConfTx, ReqSn, ReqTx),
  OnChainTx (..),
 )
import qualified Hydra.Logic as Logic
import System.Console.Repline (CompleterStyle (Word0), ExitDecision (Exit), evalRepl)
import Prelude (error)

-- | Monadic interface around 'Hydra.Logic.update'.
runHydra ::
  Monad m =>
  EventQueue m ->
  HydraNetwork m ->
  OnChain m ->
  ClientSide m ->
  HydraHead m ->
  m ()
runHydra EventQueue{nextEvent} HydraNetwork{broadcast} OnChain{postTx} ClientSide{showInstruction} HydraHead{hm} = do
  e <- nextEvent
  mout <- modifyHead hm $ \s -> case Logic.update s e of
    Nothing -> (Nothing, s)
    Just (st, out) -> (Just out, st)
  case mout of
    Nothing -> showInstruction CommandNotPossible -- NOTE(SN): only if the 'e' was a ClientCommand obviously
    Just out -> forM_ out $ \case
      ClientEffect i -> showInstruction i
      NetworkEffect msg -> broadcast msg
      OnChainEffect tx -> postTx tx
      Wait _cont -> panic "TODO: wait and reschedule continuation"

whatNext :: HydraHeadQuery m -> m [ClientCommand]
whatNext = error "TODO: identify possible next commands"

--
-- Some general event queue from which the Hydra head is "fed"
--

-- | The single, required queue in the system from which a hydra head is "fed".
-- NOTE(SN): this probably should be bounded and include proper logging
-- NOTE(SN): handle pattern, but likely not required as there is no need for an
-- alternative implementation
data EventQueue m = EventQueue
  { putEvent :: Event -> m ()
  , nextEvent :: m Event
  }

createEventQueue :: IO (EventQueue IO)
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
data HydraHead m = HydraHead
  { hq :: HydraHeadQuery m -- TODO(SN): maybe use fancy patterns to hide this better
  , hm :: HydraHeadMod m
  }

newtype HydraHeadQuery m = HydraHeadQuery
  { queryHead :: m HeadState
  }

newtype HydraHeadMod m = HydraHeadMod
  { modifyHead :: forall a. (HeadState -> (a, HeadState)) -> m a
  }

queryHeadState :: HydraHead m -> m HeadState
queryHeadState = queryHead . hq

possibleEvents :: HydraHead m -> m [Event]
possibleEvents = panic "TODO"

evolveHead :: HydraHead m -> Event -> m (Maybe [Effect])
evolveHead HydraHead{hm} e =
  modifyHead hm $ \s -> case Logic.update s e of
    Nothing -> (Nothing, s)
    Just (st, out) -> (Just out, st)

createHydraHead :: HeadState -> IO (HydraHead IO)
createHydraHead initialState = do
  tv <- newTVarIO initialState
  pure
    HydraHead
      { hq = HydraHeadQuery{queryHead = readTVarIO tv}
      , hm = HydraHeadMod{modifyHead = atomically . stateTVar tv}
      }

--
-- HydraNetwork handle to abstract over network access
--

-- | Handle to interface with the hydra network and send messages "off chain".
newtype HydraNetwork m = HydraNetwork
  { -- | Send a 'HydraMessage' to the whole hydra network.
    broadcast :: HydraMessage -> m ()
  }

-- | Connects to a configured set of peers and sets up the whole network stack.
createHydraNetwork :: EventQueue IO -> IO (HydraNetwork IO)
createHydraNetwork EventQueue{putEvent} = do
  -- NOTE(SN): obviously we should connect to a known set of peers here and do
  -- really broadcast messages to them
  pure HydraNetwork{broadcast = simulatedBroadcast}
 where
  simulatedBroadcast msg = do
    putStrLn @Text $ "[Network] should broadcast " <> show msg
    let ma = case msg of
          ReqTx -> Just AckTx
          AckTx -> Just ConfTx
          ConfTx -> Nothing
          ReqSn -> Just AckSn
          AckSn -> Just ConfSn
          ConfSn -> Nothing
    case ma of
      Just answer -> do
        putStrLn @Text $ "[Network] simulating answer " <> show answer
        putEvent $ NetworkEvent answer
      Nothing -> pure ()

--
-- OnChain handle to abstract over chain access
--

-- | Handle to interface with the main chain network
newtype OnChain m = OnChain
  { -- | Construct and send a transaction to the main chain corresponding to the
    -- given 'OnChainTx' event.
    postTx :: OnChainTx -> m ()
  }

-- | Connects to a cardano node and sets up things in order to be able to
-- construct actual transactions using 'OnChainTx' and send them on 'postTx'.
createChainClient :: EventQueue IO -> IO (OnChain IO)
createChainClient EventQueue{putEvent} = do
  -- NOTE(SN): obviously we should construct and send transactions, e.g. using
  -- plutus instead
  pure OnChain{postTx = simulatedPostTx}
 where
  simulatedPostTx tx = do
    putStrLn @Text $ "[OnChain] should post tx for " <> show tx
    let ma = case tx of
          InitTx -> Just CommitTx -- simulate other peer committing
          CommitTx -> Just CollectComTx -- simulate other peer collecting
          CollectComTx -> Nothing
          CloseTx -> Just ContestTx -- simulate other peer contesting
          ContestTx -> Nothing
          FanoutTx -> Nothing
    case ma of
      Just answer -> void . async $ do
        threadDelay 1000000
        putStrLn @Text $ "[OnChain] simulating  " <> show answer
        putEvent $ OnChainEvent answer
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
createClientSideRepl :: HydraHeadQuery IO -> EventQueue IO -> IO (ClientSide IO)
createClientSideRepl hq EventQueue{putEvent} = do
  link =<< async runRepl
  pure
    ClientSide
      { showInstruction = \ins -> putStrLn @Text $ "[ClientSide] " <> prettyInstruction ins
      }
 where
  prettyInstruction = \case
    CommandNotPossible -> "You dummy.. use a different command."
    ReadyToCommit -> "Head initialized, commit funds to it using 'commit'"
    AcceptingTx -> "Head is open, now feed the hydra with your 'newtx'"

  runRepl = evalRepl (const $ pure prompt) replCommand [] Nothing Nothing (Word0 replComplete) replInit (pure Exit)

  prompt = ">>> "

  -- TODO(SN): avoid redundancy
  commands = ["init", "commit", "newtx", "close", "contest"]

  replCommand c
    | c == "init" = liftIO $ putEvent $ ClientEvent Init -- a comamnd
    | c == "commit" = liftIO $ putEvent $ ClientEvent Commit -- a comamnd
    | c == "newtx" = liftIO $ putEvent $ ClientEvent NewTx -- a comamnd
    | c == "close" = liftIO $ putEvent $ ClientEvent Close -- a comamnd
    | c == "contest" = liftIO $ putEvent $ ClientEvent Contest -- a comamnd
    | c == "whatnext" = liftIO (print =<< whatNext hq)
    | otherwise = liftIO $ putStrLn @Text $ "Unknown command, use any of: " <> show commands

  replComplete n = pure $ filter (n `isPrefixOf`) commands

  replInit = liftIO $ putStrLn @Text "Welcome to the Hydra Node REPL, you can even use tab completion! (Ctrl+D to exit)"
