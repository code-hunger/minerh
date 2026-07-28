{-# LANGUAGE NumericUnderscores #-}

module GameLoop (loop, UpdateStatus (..), UpdateHandler (..), EventEmitter (..)) where

import Clock (delayIfWasQuick)
import qualified Control.Concurrent.Async as Async (async, cancel)
import Control.Concurrent.STM (atomically)
import qualified Control.Concurrent.STM.TQueue as TQ
import Control.Monad (forever)
import Control.Monad.IO.Class (MonadIO (liftIO))

data UpdateStatus = Live | Die

newtype UpdateHandler e m = UpdateHandler ([e] -> m UpdateStatus)

newtype EventEmitter e = EventEmitter (IO e)

frequency :: Int
frequency = 60

-- Runs `updateHandler` at the given frequency until it returns Die,
-- passing it all events accumulated from `nextEvent` since the last update.
loop ::
  forall m e.
  (MonadIO m) =>
  TQ.TQueue e ->
  UpdateHandler e m ->
  EventEmitter e ->
  m ()
loop eventQ (UpdateHandler updateHandler) (EventEmitter nextEvent) = do
  -- Starts a thread which continuously polls events from `nextEvent` and writes them to
  -- the event queue `eventQ`. The thread is canceled when exiting the program.
  -- I should add some canceling mechanism in case an error happens before that.
  inputEventThread <- liftIO $ Async.async . forever $ atomically . TQ.writeTQueue eventQ =<< nextEvent

  let flush = liftIO (atomically (TQ.flushTQueue eventQ))
      delay = delayIfWasQuick (1_000_000_000 `div` frequency)
      go =
        delay (flush >>= updateHandler)
          >>= \case
            Die -> pure ()
            Live -> go

  () <- go

  liftIO $ Async.cancel inputEventThread
