{-# LANGUAGE NumericUnderscores #-}

module GameLoop (loop, EventOrTick (..), UpdateStatus (..), UpdateHandler (..), EventEmitter (..)) where

import Control.Monad (forever)

import qualified Control.Concurrent.Async as Async (async, cancel)
import Control.Concurrent.STM (atomically, check, orElse, readTVar, registerDelay)

import qualified Control.Concurrent.STM.TQueue as TQ
import Control.Monad.IO.Class (MonadIO (liftIO))
import GHC.Conc.Sync (TVar)

data EventOrTick e = Tick | Event e

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
    UpdateHandler e m ->
    EventEmitter e ->
    m ()
loop (UpdateHandler updateHandler) (EventEmitter nextEvent) = do
    eventQ <- liftIO TQ.newTQueueIO

    -- Starts a thread which continuously polls events from `nextEvent` and writes them to the event
    -- queue `eventQ`. The thread is canceled when exiting the program.
    -- I feel I should add some canceling mechanism in case an error happens before that.
    inputEventThread <- liftIO $ Async.async . forever $ atomically . TQ.writeTQueue eventQ =<< nextEvent

    let
        -- Concurrently try to read the next coming event and emit a tick when the given timer is
        -- out. Whichever happens first is returned.
        -- That is, if an event arrives in the queue before the timer is up, that event is returned.
        -- Otherwise, reading from the queue is canceled and a tick is returned.
        eventOrTick tickTimer =
            liftIO . atomically $
                let readEvent = Event <$> TQ.readTQueue eventQ
                    tick = readTVar tickTimer >>= check >> pure Tick
                 in readEvent `orElse` tick

        registerTick = liftIO $ registerDelay (1_000_000 `div` frequency) :: m (TVar Bool)

        -- Continuously polls for ticks or events, and calls the updateHandler accordingly.
        -- Events are collected in the accumulator and are only processed at Tick times by updateHandler.
        -- Exits are possible at Tick time only. The loop terminates when updateHandler returns a Die.
        go :: [e] -> TVar Bool -> m ()
        go events tickTimer =
            eventOrTick tickTimer
                >>= \case
                    Event e ->
                        -- accumulate events and keep the ticker
                        go (e : events) tickTimer
                    Tick ->
                        updateHandler events >>= \case
                            Die -> pure ()
                            Live ->
                                -- event stack consumed!
                                -- Reset and start a new ticker.
                                registerTick >>= go []

    () <- go [] =<< registerTick

    liftIO $ Async.cancel inputEventThread
