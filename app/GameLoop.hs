{-# LANGUAGE NumericUnderscores #-}

module GameLoop (loop, EventOrTick (..), UpdateStatus (..)) where

import Control.Monad (forever, when)

import qualified Control.Concurrent.Async as Async (async, cancel)
import Control.Concurrent.STM (atomically, check, orElse, readTVar, registerDelay)

import qualified Control.Concurrent.STM.TQueue as TQ
import Control.Monad.IO.Class (MonadIO (liftIO))
import GHC.Conc.Sync (TVar)

data EventOrTick e = Tick | Event e

data UpdateStatus = Live | Die

frequency :: Int
frequency = 30

loop ::
    forall m e.
    (MonadIO m) =>
    ([e] -> m UpdateStatus) ->
    IO e ->
    m ()
loop updateHandler nextEvent = do
    eventQ <- liftIO TQ.newTQueueIO

    inputEventThread <- liftIO $ Async.async . forever $ atomically . TQ.writeTQueue eventQ =<< nextEvent

    let eventOrTick tickTimer =
            liftIO . atomically $
                let readEvent = Event <$> TQ.readTQueue eventQ
                    tick = readTVar tickTimer >>= check >> pure Tick
                 in readEvent `orElse` tick

        registerTick = liftIO $ registerDelay (1_000_000 `div` frequency) :: m (TVar Bool)

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
