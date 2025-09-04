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
frequency = 50

loop :: forall m e. (MonadIO m) => IO e -> (EventOrTick e -> m UpdateStatus) -> m ()
loop nextEvent handleEvent = do
    eventQ <- liftIO TQ.newTQueueIO

    inputEventThread <- liftIO $ Async.async . forever $ atomically . TQ.writeTQueue eventQ =<< nextEvent

    let eventOrTick tickTimer =
            liftIO . atomically $
                let readEvent = Event <$> TQ.readTQueue eventQ
                    tick = readTVar tickTimer >>= check >> pure Tick
                 in readEvent `orElse` tick

        registerTick = liftIO $ registerDelay (1_000_000 `div` frequency) :: m (TVar Bool)

        go :: TVar Bool -> m ()
        go tickTimer = do
            e <- liftIO $ eventOrTick tickTimer
            handleEvent e >>= \case
                Die -> pure ()
                Live -> case e of
                    -- Tick used, start a new timer!
                    Tick -> registerTick >>= go
                    -- Tick not yet used, keep it
                    _ -> go tickTimer

    () <- go =<< registerTick

    liftIO $ Async.cancel inputEventThread
