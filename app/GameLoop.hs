module GameLoop (loop, EventOrTick (..), UpdateStatus (..)) where

import Control.Monad (forever, when)

import qualified Control.Concurrent.Async as Async (async, cancel)
import Control.Concurrent.STM (atomically, check, orElse, readTVar, registerDelay)

import qualified Control.Concurrent.STM.TQueue as TQ
import Control.Monad.IO.Class (MonadIO (liftIO))
import GHC.Conc.Sync (TVar)

data EventOrTick e = Tick | Event e

data UpdateStatus = Live | Die

loop :: forall m e. (MonadIO m) => IO e -> (EventOrTick e -> m UpdateStatus) -> m ()
loop nextEvent handleEvent = do
    eventQ <- liftIO TQ.newTQueueIO

    inputEventThread <- liftIO $ Async.async . forever $ atomically . TQ.writeTQueue eventQ =<< nextEvent

    let eventOrTick tickTimer =
            liftIO . atomically $
                readEvent `orElse` tick
          where
            readEvent = Event <$> TQ.readTQueue eventQ
            tick = readTVar tickTimer >>= check >> pure Tick

        registerTick = liftIO $ registerDelay 1000000 :: m (TVar Bool)

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
