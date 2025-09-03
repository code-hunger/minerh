module GameLoop (Event (InputEvent, Tick), loop) where

import Control.Monad (forever, when)

import qualified Control.Concurrent.Async as Async (Async, async, cancel)
import Control.Concurrent.STM (atomically, check, orElse, readTVar, registerDelay)

import qualified Control.Concurrent.STM.TQueue as TQ

data Event e = Tick | InputEvent e

loop :: IO a -> (Event a -> IO Bool) -> IO ()
loop nextEvent handleEvent = do
    eventQ <- TQ.newTQueueIO

    inputEventThread <- Async.async . forever . atomically . TQ.writeTQueue eventQ =<< nextEvent

    let eventOrTick tickTimer =
            atomically $
                readEvent `orElse` tick
          where
            readEvent = InputEvent <$> TQ.readTQueue eventQ
            tick = readTVar tickTimer >>= check >> pure Tick

        registerTick = registerDelay 1000000

        go tickTimer = do
            e <- eventOrTick tickTimer
            shouldContinue <- handleEvent e
            when shouldContinue $
                case e of
                    -- Tick used, start a new timer!
                    Tick -> registerTick >>= go
                    -- Tick not yet used, keep it
                    _ -> go tickTimer

    () <- go =<< registerTick

    Async.cancel inputEventThread
