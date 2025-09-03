module GameLoop where

import Control.Monad (forever, when)

import Control.Concurrent.Async (async, cancel)
import Control.Concurrent.STM (atomically, check, orElse, readTVar, registerDelay)

import Control.Concurrent.STM.TQueue

data Event e = Tick | InputEvent e

loop :: IO a -> (Event a -> IO Bool) -> IO ()
loop nextEvent handleEvent = do
    eventQ <- newTQueueIO

    inputEventThread <- async $ forever $ do
        e <- nextEvent
        atomically $ writeTQueue eventQ e

    let eventOrTick tickTimer =
            atomically $
                readEvent `orElse` tick
          where
            readEvent = InputEvent <$> readTQueue eventQ
            tick = readTVar tickTimer >>= check >> pure Tick

        registerTick = registerDelay 1000000

        go tickTimer = do
            e <- eventOrTick tickTimer
            shouldContinue <- handleEvent e
            when shouldContinue $
                case e of
                    Tick -> registerTick >>= go
                    _ -> go tickTimer

    () <- go =<< registerTick

    cancel inputEventThread

-- to avoid including `extra` as dependency
whileM :: (Monad m) => m Bool -> m ()
whileM act = do
    b <- act
    when b $ whileM act
