{-# LANGUAGE ViewPatterns #-}

module Clock where

import Control.Concurrent (threadDelay)
import Control.Monad (when)
import Control.Monad.IO.Class (MonadIO (liftIO))
import System.Clock

delayIfWasQuick :: (Integral a, MonadIO m) => a -> m b -> m b
delayIfWasQuick (fromIntegral -> n) action = do
  start <- liftIO $ getTime Monotonic
  r <- action
  end <- liftIO $ getTime Monotonic
  let elapsed = start `diffTimeSpec` end
      remaining = n - toNanoSecs elapsed
  when (remaining > 0) $
    liftIO $
      threadDelay (fromIntegral (remaining `div` 1000))
  pure r
