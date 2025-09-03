{-# LANGUAGE LambdaCase #-}

module VtyPlay where

import qualified Graphics.Vty as Vty
import Graphics.Vty.Platform.Unix (mkVty)

import Control.Concurrent.STM.TQueue
import qualified Data.ByteString as BS
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Word (Word8)

import Control.Concurrent.Async (async, cancel)

import Control.Concurrent.STM (atomically, check, orElse, readTVar, registerDelay)
import Control.Monad (forever, when)
import Data.Array (Array, bounds, elems)

stringToUtf8 :: String -> [Word8]
stringToUtf8 = BS.unpack . TE.encodeUtf8 . T.pack

runGame :: (Show b) => Array (Int, Int) b -> IO ()
runGame board = do
    vty <- mkVty Vty.defaultConfig

    Vty.setWindowTitle vty "Miner V"
    Vty.update vty $ Vty.picForImage $ draw board

    eventQ <- newTQueueIO

    vtyEvents <- async $ forever $ do
        e <- Vty.nextEvent vty
        atomically $ writeTQueue eventQ e

    let registerTick = registerDelay 1000000
    tickTimer <- registerTick

    let eventOrTick =
            atomically $
                readEvent `orElse` tick
          where
            readEvent = Right <$> readTQueue eventQ
            tick = readTVar tickTimer >>= check >> pure (Left ())

    whileM $
        eventOrTick >>= \case
            Left () -> pure True
            Right (Vty.EvKey Vty.KEsc []) -> pure False
            Right (Vty.EvKey (Vty.KChar 'q') []) -> pure False
            Right ev -> True <$ Vty.update vty (Vty.picForImage (Vty.string Vty.defAttr (show ev)))

    cancel vtyEvents

    Vty.shutdown vty
    putStrLn "Game over!"

draw :: (Show b) => Array (Int, Int) b -> Vty.Image
draw board = mconcat $ Vty.utf8String Vty.defAttr . stringToUtf8 . concatMap show <$> chunkRows board

chunkRows :: Array (Int, Int) b -> [[b]]
chunkRows array = go (elems array)
  where
    go [] = []
    go xs =
        let (h, t) = splitAt width xs
         in h : go t

    width =
        let ((_, xmin), (_, xmax)) = bounds array
         in xmax - xmin + 1

-- printBoard :: Array (Int, Int) Block -> String
-- printBoard = ("-------\n" ++) . unlines . map (concatMap show) . chunkRows

-- to avoid including `extra` as dependency
whileM :: (Monad m) => m Bool -> m ()
whileM act = do
    b <- act
    when b $ whileM act
