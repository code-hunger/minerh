{-# LANGUAGE LambdaCase #-}

module VtyPlay where

import qualified Graphics.Vty as Vty
import Graphics.Vty.Platform.Unix (mkVty)

import qualified Data.ByteString as BS
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Word (Word8)

import Data.Array (Array, bounds, elems)
import GameLoop (Event (InputEvent, Tick), loop)

stringToUtf8 :: String -> [Word8]
stringToUtf8 = BS.unpack . TE.encodeUtf8 . T.pack

runGame :: (Show b) => Array (Int, Int) b -> IO ()
runGame board = do
    vty <- mkVty Vty.defaultConfig

    Vty.setWindowTitle vty "Miner V"
    Vty.update vty $ Vty.picForImage $ draw board

    loop (Vty.nextEvent vty) (handleEvent vty)

    Vty.shutdown vty
    putStrLn "Game over!"

handleEvent :: Vty.Vty -> Event Vty.Event -> IO Bool
handleEvent vty = \case
    Tick -> pure True
    InputEvent (Vty.EvKey Vty.KEsc []) -> pure False
    InputEvent (Vty.EvKey (Vty.KChar 'q') []) -> pure False
    InputEvent ev -> True <$ Vty.update vty (Vty.picForImage (Vty.string Vty.defAttr (show ev)))

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
