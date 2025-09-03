module VtyPlay where

import Graphics.Vty
import Graphics.Vty.Platform.Unix (mkVty)

import qualified Data.ByteString as BS
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Word (Word8)

import Data.Array (Array, bounds, elems)

stringToUtf8 :: String -> [Word8]
stringToUtf8 = BS.unpack . TE.encodeUtf8 . T.pack

runGame :: (Show b) => Array (Int, Int) b -> IO ()
runGame board = do
    vty <- mkVty defaultConfig

    setWindowTitle vty "Miner V"
    update vty $ picForImage $ draw board
    e <- nextEvent vty
    shutdown vty
    print ("Last event was: " ++ show e)

draw :: (Show b) => Array (Int, Int) b -> Image
draw board = mconcat $ utf8String defAttr . stringToUtf8 . concatMap show <$> chunkRows board

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
