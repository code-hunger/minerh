{-# LANGUAGE TypeOperators #-}

module Vty.Draw where

import Board (Board (Item), Coord (..))
import qualified Board (lines)
import qualified Data.ByteString as BS
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Word (Word8)

import qualified Graphics.Vty as Vty

data Block = Air | Dirt | Stone | Stairs | Fire
    deriving (Eq)

instance Show Block where
    show Stone = "🪨"
    show Dirt = "  "
    show Air = "  "
    show Fire = "🔥"
    show Stairs = "🪜"

draw :: (Board board m, Item board ~ Block) => (board, Coord) -> m Vty.Picture
draw (board, player) = do
    boardImage <- boardToImage board player
    pure $ Vty.picForImage boardImage

boardToImage :: (Board board m, Item board ~ Block) => board -> Coord -> m Vty.Image
boardToImage board player = linesToPicture <$> Board.lines board
  where
    linesToPicture = mconcat . fmap printLine . indexed
    printLine (row, xs) =
        let toPic (col, block) =
                if x player == col && y player == row
                    then Vty.utf8String Vty.defAttr $ stringToUtf8 "◉◉"
                    else Vty.utf8String (attr block) $ stringToUtf8 $ show block
         in Vty.horizCat $ toPic <$> indexed xs :: Vty.Image

    attr Dirt = Vty.defAttr `Vty.withBackColor` Vty.linearColor @Int 149 69 53
    attr Stone = Vty.defAttr `Vty.withForeColor` Vty.linearColor @Int 150 150 150
    attr Fire = Vty.defAttr `Vty.withBackColor` Vty.red
    attr _ = Vty.defAttr

    stringToUtf8 :: String -> [Word8]
    stringToUtf8 = BS.unpack . TE.encodeUtf8 . T.pack

    indexed = zip [0 ..]
