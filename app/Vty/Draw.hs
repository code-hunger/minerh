{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ViewPatterns #-}

module Vty.Draw where

import Board (Board (Item), Coord (..), Index (unIndex))
import qualified Board (lines)
import qualified Data.ByteString as BS
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Word (Word8)

import Game (Block (..), Game (Game))
import qualified Graphics.Vty as Vty

draw :: (Board board m, Item board ~ Block) => Game (board ph) ph -> m Vty.Picture
draw game = Vty.picForImage <$> boardToImage game

boardToImage :: (Board board m, Item board ~ Block) => Game (board ph) ph -> m Vty.Image
boardToImage (Game (unIndex -> player, _) board _) =
    linesToPicture <$> Board.lines board
  where
    linesToPicture = mconcat . fmap printLine . indexed
    printLine (row, xs) =
        let toPic (col, block) =
                if x player == col && y player == row
                    then Vty.utf8String Vty.defAttr $ stringToUtf8 "◉◉"
                    else Vty.utf8String (attr block) $ stringToUtf8 $ printBlock block
         in Vty.horizCat $ toPic <$> indexed xs :: Vty.Image

    attr Dirt = Vty.defAttr `Vty.withBackColor` Vty.linearColor @Int 149 69 53
    attr Stone = Vty.defAttr `Vty.withForeColor` Vty.linearColor @Int 150 150 150
    attr Fire = Vty.defAttr `Vty.withBackColor` Vty.red
    attr _ = Vty.defAttr

    stringToUtf8 :: String -> [Word8]
    stringToUtf8 = BS.unpack . TE.encodeUtf8 . T.pack

    indexed = zip [0 ..]

printBlock :: Block -> String
printBlock Stone = "🪨"
printBlock Dirt = "  "
printBlock Air = "  "
printBlock Fire = "🔥"
printBlock Stairs = "🪜"
