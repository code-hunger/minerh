{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ViewPatterns #-}

module Vty.Draw where

import Board (Board (Item, bounds, getWidth), Coord (..), Index (unIndex))
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
boardToImage (Game (unIndex -> player, playerState) board movingParts) = do
    width <- getWidth board
    ((stats <> topBorder width) <>) . linesToPicture <$> Board.lines board
  where
    linesToPicture = mconcat . fmap printLine . indexed
    printLine (row, xs) =
        let toPic (col, block) =
                if x player == col && y player == row
                    then Vty.utf8String Vty.defAttr $ stringToUtf8 "◉◉"
                    else Vty.utf8String (attr block) $ stringToUtf8 $ printBlock block
         in -- We add an empty string at the end of each line to fix right border's colours.
            -- Otherwise Vty semms not to clear the colour immediately after each block,
            -- which causes ugly trailing non-black colours to be draw at the end making the right
            -- border jagged.
            verticalBorder
                Vty.<|> Vty.horizCat (toPic <$> indexed xs)
                Vty.<|> verticalBorder
    verticalBorder = Vty.string Vty.defAttr "│"
    topBorder width = Vty.string Vty.defAttr $ "┌" ++ (concat . replicate width $ horizontalBorderChar) ++ "┐"
      where
        horizontalBorderChar = "──"

    stats =
        Vty.string Vty.defAttr $
            "Stats: moving parts ("
                ++ show (length movingParts)
                ++ "), player is "
                ++ show playerState

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
