{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ViewPatterns #-}

module Vty.Draw where

import Board (Board (Item, getWidth, safeAt), Coord (..), Index (unIndex))
import qualified Data.ByteString as BS
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Word (Word8)

import Control.Monad.Extra (mapMaybeM)
import Data.List.Extra (mconcatMap)
import Game (Block (..), Game (Game))
import qualified Graphics.Vty as Vty

draw :: (Board board m, Item board ~ Block) => Game (board ph) ph -> m Vty.Picture
draw game = Vty.picForImage <$> boardToImage game

boardToImage :: (Board board m, Item board ~ Block) => Game (board ph) ph -> m Vty.Image
boardToImage (Game (unIndex -> playerPos, playerState) board movingParts) = do
    (min 50 -> width) <- getWidth board
    (stats <>) . addHorizontalBorders width <$> image width
  where
    image width = mconcatMap (addVerticalBorders . printLine) . indexed yStartFrom <$> sliceImage width

    sliceImage size =
        let rowsToDraw = [yStartFrom .. yStartFrom + size]
         in mapM (sliceRow board size . Coord xStartFrom) rowsToDraw

    yStartFrom = (y playerPos - 25) `max` 0
    xStartFrom = (x playerPos - 25) `max` 0

    stats =
        Vty.string Vty.defAttr $
            "Stats: moving parts ("
                ++ show (length movingParts)
                ++ "), player is "
                ++ show playerState

    printLine (row, xs) =
        let toPic (col, block) =
                if playerPos == Coord col row
                    then Vty.utf8String Vty.defAttr $ stringToUtf8 "◉◉"
                    else Vty.utf8String (attr block) $ stringToUtf8 $ printBlock block
         in Vty.horizCat (toPic <$> indexed xStartFrom xs)

sliceRow :: (Board board m) => board ph -> Int -> Coord -> m [Item board]
sliceRow board count startFrom = mapMaybeM (Board.safeAt board) wantedCoords
  where
    wantedCoords =
        [ Coord x' (y startFrom)
        | x' <- [x startFrom .. x startFrom + count - 1]
        ]

attr :: Block -> Vty.Attr
attr Dirt = Vty.defAttr `Vty.withBackColor` Vty.linearColor @Int 149 69 53
attr Stone = Vty.defAttr `Vty.withForeColor` Vty.linearColor @Int 150 150 150
attr Fire = Vty.defAttr `Vty.withBackColor` Vty.red
attr _ = Vty.defAttr

indexed :: (Enum a) => a -> [b] -> [(a, b)]
indexed startFrom = zip [startFrom ..]

stringToUtf8 :: String -> [Word8]
stringToUtf8 = BS.unpack . TE.encodeUtf8 . T.pack

addHorizontalBorders :: Int -> Vty.Image -> Vty.Image
addHorizontalBorders width pic = topBorder <> pic <> bottomBorder
  where
    topBorder = Vty.string Vty.defAttr $ "┌" ++ (concat . replicate width $ horizontalBorderChar) ++ "┐"
    bottomBorder = Vty.string Vty.defAttr $ "└" ++ (concat . replicate width $ horizontalBorderChar) ++ "┘"
    horizontalBorderChar = "──"

addVerticalBorders :: Vty.Image -> Vty.Image
addVerticalBorders image = verticalBorder Vty.<|> image Vty.<|> verticalBorder
  where
    verticalBorder = Vty.string Vty.defAttr "│"

printBlock :: Block -> String
printBlock Stone = "🪨"
printBlock Dirt = "  "
printBlock Air = "  "
printBlock Fire = "🔥"
printBlock Stairs = "🪜"
