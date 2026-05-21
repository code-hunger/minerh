{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ViewPatterns #-}

module Vty.Draw where

import Board (Board (getSize), Coord (..), Index (unIndex), SafeArray (Item, justify, (!)))
import qualified Board (BoardSize (..))
import qualified Data.ByteString as BS
import Data.Maybe (fromMaybe)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Word (Word8)

import Data.List.Extra (mconcatMap)
import Game.Core (Block (..), Game (Game), playerPos)
import qualified Graphics.Vty as Vty

draw :: (Board board m, Item board ~ Block, Monad m) => Game (board ph) ph -> m Vty.Picture
draw game = Vty.picForImage <$> boardToImage game

data BoardSlice = BoardSlice {rows :: Int, cols :: Int, startX :: Int, startY :: Int}

boardToImage :: forall board m ph. (Board board m, Item board ~ Block, Monad m) => Game (board ph) ph -> m Vty.Image
boardToImage (Game playerState board movingParts) = do
    slice <- makeSlice <$> getSize board
    (stats <>) . addHorizontalBorders slice <$> image slice
  where
    makeSlice size =
        BoardSlice
            { rows = Board.rows size `min` 50
            , cols = Board.cols size `min` 50
            , startX = ((x pp - 25) `min` (Board.cols size - 50)) `max` 0
            , startY = ((y pp - 25) `min` (Board.rows size - 50)) `max` 0
            }

    pp = unIndex $ playerPos playerState

    image slice =
        let enumerateRows = indexed (startY slice)
         in mconcatMap (addVerticalBorders . printLine slice) . enumerateRows <$> sliceImage slice

    sliceImage :: BoardSlice -> m [[Block]]
    sliceImage slice =
        let visibleRows =
                [ startY slice + i
                | i <- [0 .. rows slice - 1]
                ]
         in mapM (sliceRow board slice) visibleRows

    stats =
        Vty.string Vty.defAttr $
            "Stats: moving parts ("
                ++ show (length movingParts)
                ++ "), player is "
                ++ show playerState

    printLine slice (row, xs) =
        let toPic (col, block) =
                if pp == Coord col row
                    then Vty.utf8String Vty.defAttr $ stringToUtf8 "◉◉"
                    else Vty.utf8String (attr block) $ stringToUtf8 $ printBlock block
         in Vty.horizCat (toPic <$> indexed (startX slice) xs)

sliceRow :: forall m board ph. (Board board m, Monad m) => board ph -> BoardSlice -> Int -> m [Item board]
sliceRow board slice row = mapM forceRead wantedCoords
  where
    wantedCoords =
        [ Coord (startX slice + i) row
        | i <- [0 .. cols slice - 1]
        ]
    forceRead :: Coord -> m (Item board)
    forceRead k =
        -- As of now, we can't convince the type system that our coordinates are valid,
        -- so we have to use fromJust. I leave a message just in case.
        -- A more 'proper' solution would be to export richer functions from the Board module, which
        -- provide more justified indices, e.g. a function that returns an already justified view
        -- around a justified index.
        fromMaybe (error "Board index out of bounds during rendering")
            <$> (justify board k >>= mapM (board !))

attr :: Block -> Vty.Attr
attr Dirt = Vty.defAttr `Vty.withBackColor` Vty.linearColor @Int 149 69 53
attr Stone = Vty.defAttr `Vty.withForeColor` Vty.linearColor @Int 150 150 150
attr Fire = Vty.defAttr `Vty.withBackColor` Vty.red
attr _ = Vty.defAttr

indexed :: (Enum a) => a -> [b] -> [(a, b)]
indexed startFrom = zip [startFrom ..]

stringToUtf8 :: String -> [Word8]
stringToUtf8 = BS.unpack . TE.encodeUtf8 . T.pack

addHorizontalBorders :: BoardSlice -> Vty.Image -> Vty.Image
addHorizontalBorders (cols -> width) pic = topBorder <> pic <> bottomBorder
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
