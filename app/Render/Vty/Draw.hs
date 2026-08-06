{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ViewPatterns #-}

module Render.Vty.Draw where

import Board (Board (getSize), Coord (..), Index (unIndex), SafeArray (Item))
import qualified Board (BoardSize (..))
import qualified Data.ByteString as BS
import Data.List.Extra (mconcatMap)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Word (Word8)
import Game.Core (Block (..), Game (Game), playerPos)
import qualified Graphics.Vty as Vty
import Render.Common (BoardSlice (..), sliceImage)

draw :: (Board board m, Item board ~ Block, Monad m) => Game (board ph) ph -> m Vty.Picture
draw game = Vty.picForImage <$> boardToImage game

boardToImage ::
    forall board m ph.
    ( Board board m
    , Item board ~ Block
    , Monad m
    ) =>
    Game (board ph) ph ->
    m Vty.Image
boardToImage (Game playerState board movingParts) = do
    slice <- makeSlice <$> getSize board
    imageSlice <- image slice
    let width = cols slice
    pure $
        stats
            <> topBorder width
            <> imageSlice
            <> bottomBorder width
            <> Vty.string def "Should see bottom border above this line"
  where
    windowHalfWidth = 25
    windowWidth = windowHalfWidth * 2
    windowHalfHeight = 25
    windowHeight = windowHalfHeight * 2
    makeSlice size =
        BoardSlice
            { rows = windowHeight `min` Board.rows size
            , cols = windowWidth `min` Board.cols size
            , startX = (x pp - windowHalfWidth) `clampTo` (0, Board.cols size - windowWidth)
            , startY = (y pp - windowHalfHeight) `clampTo` (0, Board.rows size - windowHeight)
            }
      where
        a `clampTo` (from, to) = a `min` to `max` from

    pp = unIndex $ playerPos playerState

    image slice =
        let enumerateRows = indexed (startY slice)
            verticalBorder = Vty.string Vty.defAttr "│"
            addVerticalBorders i = verticalBorder Vty.<|> i Vty.<|> verticalBorder
         in mconcatMap (addVerticalBorders . printLine slice) . enumerateRows <$> sliceImage board slice

    stats =
        Vty.string Vty.defAttr $
            "Stats: moving parts ("
                ++ show (length movingParts)
                ++ "), player is "
                ++ show playerState

    printLine slice (row, xs) =
        let toPic (col, block) =
                if pp == Coord col row
                    then vtyUtf8String def "◉◉"
                    else vtyUtf8String (attr block) $ printBlock block
         in Vty.horizCat
                -- ( Vty.string def (show row) :
                (toPic <$> indexed (startX slice) xs)

    topBorder width = vtyUtf8String def $ "┌" ++ concat (replicate width horizontalBorderChar) ++ "┐"
    bottomBorder width = vtyUtf8String def $ "└" ++ concat (replicate width horizontalBorderChar) ++ "┘"
    horizontalBorderChar = "──"

    def = Vty.defAttr

    vtyUtf8String a = Vty.utf8String a . stringToUtf8

attr :: Block -> Vty.Attr
attr Dirt = Vty.defAttr `Vty.withBackColor` Vty.linearColor @Int 149 69 53
attr Stone = attr Dirt
attr Fire = Vty.defAttr `Vty.withBackColor` Vty.red
attr _ = Vty.defAttr

indexed :: (Enum a) => a -> [b] -> [(a, b)]
indexed startFrom = zip [startFrom ..]

stringToUtf8 :: String -> [Word8]
stringToUtf8 = BS.unpack . TE.encodeUtf8 . T.pack

printBlock :: Block -> String
printBlock Stone = "🪨"
printBlock Dirt = "  "
printBlock Air = "  "
printBlock Fire = "🔥"
printBlock Stairs = "🪜"
