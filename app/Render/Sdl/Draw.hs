{-# LANGUAGE TypeOperators #-}

module Render.Sdl.Draw where

import Board (Board (getSize), Coord (..), Index (unIndex), SafeArray (Item), WithPass)
import qualified Board (BoardSize (..))
import Game.Core (Block (..), Game (Game), GameM, playerPos)
import Render.Common

import Control.Monad
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.State.Strict (MonadTrans (lift))
import qualified Control.Monad.State.Strict as State (get)
import Data.Array.IO (IOArray)

import qualified Data.Text ()
import Data.Word (Word8)
import Linear (V2 (..), V4 (..))
import qualified SDL

type GameIO = forall ph. GameM (WithPass (IOArray (Int, Int) Block)) ph IO ()

render ::
    forall board m ph.
    (Board board m, Item board ~ Block, MonadIO m) =>
    SDL.Renderer ->
    Int ->
    GameM board ph m ()
render renderer windowSize = do
    SDL.clear renderer
    state <- State.get
    renderGrid renderer windowSize state
    SDL.present renderer

renderGrid ::
    forall board m ph.
    (Board board m, Item board ~ Block, MonadIO m) =>
    SDL.Renderer ->
    Int ->
    Game (board ph) ph ->
    GameM board ph m ()
renderGrid renderer windowSize (Game playerState board _movingParts) = do
    slice <- lift $ makeSlice <$> getSize board

    mapM_ (drawRow slice) . zip [0 ..] =<< lift (sliceImage board slice)

    -- draw player on top!
    drawTile (x pp - startX slice, y pp - startY slice) (V4 255 255 255 255)
  where
    drawRow :: BoardSlice -> (Int, [Block]) -> GameM board ph m ()
    drawRow _slice (r, items) = forM_ (zip [0 ..] items) $ \(c, item) -> drawTile (c, r) (tileColor item)

    drawTile (c, r) colour = do
        let x = fromIntegral c * tileSize
            y = fromIntegral r * tileSize
            rect = SDL.Rectangle (SDL.P (V2 x y)) (V2 tileSize tileSize)
            tileSize = fromIntegral windowSize `div` 50

        SDL.rendererDrawColor renderer SDL.$= colour
        SDL.fillRect renderer (Just rect)

    makeSlice size =
        BoardSlice
            { rows = Board.rows size `min` 50
            , cols = Board.cols size `min` 50
            , startX = ((x pp - 25) `min` (Board.cols size - 50)) `max` 0
            , startY = ((y pp - 25) `min` (Board.rows size - 50)) `max` 0
            }
    pp = unIndex $ playerPos playerState

tileColor :: Block -> V4 Word8
tileColor Air = V4 0 0 0 0 -- transparent
tileColor Dirt = V4 120 72 40 255
tileColor Stone = V4 130 130 140 255
tileColor Stairs = V4 160 140 110 255
tileColor Fire = V4 255 80 0 255
