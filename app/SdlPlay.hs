{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module SdlPlay where

import Board (Board (getSize), Coord (..), Index (unIndex), SafeArray (Item), WithPass)
import qualified Board (BoardSize (..))
import Control.Monad
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.State.Strict (MonadTrans (lift))
import qualified Control.Monad.State.Strict as State (MonadIO (liftIO), get)
import Data.Array.IO (IOArray)
import Data.Text ()
import Data.Word (Word8)
import Game.Core (Block (..), Game (Game), GameM, playerPos)
import Linear (V2 (..), V4 (..))
import qualified SDL
import Vty.Core (UserEvent (..))
import Vty.Draw (BoardSlice (..), indexed, sliceRow)

type GameIO = forall ph. GameM (WithPass (IOArray (Int, Int) Block)) ph IO ()

runSDL ::
    forall m.
    (MonadIO m) =>
    (GameIO -> IO [UserEvent] -> m ()) ->
    m ()
runSDL f = do
    SDL.initializeAll
    window <- SDL.createWindow "My SDL Application" SDL.defaultWindow
    renderer <- SDL.createRenderer window (-1) SDL.defaultRenderer

    f (render renderer) (State.liftIO $ map toUserEvent <$> SDL.pollEvents)

    SDL.destroyWindow window

render ::
    forall board m ph.
    (Board board m, Item board ~ Block, MonadIO m) =>
    SDL.Renderer ->
    GameM board ph m ()
render renderer = do
    SDL.clear renderer
    state <- State.get
    renderGrid renderer state
    SDL.present renderer

renderGrid ::
    forall board m ph.
    (Board board m, Item board ~ Block, MonadIO m) =>
    SDL.Renderer ->
    Game (board ph) ph ->
    GameM board ph m ()
renderGrid renderer (Game playerState board _movingParts) = do
    slice <- lift $ makeSlice <$> getSize board

    mapM_ (drawRow slice) . indexed 0 =<< lift (sliceImage slice)

    -- draw player on top!
    drawTile (x pp - startX slice, y pp - startY slice) (V4 255 255 255 255)
  where
    drawRow :: BoardSlice -> (Int, [Block]) -> GameM board ph m ()
    drawRow slice (r, items) = forM_ (indexed 0 items) $ \(c, item) -> drawTile (c, r) (tileColor item)

    drawTile (c, r) colour = do
        let x = fromIntegral c * tileSize
            y = fromIntegral r * tileSize
            rect = SDL.Rectangle (SDL.P (V2 x y)) (V2 tileSize tileSize)
            tileSize = 12

        SDL.rendererDrawColor renderer SDL.$= colour
        SDL.fillRect renderer (Just rect)

    sliceImage :: BoardSlice -> m [[Block]]
    sliceImage slice =
        let visibleRows =
                [ startY slice + i
                | i <- [0 .. rows slice - 1]
                ]
         in mapM (sliceRow board slice) visibleRows

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

logInfo :: (MonadIO m) => String -> GameM board ph m ()
logInfo = State.liftIO . appendFile "log"

toUserEvent :: SDL.Event -> UserEvent
toUserEvent = \case
    SDL.Event _ (SDL.KeyboardEvent ke)
        | SDL.keyboardEventKeyMotion ke == SDL.Pressed
        , not (SDL.keyboardEventRepeat ke) ->
            let ks = SDL.keyboardEventKeysym ke
                code = SDL.keysymKeycode ks
                mods = SDL.keysymModifier ks
                shift = hasShift mods
             in case (code, shift) of
                    (SDL.KeycodeEscape, _) -> KEsc
                    (SDL.KeycodeQ, _) -> KQ
                    (SDL.KeycodeS, _) -> Save
                    (SDL.KeycodeDown, True) -> KDownShift
                    (SDL.KeycodeJ, True) -> KDownShift
                    (SDL.KeycodeDown, _) -> KDown
                    (SDL.KeycodeJ, _) -> KDown
                    (SDL.KeycodeUp, True) -> KUpShift
                    (SDL.KeycodeK, True) -> KUpShift
                    (SDL.KeycodeUp, _) -> KUp
                    (SDL.KeycodeK, _) -> KUp
                    (SDL.KeycodeRight, _) -> KRight
                    (SDL.KeycodeL, _) -> KRight
                    (SDL.KeycodeLeft, _) -> KLeft
                    (SDL.KeycodeH, _) -> KLeft
                    _ -> Other
    _ -> Other

-- SDL.KeyModifier is a bitmask newtype; check shift bits.
hasShift :: SDL.KeyModifier -> Bool
hasShift (SDL.KeyModifier a b _ _ _ _ _ _ _ _ _) = a || b
