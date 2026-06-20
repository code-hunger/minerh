{-# LANGUAGE OverloadedStrings #-}

module Render.Sdl.Core where

import Control.Monad.IO.Class (MonadIO, liftIO)

import Data.Text ()
import qualified SDL

import Render.Common (UserEvent (..))
import Render.Sdl.Draw (GameIO, render)

runSDL ::
    forall m.
    (MonadIO m) =>
    (GameIO -> IO [UserEvent] -> m ()) ->
    m ()
runSDL f = do
    SDL.initializeAll
    window <- SDL.createWindow "My SDL Application" SDL.defaultWindow
    renderer <- SDL.createRenderer window (-1) SDL.defaultRenderer

    f (render renderer) (liftIO $ map toUserEvent <$> SDL.pollEvents)

    SDL.destroyWindow window

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
