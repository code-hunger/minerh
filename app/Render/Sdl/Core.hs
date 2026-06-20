{-# LANGUAGE OverloadedStrings #-}

module Render.Sdl.Core where

import Control.Monad.IO.Class (MonadIO, liftIO)

import Data.Text ()
import qualified SDL

import Render.Common (UserEvent (..))
import Render.Sdl.Draw (GameIO, render)

import Foreign.C (CInt (CInt))
import Linear (V2 (..))

defaultSize = V2 300 300

runSDL ::
    forall m.
    (MonadIO m) =>
    (GameIO -> IO [UserEvent] -> m ()) ->
    m ()
runSDL f = do
    SDL.initialize [SDL.InitEvents, SDL.InitVideo]
    windowSize <- decideWindowSize
    window <-
        SDL.createWindow "Haskell Game" $
            SDL.defaultWindow
                { SDL.windowInitialSize = V2 windowSize windowSize
                , SDL.windowResizable = True
                }
    renderer <- SDL.createRenderer window (-1) SDL.defaultRenderer

    f (render renderer (fromIntegral windowSize)) (liftIO $ map toUserEvent <$> SDL.pollEvents)

    SDL.destroyWindow window
  where
    decideWindowSize = do
        displays <- SDL.getDisplays
        displaySize <- case displays of
            [] -> liftIO $ putStrLn "No displays detected, something wrong?" >> pure defaultSize
            [single] -> pure $ SDL.displayBoundsSize single
            (first : _) -> liftIO $ putStrLn "Several displays available. Picking first one." >> pure (SDL.displayBoundsSize first)
        let V2 w h = displaySize
        pure $ w `min` h

toUserEvent :: SDL.Event -> UserEvent
toUserEvent = \case
    SDL.Event _ (SDL.KeyboardEvent ke)
        | SDL.keyboardEventKeyMotion ke == SDL.Pressed ->
            -- , not (SDL.keyboardEventRepeat ke)
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
