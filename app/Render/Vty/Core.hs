module Render.Vty.Core (runVty, Renderer (..)) where

import Control.Monad.IO.Class (MonadIO (liftIO))

import qualified Graphics.Vty as Vty
import Graphics.Vty.CrossPlatform (mkVty)
import Graphics.Vty.Input.Events (Event (EvKey))

import Render.Common (UserEvent (..))

newtype Renderer = Renderer (Vty.Picture -> IO ())

runVty ::
    forall m.
    (MonadIO m) =>
    (Renderer -> IO UserEvent -> m ()) ->
    m ()
runVty f = do
    vty <- liftIO $ mkVty Vty.defaultConfig

    liftIO $ Vty.setWindowTitle vty "Miner V"

    f (Renderer $ Vty.update vty) (liftIO $ toUserEvent <$> Vty.nextEvent vty)

    liftIO $ Vty.shutdown vty
    liftIO $ putStrLn "Game over!"

toUserEvent :: Vty.Event -> UserEvent
toUserEvent = \case
    (EvKey Vty.KEsc []) -> KEsc
    (EvKey (Vty.KChar 'q') []) -> KQ
    (EvKey Vty.KDown [Vty.MShift]) -> KDownShift
    (EvKey (Vty.KChar 'J') []) -> KDownShift
    (EvKey Vty.KDown []) -> KDown
    (EvKey (Vty.KChar 'j') []) -> KDown
    (EvKey Vty.KUp []) -> KUp
    (EvKey (Vty.KChar 'k') []) -> KUp
    (EvKey Vty.KUp [Vty.MShift]) -> KUpShift
    (EvKey (Vty.KChar 'K') []) -> KUpShift
    (EvKey Vty.KRight []) -> KRight
    (EvKey (Vty.KChar 'l') []) -> KRight
    (EvKey Vty.KLeft []) -> KLeft
    (EvKey (Vty.KChar 'h') []) -> KLeft
    (EvKey (Vty.KChar 's') []) -> Save
    _ -> Other
