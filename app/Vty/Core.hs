module Vty.Core (runVty, UserEvent (..)) where

import qualified Graphics.Vty as Vty
import Graphics.Vty.Platform.Unix (mkVty)

import Control.Monad.IO.Class (MonadIO (liftIO))
import Graphics.Vty.Input.Events (Event (EvKey))

runVty ::
    forall m.
    (MonadIO m) =>
    (forall e. IO e -> (e -> UserEvent) -> (Vty.Picture -> IO ()) -> m ()) ->
    m ()
runVty f = do
    vty <- liftIO $ mkVty Vty.defaultConfig

    liftIO $ Vty.setWindowTitle vty "Miner V"

    f (liftIO $ Vty.nextEvent vty) toUserEvent (Vty.update vty)

    liftIO $ Vty.shutdown vty
    liftIO $ putStrLn "Game over!"

data UserEvent = UTick | KEsc | KQ | KDown | KLeft | KUp | KRight | Other deriving (Show)

toUserEvent :: Vty.Event -> UserEvent
toUserEvent = \case
    (EvKey Vty.KEsc []) -> KEsc
    (EvKey (Vty.KChar 'q') []) -> KQ
    (EvKey Vty.KDown []) -> KDown
    (EvKey (Vty.KChar 'j') []) -> KDown
    (EvKey Vty.KUp []) -> KUp
    (EvKey (Vty.KChar 'k') []) -> KUp
    (EvKey Vty.KRight []) -> KRight
    (EvKey (Vty.KChar 'l') []) -> KRight
    (EvKey Vty.KLeft []) -> KLeft
    (EvKey (Vty.KChar 'h') []) -> KLeft
    _ -> Other
