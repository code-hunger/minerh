module Vty.Core (runVty, UserEvent (..)) where

import qualified Graphics.Vty as Vty
import Graphics.Vty.CrossPlatform (mkVty)

import Control.Monad.IO.Class (MonadIO (liftIO))
import Graphics.Vty.Input.Events (Event (EvKey))

runVty ::
    forall m.
    (MonadIO m) =>
    ((Vty.Picture -> IO ()) -> IO UserEvent -> m ()) ->
    m ()
runVty f = do
    vty <- liftIO $ mkVty Vty.defaultConfig

    liftIO $ Vty.setWindowTitle vty "Miner V"

    f (Vty.update vty) (liftIO $ toUserEvent <$> Vty.nextEvent vty)

    liftIO $ Vty.shutdown vty
    liftIO $ putStrLn "Game over!"

data UserEvent = KEsc | KQ | KDown | KLeft | KUp | KRight | Other deriving (Show, Eq)

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
