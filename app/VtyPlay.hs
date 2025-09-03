{-# LANGUAGE LambdaCase #-}

module VtyPlay (runGame, UserEvent (..)) where

import qualified Graphics.Vty as Vty
import Graphics.Vty.Platform.Unix (mkVty)

import Control.Monad.IO.Class (MonadIO (liftIO))

import GameLoop (Event (InputEvent, Tick), loop)

runGame :: (MonadIO m) => UserLogic m -> m ()
runGame f = do
    vty <- liftIO $ mkVty Vty.defaultConfig

    liftIO $ Vty.setWindowTitle vty "Miner V"

    loop (Vty.nextEvent vty) (handleEvent f vty)

    liftIO $ Vty.shutdown vty
    liftIO $ putStrLn "Game over!"

data UserEvent = UTick | KEsc | KQ | KDown | KLeft | KUp | KRight | Other deriving (Show)

type UserLogic m = UserEvent -> m (Maybe Vty.Picture)

handleEvent :: (MonadIO m) => UserLogic m -> Vty.Vty -> Event Vty.Event -> m Bool
handleEvent f vty e = do
    let ue = case e of
            Tick -> UTick
            InputEvent (Vty.EvKey Vty.KEsc []) -> KEsc
            InputEvent (Vty.EvKey (Vty.KChar 'q') []) -> KQ
            InputEvent (Vty.EvKey Vty.KDown []) -> KDown
            InputEvent (Vty.EvKey (Vty.KChar 'j') []) -> KDown
            InputEvent (Vty.EvKey Vty.KUp []) -> KUp
            InputEvent (Vty.EvKey (Vty.KChar 'k') []) -> KUp
            InputEvent (Vty.EvKey Vty.KRight []) -> KRight
            InputEvent (Vty.EvKey (Vty.KChar 'l') []) -> KRight
            InputEvent (Vty.EvKey Vty.KLeft []) -> KLeft
            InputEvent (Vty.EvKey (Vty.KChar 'h') []) -> KLeft
            InputEvent _ -> Other

    f ue >>= \case
        Nothing -> pure False
        Just picture -> do
            liftIO $ Vty.update vty picture
            pure True
