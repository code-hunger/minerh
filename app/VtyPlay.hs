{-# LANGUAGE LambdaCase #-}

module VtyPlay (runGame, UserEvent (..)) where

import qualified Graphics.Vty as Vty
import Graphics.Vty.Platform.Unix (mkVty)

import GameLoop (Event (InputEvent, Tick), loop)

runGame :: UserLogic -> IO ()
runGame f = do
    vty <- mkVty Vty.defaultConfig

    Vty.setWindowTitle vty "Miner V"

    loop (Vty.nextEvent vty) (handleEvent f vty)

    Vty.shutdown vty
    putStrLn "Game over!"

data UserEvent = UTick | KEsc | KDown | KLeft | KUp | KRight | Other

type UserLogic = UserEvent -> IO (Maybe Vty.Picture)

handleEvent :: UserLogic -> Vty.Vty -> Event Vty.Event -> IO Bool
handleEvent f vty e = do
    let ue = case e of
            Tick -> UTick
            InputEvent (Vty.EvKey Vty.KEsc []) -> KEsc
            InputEvent (Vty.EvKey Vty.KDown []) -> KDown
            InputEvent (Vty.EvKey Vty.KUp []) -> KUp
            InputEvent (Vty.EvKey Vty.KRight []) -> KRight
            InputEvent (Vty.EvKey Vty.KLeft []) -> KLeft
            InputEvent _ -> Other

    f ue >>= \case
        Nothing -> pure False
        Just picture -> do
            Vty.update vty picture
            pure True
