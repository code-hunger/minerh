module Main where

import BoardGen (BoardSize (..), CellUpdater, initBoard, makePureBoards, nextBoard)
import Vty.Core (UserEvent (..), runVty)
import Vty.Draw (draw)

import qualified Control.Monad.State.Lazy as StateL (evalStateT)
import Control.Monad.State.Strict (MonadIO (liftIO), MonadTrans (lift), StateT, evalStateT)
import qualified Control.Monad.State.Strict as State (get, state)
import Data.Array (Array)
import Data.Array.IO (IOArray)
import System.Random (RandomGen, mkStdGen, uniformR)

import Board (ArrayS, Board (justify), Coord (..), withArray)
import Control.Monad
import Game (Block (..), Dir (..), Game (..), PlayerState (Standing), runPlayerUp)
import qualified Game
import GameLoop (UpdateStatus (..))
import qualified GameLoop as Game (loop)
import Store (deserialize, serialize)
import System.Directory.Extra (doesFileExist)

storeFileName :: String
storeFileName = "store"

main :: IO ()
main = do
    hasStore <- doesFileExist storeFileName
    if hasStore then loadGame else newGame
  where
    newGame = do
        array <- initBoard Dirt size
        withArray array $ \board -> do
            () <- flip StateL.evalStateT (mkStdGen 42) $ do
                nextBoard board weigh
                nextBoard board weigh
                nextBoard board weigh
            Just startPos <- justify board $ Coord (cols size `div` 2) 0
            evalStateT (runVty f) $
                Game (startPos, Standing) board []
    loadGame = do
        gameData <- readFile storeFileName
        deserialize gameData $ evalStateT (runVty f)

    f render =
        let draw' Die = pure Die
            draw' Live = do
                state <- State.get
                picture <- liftIO $ draw state
                () <- liftIO $ render picture
                pure Live
         in Game.loop (draw' <=< update)

update ::
    [UserEvent] ->
    StateT (Game (ArrayS (IOArray (Int, Int) Block) ph) ph) IO UpdateStatus
update [] = Game.update >> pure Live
update (KEsc : _) = pure Die
update (KQ : _) = pure Die
update (KUpShift : events) = runPlayerUp GoUp >> update events
update (KDownShift : events) = runPlayerUp GoDown >> update events
update (Save : events) = do
    liftIO . writeFile storeFileName =<< lift . serialize =<< State.get
    update events
update (e : events) = do
    mapM_ Game.movePlayer $ toMovement e
    update events

size :: BoardSize
size = BoardSize{cols = 30, rows = 20}

boards :: [Array (Int, Int) Block]
boards = makePureBoards (BoardSize{rows = 30, cols = 100}) (mkStdGen 42) Dirt weigh

weigh :: (RandomGen g) => CellUpdater m g Block
weigh current neighbours =
    State.state (uniformR @Double (0, 1)) >>= \r ->
        let stones = countStones neighbours
            threshold = case current of
                Dirt -> fromIntegral (10 - stones) / 100
                Stone -> fromIntegral (1 + stones) / 100
                _ -> 2
            -- -> error "Can generate Dirt and Stone for now."
            switch Stone = Dirt
            switch Dirt = Stone
            switch _ = Fire -- error "Can generate Dirt and Stone for now."
            next = if r < threshold then switch current else current
         in pure $ if next == Stone && r * 3 < threshold then Fire else next
  where
    count :: (a -> Bool) -> [a] -> Int
    count xs f = length $ filter xs f

    countStones :: [Block] -> Int
    countStones = count (== Stone)

toMovement :: UserEvent -> Maybe Dir
toMovement KDown = Just GoDown
toMovement KUp = Just GoUp
toMovement KLeft = Just GoLeft
toMovement KRight = Just GoRight
toMovement _ = Nothing
