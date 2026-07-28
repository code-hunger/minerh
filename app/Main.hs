module Main where

import Board (BoardSize (..), Coord (..), SafeArray (justify), WithPass, withPass)
import BoardGen (CellUpdater, initBoard, nextBoard)
import qualified Control.Concurrent.STM.TQueue as TQ
import Control.Monad
import qualified Control.Monad.State.Lazy as StateL (evalStateT)
import Control.Monad.State.Strict (MonadIO (liftIO), MonadTrans (lift), StateT, evalStateT)
import qualified Control.Monad.State.Strict as State (get, state)
import Data.Array.IO (IOArray)
import Game (Block (..), Dir (..), Game (Game), PlayerState (Standing), runPlayerUp)
import qualified Game
import Game.Core (GameM)
import qualified Game.Update
import GameLoop (EventEmitter (..), UpdateHandler (..), UpdateStatus (..))
import qualified GameLoop as Game (loop)
import Render.Common (UserEvent (..))
import Render.Sdl.Core (runSDL)
import Render.Vty.Core (Renderer (..), runVty)
import Render.Vty.Draw (draw)
import Store (deserialize, serialize)
import System.Directory.Extra (doesFileExist)
import qualified System.Environment as SE
import System.Random (RandomGen, mkStdGen, uniformR)

storeFileName :: String
storeFileName = "store"

type GameIO ph = GameM (WithPass (IOArray (Int, Int) Block)) ph IO ()

main :: IO ()
main = do
  hasStore <- doesFileExist storeFileName
  args <- SE.getArgs
  let renderer = case args of
        "SDL" : _ -> loopInSDL =<< liftIO TQ.newTQueueIO
        _ -> loopInVty =<< liftIO TQ.newTQueueIO
  (if hasStore then loadGame else newGame) renderer

newGame :: (forall ph. GameM (WithPass (IOArray (Int, Int) Block)) ph IO ()) -> IO ()
newGame run = do
  array <- initBoard Dirt size
  withPass array $ \board -> do
    () <- flip StateL.evalStateT (mkStdGen 42) $ do
      nextBoard board weigh
      nextBoard board weigh
      nextBoard board weigh
    Just startPos <- justify board $ Coord (cols size `div` 2) 0
    evalStateT run $
      Game (Standing startPos) board []

loadGame :: (forall ph. GameIO ph) -> IO ()
loadGame renderer = do
  gameData <- readFile storeFileName
  deserialize gameData $ evalStateT renderer

loopInVty q = runVty $ \(Renderer render) emitEvent ->
  let draw' Die = pure Die
      draw' Live = do
        state <- State.get
        picture <- liftIO $ draw state
        liftIO @_ @() $ render picture
        pure Live
   in Game.loop q (UpdateHandler (draw' <=< update)) (EventEmitter emitEvent)

loopInSDL q = runSDL $ \draw emitEvent ->
  let draw' Die = pure Die
      draw' Live = draw >> pure Live
   in Game.loop q (UpdateHandler $ draw' <=< update . concat) (EventEmitter emitEvent)

update ::
  [UserEvent] ->
  StateT (Game (WithPass (IOArray (Int, Int) Block) ph) ph) IO UpdateStatus
update [] = Game.Update.update >> pure Live
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
size = BoardSize {cols = 30, rows = 20}

-- boards :: [Array (Int, Int) Block]
-- boards = makePureBoards (BoardSize{rows = 30, cols = 100}) (mkStdGen 42) Dirt weigh

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
