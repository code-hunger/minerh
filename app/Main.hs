module Main where

import BoardGen (BoardSize (..), CellUpdater, initBoard, makePureBoards, nextBoard)
import Vty.Core (UserEvent (..), runVty)
import Vty.Draw (draw)

import Control.Monad.State (MonadIO (liftIO), MonadTrans (lift), StateT, evalStateT)
import qualified Control.Monad.State as State (get, modify', state)
import Data.Array (Array)
import Data.Array.IO (IOArray)
import System.Random (RandomGen, mkStdGen, uniformR)

import Board (Board (Item, hasIndex, (!)), Coord (..), MBoard (write))
import Control.Monad (when, (>=>))
import Game (Block (..), Game (..))
import GameLoop (EventOrTick (..), UpdateStatus (..), loop)

main :: IO ()
main = evalStateT (runVty f) =<< start
  where
    f eventStream toUserEvent render =
        let draw' Die = pure Die
            draw' Live = do
                state <- State.get
                picture <- liftIO $ draw state
                () <- liftIO $ render picture
                pure Live
         in loop eventStream ((update >=> draw') . \case Tick -> UTick; Event c -> toUserEvent c)

update ::
    UserEvent ->
    StateT (Game (IOArray (Int, Int) Block)) IO UpdateStatus
update e = do
    let move dir = do
            newPosition <- movePlayer dir
            State.modify' (\g -> g{player = newPosition})
            pure Live
    case e of
        KEsc -> pure Die
        KQ -> pure Die
        KDown -> move GoDown
        KUp -> move GoUp
        KLeft -> move GoLeft
        KRight -> move GoRight
        _ -> pure Live

size :: BoardSize
size = BoardSize{cols = 100, rows = 90}

start :: IO (Game (IOArray (Int, Int) Block))
start = do
    b <- initBoard Dirt size
    () <- flip evalStateT (mkStdGen 42) $ do
        nextBoard b weigh
        nextBoard b weigh
        nextBoard b weigh
    return $ Game (Coord 23 0) b []

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
            switch _ = Air -- error "Can generate Dirt and Stone for now."
            next = if r < threshold then switch current else current
         in pure $ if next == Stone && r * 10 < threshold then Fire else next

count :: (a -> Bool) -> [a] -> Int
count xs f = length $ filter xs f

countStones :: [Block] -> Int
countStones = count (== Stone)

utf8block :: Char
utf8block = '█'

data Dir = GoLeft | GoRight | GoUp | GoDown
    deriving (Eq, Show)

movePos :: Coord -> Dir -> Coord
movePos (Coord x y) GoLeft = Coord (x - 1) y
movePos (Coord x y) GoRight = Coord (x + 1) y
movePos (Coord x y) GoUp = Coord x (y - 1)
movePos (Coord x y) GoDown = Coord x (y + 1)

movePlayer ::
    (MBoard b m, MonadFail m, Item b ~ Block) =>
    Dir ->
    StateT (Game b) m Coord
movePlayer dir = do
    (Game pos board _) <- State.get
    let nextPos = movePos pos dir
    inBounds <- lift $ hasIndex board nextPos
    if not inBounds
        then pure pos
        else lift $ do
            nextBlock <- board ! nextPos
            thisBlock <- board ! pos
            let needsDig = nextBlock == Dirt
            when needsDig $ write board nextPos Air
            let willMove =
                    not (needsDig && dir == GoUp)
                        && nextBlock /= Stone
                        && nextBlock /= Fire
            let needsStairs = dir == GoUp && thisBlock == Air && willMove
            when needsStairs $ write board pos Stairs
            pure $ if willMove then nextPos else pos
