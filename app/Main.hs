module Main where

import BoardGen (BoardSize (..), CellUpdater, initBoard, makePureBoards, nextBoard)
import Vty.Core (UserEvent (..), runVty)
import Vty.Draw (Block (..), draw)

import Control.Monad.State (MonadIO (liftIO), MonadTrans (lift), StateT, evalStateT)
import qualified Control.Monad.State as State (get, put, state)
import Data.Array (Array)
import Data.Array.IO (IOArray)
import System.Random (RandomGen, mkStdGen, uniformR)

import Board (Board (Item, hasIndex, (!)), Coord (..), MBoard (write))
import Control.Monad (forM, join, when, (<=<), (>=>))
import Data.Maybe (isJust)
import GameLoop (loop)

main :: IO ()
main = do
    board <- startingBoard

    let update ::
            UserEvent ->
            StateT Coord IO (Maybe (IOArray (Int, Int) Block, Coord))
        update e = do
            p <- State.get
            let move dir = do
                    p' <- liftIO $ movePlayer p dir board
                    State.put p'
                    pure $ Just (board, p')
            case e of
                KEsc -> pure Nothing
                KQ -> pure Nothing
                KDown -> move GoDown
                KUp -> move GoUp
                KLeft -> move GoLeft
                KRight -> move GoRight
                _ -> pure $ Just (board, p)

    flip evalStateT startingPos $ runVty $ \eventStream toUserEvent render ->
        loop eventStream $
            fmap isJust
                . mapM (liftIO . render)
                <=< mapM (liftIO . draw)
                <=< update
                    . maybe UTick toUserEvent

size :: BoardSize
size = BoardSize{cols = 100, rows = 90}

startingPos :: Coord
startingPos = Coord 23 0

startingBoard :: IO (IOArray (Int, Int) Block)
startingBoard = do
    b <- initBoard Dirt size
    () <- flip evalStateT (mkStdGen 42) $ do
        nextBoard b weigh
        nextBoard b weigh
        nextBoard b weigh
    return b

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

type Pos = (Int, Int)
data Dir = GoLeft | GoRight | GoUp | GoDown
    deriving (Eq, Show)

movePos :: Coord -> Dir -> Coord
movePos (Coord x y) GoLeft = Coord (x - 1) y
movePos (Coord x y) GoRight = Coord (x + 1) y
movePos (Coord x y) GoUp = Coord x (y - 1)
movePos (Coord x y) GoDown = Coord x (y + 1)

movePlayer ::
    (MBoard b m, MonadFail m, Item b ~ Block) =>
    Coord ->
    Dir ->
    b ->
    m Coord
movePlayer pos dir board = do
    let nextPos = movePos pos dir
    inBounds <- hasIndex board nextPos
    if not inBounds
        then pure pos
        else do
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
