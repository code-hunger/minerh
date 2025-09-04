{-# LANGUAGE FlexibleContexts #-}

module Main where

import BoardGen (BoardSize (..), CellUpdater, initBoard, makePureBoards, nextBoard)
import VtyPlay (UserEvent (..), runGame)

import Control.Monad.State (MonadIO (liftIO), MonadState (state), evalStateT)
import qualified Control.Monad.State as State (get, put)
import Data.Array (Array)
import Data.Array.IO (IOArray)
import System.Random (RandomGen, mkStdGen, uniformR)

import qualified Graphics.Vty as Vty

import qualified Data.ByteString as BS
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Word (Word8)
import Graphics.Vty (defAttr)

import Board (Board (hasIndex, (!)), Coord (..), MBoard (write))
import qualified Board (lines)
import Control.Monad (when)

main :: IO ()
main = do
    board <- startingBoard
    let draw more player = do
            boardImage <- liftIO $ boardToImage board player
            let boardPicture = Vty.picForImage $ boardImage Vty.<-> more
            pure $ Just boardPicture

    flip evalStateT startingPos $
        runGame $
            \e -> do
                p <- State.get
                let move dir = do
                        p' <- liftIO $ movePlayer p dir board
                        State.put p'
                        draw (Vty.string defAttr $ show e) p'
                case e of
                    KEsc -> pure Nothing
                    KQ -> pure Nothing
                    KDown -> move GoDown
                    KUp -> move GoUp
                    KLeft -> move GoLeft
                    KRight -> move GoRight
                    UTick -> draw (Vty.string defAttr "Got tick!") p
                    _ -> draw (Vty.string defAttr "UNKNWN") p

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

boardToImage :: (MBoard b m Block) => b -> Coord -> m Vty.Image
boardToImage board player = mconcat . fmap printLine . zip [0 ..] <$> Board.lines board
  where
    printLine (row, xs) = Vty.horizCat $ toPic <$> zip [0 ..] xs
      where
        toPic (col, block) =
            if x player == col && y player == row
                then Vty.utf8String Vty.defAttr $ stringToUtf8 "◉◉"
                else Vty.utf8String (attr block) $ stringToUtf8 $ show block

    attr Dirt = Vty.defAttr `Vty.withBackColor` Vty.linearColor @Int 149 69 53
    attr Stone = Vty.defAttr `Vty.withForeColor` Vty.linearColor @Int 150 150 150
    attr Fire = Vty.defAttr `Vty.withBackColor` Vty.red
    attr _ = Vty.defAttr

data Block = Air | Dirt | Stone | Stairs | Fire
    deriving (Eq)

instance Show Block where
    show Stone = "🪨"
    show Dirt = "  "
    show Air = "  "
    show Fire = "🔥"
    show Stairs = "🪜"

boards :: [Array (Int, Int) Block]
boards = makePureBoards (BoardSize{rows = 30, cols = 100}) (mkStdGen 42) Dirt weigh

stringToUtf8 :: String -> [Word8]
stringToUtf8 = BS.unpack . TE.encodeUtf8 . T.pack

weigh :: (RandomGen g) => CellUpdater m g Block
weigh current neighbours =
    state (uniformR @Double (0, 1)) >>= \r ->
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
    (MBoard b m Block, MonadFail m) =>
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
