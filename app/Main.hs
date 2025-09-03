{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}

module Main where

import BoardGen (BoardSize, CellUpdater, initBoard, makePureBoards, nextBoard)
import VtyPlay (UserEvent (..), runGame)

import Control.Monad.State (MonadState (state), evalStateT)
import Data.Array (Array)
import Data.Array.Base (IArray (bounds), MArray (getBounds), elems, getElems, readArray, writeArray)
import Data.Array.IO (IOArray)
import System.Random (RandomGen, mkStdGen, uniformR)

import qualified Graphics.Vty as Vty

import qualified Data.ByteString as BS
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Word (Word8)

-- main = mapM_ (putStr . printBoard) (take 5 boards)

main :: IO ()
main = do
    board <- startingBoard
    runGame $ \case
        KEsc -> pure Nothing
        _ -> Just . Vty.picForImage <$> draw board

size :: BoardSize
size = (20, 20)

startingBoard :: IO (IOArray (Int, Int) Block)
startingBoard = do
    b <- initBoard Dirt size
    () <- flip evalStateT (mkStdGen 42) $ do
        nextBoard b weigh
        nextBoard b weigh
        nextBoard b weigh
        nextBoard b weigh
    return b

draw :: (Show b, MArray a b m) => a (Int, Int) b -> m Vty.Image
draw board = mconcat . fmap printLine <$> chunkRows board
  where
    printLine :: (Show b) => [b] -> Vty.Image
    printLine = Vty.utf8String Vty.defAttr . stringToUtf8 . concatMap show

data Block = Air | Dirt | Stone | Stairs | Fire
    deriving (Eq)

instance Show Block where
    show Stone = [utf8block, utf8block]
    show Dirt = "xx"
    show Air = "  "
    show Fire = "🔥"
    show Stairs = "||"

boards :: [Array (Int, Int) Block]
boards = makePureBoards (30, 100) (mkStdGen 42) Dirt weigh

stringToUtf8 :: String -> [Word8]
stringToUtf8 = BS.unpack . TE.encodeUtf8 . T.pack

chunkRows' :: Array (Int, Int) b -> [[b]]
chunkRows' array = go (elems array)
  where
    go [] = []
    go xs =
        let (h, t) = splitAt width xs
         in h : go t
    width =
        let ((_, xmin), (_, xmax)) = bounds array
         in xmax - xmin + 1

chunkRows :: (MArray a b m) => a (Int, Int) b -> m [[b]]
chunkRows array = do
    width <- getWidth array

    let go [] = []
        go xs =
            let (h, t) = splitAt width xs
             in h : go t
    go <$> getElems array
  where
    getWidth = fmap boundsToWidth . getBounds
    boundsToWidth ((_, xmin), (_, xmax)) = xmax - xmin + 1

weigh :: (RandomGen g) => CellUpdater g Block
weigh current neighbours =
    state (uniformR @Double (0, 1)) >>= \r ->
        let stones = countStones neighbours
            threshold = case current of
                Dirt -> fromIntegral (10 - stones) / 100
                Stone -> fromIntegral (1 + stones) / 100
                _ -> error "Can generate Dirt and Stone for now."
            switch Stone = Dirt
            switch Dirt = Stone
            switch _ = error "Can generate Dirt and Stone for now."
         in pure $ if r < threshold then switch current else current

count :: (a -> Bool) -> [a] -> Int
count xs f = length $ filter xs f

countStones :: [Block] -> Int
countStones = count (== Stone)

utf8block :: Char
utf8block = '█'

type Pos = BoardSize
data Dir = GoLeft | GoRight | GoUp | GoDown
    deriving (Eq, Show)

movePos :: Pos -> Dir -> Pos
movePos (x, y) GoLeft = (x - 1, y)
movePos (x, y) GoRight = (x + 1, y)
movePos (x, y) GoUp = (x, y + 1)
movePos (x, y) GoDown = (x, y - 1)

movePlayer :: (MArray a Block m) => Pos -> Dir -> a (Int, Int) Block -> m Pos
movePlayer pos dir board = do
    let nextPos = movePos pos dir
    nextBlock <- readArray board nextPos
    case nextBlock of
        Air -> pure nextPos
        Dirt -> do
            writeArray board nextPos Air
            case dir of
                GoUp -> pure pos
                _ -> pure nextPos
        Stairs -> pure nextPos
        _ -> pure pos
