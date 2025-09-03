{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}

module Main where

import BoardGen (BoardSize (..), CellUpdater, initBoard, makePureBoards, nextBoard)
import VtyPlay (UserEvent (..), runGame)

import Control.Monad.State (MonadIO (liftIO), MonadState (state), StateT, evalStateT)
import qualified Control.Monad.State as State (get, put)
import Data.Array (Array, Ix (inRange))
import Data.Array.Base (IArray (bounds), MArray (getBounds), elems, getElems, readArray, writeArray)
import Data.Array.IO (IOArray)
import System.Random (RandomGen, mkStdGen, uniformR)

import qualified Graphics.Vty as Vty

import qualified Data.ByteString as BS
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Word (Word8)
import Graphics.Vty (defAttr)

import Control.Monad (when)
import Data.Tuple (swap)

-- main = mapM_ (putStr . printBoard) (take 5 boards)

main :: IO ()
main = do
    board <- startingBoard
    let draw more = do
            boardImage <- liftIO $ boardToImage board
            Just . Vty.addToTop (Vty.picForImage $ boardImage Vty.<-> more) <$> playerImage
    flip evalStateT startingPos $
        runGame $
            \e -> do
                let move dir = do
                        p <- State.get
                        p' <- liftIO $ movePlayer p dir board
                        State.put p'
                        draw (Vty.string defAttr $ show e)
                case e of
                    KEsc -> pure Nothing
                    KQ -> pure Nothing
                    KDown -> move GoDown
                    KUp -> move GoUp
                    KLeft -> move GoLeft
                    KRight -> move GoRight
                    UTick -> draw (Vty.string defAttr "Got tick!")
                    _ -> draw (Vty.string defAttr "UNKNWN")

-- pure $ Just $ Vty.picForImage $ Vty.string defAttr (show e)

size :: BoardSize
size = BoardSize{cols = 100, rows = 90}

startingPos :: (Int, Int)
startingPos = (23, 0)

playerAttr :: Vty.Attr
playerAttr =
    defAttr
        `Vty.withForeColor` Vty.red
        `Vty.withBackColor` Vty.white

startingBoard :: IO (IOArray (Int, Int) Block)
startingBoard = do
    b <- initBoard Dirt size
    () <- flip evalStateT (mkStdGen 42) $ do
        nextBoard b weigh
        nextBoard b weigh
        nextBoard b weigh
    return b

boardToImage :: (Show b, MArray a b m) => a (Int, Int) b -> m Vty.Image
boardToImage board = mconcat . fmap printLine <$> chunkRows board
  where
    printLine :: (Show b) => [b] -> Vty.Image
    printLine = Vty.utf8String Vty.defAttr . stringToUtf8 . concatMap show

playerImage :: (Monad m) => StateT Pos m Vty.Image
playerImage = do
    playerPos <- State.get
    let (x, y) = playerPos
    return $ Vty.translate (2 * x) y $ Vty.string playerAttr "AA"

data Block = Air | Dirt | Stone | Stairs | Fire
    deriving (Eq)

instance Show Block where
    show Stone = [utf8block, utf8block]
    show Dirt = "xx"
    show Air = "  "
    show Fire = "🔥"
    show Stairs = "||"

boards :: [Array (Int, Int) Block]
boards = makePureBoards (BoardSize{rows = 30, cols = 100}) (mkStdGen 42) Dirt weigh

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

type Pos = (Int, Int)
data Dir = GoLeft | GoRight | GoUp | GoDown
    deriving (Eq, Show)

movePos :: Pos -> Dir -> Pos
movePos (x, y) GoLeft = (x - 1, y)
movePos (x, y) GoRight = (x + 1, y)
movePos (x, y) GoUp = (x, y - 1)
movePos (x, y) GoDown = (x, y + 1)

movePlayer :: (MArray a Block m, MonadFail m) => Pos -> Dir -> a (Int, Int) Block -> m Pos
movePlayer pos dir board = do
    let nextPos = movePos pos dir
    b <- getBounds board
    if inRange b (swap nextPos)
        then do
            nextBlock <- readArray board (swap nextPos)
            case nextBlock of
                Air -> do
                    thisBlock <- readArray board (swap pos)
                    when (dir == GoUp && thisBlock == Air) $
                        writeArray board (swap pos) Stairs
                    pure nextPos
                Dirt -> do
                    writeArray board (swap nextPos) Air
                    case dir of
                        GoUp -> pure pos
                        _ -> pure nextPos
                Stairs -> pure nextPos
                _ -> pure pos
        else fail $ "Wrong position!" ++ show pos ++ show nextPos ++ show b
