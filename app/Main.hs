{-# LANGUAGE FlexibleContexts #-}

module Main where

import BoardGen (BoardSize (..), CellUpdater, initBoard, makePureBoards, nextBoard)
import VtyPlay (UserEvent (..), runGame)

import Control.Monad.State (MonadIO (liftIO), MonadState (state), StateT, evalStateT)
import qualified Control.Monad.State as State (get, put)
import Data.Array (Array)
import Data.Array.Base (MArray)
import Data.Array.IO (IOArray)
import System.Random (RandomGen, mkStdGen, uniformR)

import qualified Graphics.Vty as Vty

import qualified Data.ByteString as BS
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Word (Word8)
import Graphics.Vty (defAttr)

import Board (Board (bounds, hasIndex, (!)), Coord (Coord), MBoard (write))
import qualified Board (lines)
import Control.Monad (when)

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

startingPos :: Coord
startingPos = Coord 23 0

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

boardToImage :: (Show e, MBoard b m e) => b -> m Vty.Image
boardToImage board = mconcat . fmap printLine <$> Board.lines board
  where
    printLine :: (Show e) => [e] -> Vty.Image
    printLine = Vty.utf8String Vty.defAttr . stringToUtf8 . concatMap show

playerImage :: (Monad m) => StateT Coord m Vty.Image
playerImage = do
    Coord x y <- State.get
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

weigh :: (RandomGen g) => CellUpdater m g Block
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
    if inBounds
        then do
            nextBlock <- board ! nextPos
            thisBlock <- board ! pos
            let needsStairs = dir == GoUp && thisBlock == Air
            when needsStairs $ write board pos Stairs
            let needsDig = nextBlock == Dirt
            when needsDig $ write board nextPos Air
            let willMove =
                    not (needsDig && dir == GoUp)
                        && nextBlock /= Stone
                        && nextBlock /= Fire
            pure $ if willMove then nextPos else pos
        else do
            b <- bounds board
            fail $ "Wrong position: " ++ show pos ++ " -> " ++ show nextPos ++ show b
