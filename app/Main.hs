module Main where

import BoardGen (Board, BoardSize, CellUpdater, makePureBoards)
import VtyPlay (draw, runGame)

import Control.Monad.ST.Lazy (ST)
import Control.Monad.State (MonadState (state), State)
import Data.Array (Array)
import Data.Array.Base (readArray, writeArray)
import System.Random (RandomGen, mkStdGen, uniformR)

main :: IO ()
-- main = mapM_ (putStr . printBoard) (take 5 boards)
main = runGame $ boards !! 3

data Block = Air | Dirt | Stone | Stairs | Fire
    deriving (Eq)

instance Show Block where
    show Stone = [utf8block, utf8block]
    show Dirt = "xx"
    show Air = "  "
    show Fire = "🔥"
    show Stairs = "||"

boards :: [Array (Int, Int) Block]
boards = makePureBoards (300, 100) (mkStdGen 42) Dirt weigh

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
