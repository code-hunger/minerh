module Main where

import BoardGen (makePureBoards)

import Control.Monad.State (MonadState (state), State)
import Data.Array (Array, bounds, elems)
import System.Random (RandomGen, mkStdGen, uniformR)

main :: IO ()
main = mapM_ (putStr . printBoard) (take 5 boards)

data Block = Dirt | Stone
  deriving (Eq, Show)

boards :: [Array (Int, Int) Block]
boards = makePureBoards (10, 50) (mkStdGen 42) Dirt weigh
 where
  weigh :: (RandomGen g) => Block -> [Block] -> State g Block
  weigh current neighbours =
    state (uniformR @Double (0, 1)) >>= \r ->
      let stones = countStones neighbours
          threshold = case current of
            Dirt -> fromIntegral (10 - stones) / 100
            Stone -> fromIntegral (1 + stones) / 100
          switch Stone = Dirt
          switch Dirt = Stone
       in pure $ if r < threshold then switch current else current

count :: (a -> Bool) -> [a] -> Int
count xs f = length $ filter xs f

countStones :: [Block] -> Int
countStones = count (== Stone)

chunkRows :: Array (Int, Int) b -> [[b]]
chunkRows array = go (elems array)
 where
  go [] = []
  go xs =
    let (h, t) = splitAt width xs
     in h : go t

  width =
    let ((_, xmin), (_, xmax)) = bounds array
     in xmax - xmin + 1

printBoard :: Array (Int, Int) Block -> String
printBoard = ("-------\n" ++) . unlines . map (concatMap toChar) . chunkRows
 where
  toChar Stone = [utf8block, utf8block]
  toChar Dirt = "xx"

utf8block :: Char
utf8block = '█'
