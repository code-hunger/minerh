module Main where

import Data.Array (Array, Ix, elems, inRange)
import qualified Data.Array as A
import Data.Array.MArray (MArray, getAssocs, getBounds, readArray, writeArray)
import Data.Array.ST (STArray, freeze, newArray)
import Data.STRef (STRef, newSTRef, readSTRef, writeSTRef)

import Control.Monad (forM_, guard)
import Control.Monad.ST.Lazy (ST, runST, strictToLazyST)
import Control.Monad.ST.Lazy.Unsafe (unsafeInterleaveST)

import System.Random (RandomGen, StdGen, mkStdGen, uniformR)

main :: IO ()
main = mapM_ (putStr . printBoard) (take 5 boards)

boards :: [Array (Int, Int) Block]
boards = algo' (mkStdGen 42)

type Board s b = STArray s (Int, Int) b
data Block = Dirt | Stone deriving (Eq, Show)

initBoard :: a -> Int -> Int -> ST s (Board s a)
initBoard _ r c | r < 1 || c < 1 = error "initBoard expects positive rows and cols."
initBoard c rows cols = newArray ((0, 0), (rows - 1, cols - 1)) c

getNeighbours :: (Int, Int) -> Board s b -> ST s [b]
getNeighbours (i, j) arr = do
  bounds <- getBounds arr
  let inBounds = inRange bounds
  sequence $ do
    i' <- [i - 1 .. i + 1]
    j' <- [j - 1 .. j + 1]
    guard $ (i', j') /= (i, j)
    guard $ inBounds (i', j')
    pure $ arr `readArray` (i', j')

next ::
  forall e s.
  (Show e) =>
  ((Int, Int) -> [e] -> ST s e) ->
  Board s e ->
  ST s ()
next f board = onIndices f' board
 where
  f' i = do
    ns <- getNeighbours i board
    newValue <- f i ns
    writeArray board i newValue

onIndices ::
  (MArray a e m, Ix i) =>
  (i -> m ()) ->
  a i e ->
  m ()
onIndices f a = do
  assocs <- getAssocs a
  forM_ assocs $ f . fst

count :: (a -> Bool) -> [a] -> Int
count xs f = length $ filter xs f

countStones :: [Block] -> Int
countStones = count (== Stone)

weigh :: Block -> [Block] -> Double
weigh current neighbours =
  let stones = countStones neighbours
   in case current of
        Dirt -> fromIntegral (10 - stones) / 100
        Stone -> fromIntegral (1 + stones) / 100

algo ::
  forall s g.
  (RandomGen g) =>
  STRef s g ->
  Board s Block ->
  ST s ()
algo g board = next f' board
 where
  f' :: (Int, Int) -> [Block] -> ST s Block
  f' i nbs = strictToLazyST $ do
    v <- readArray board i
    gen <- readSTRef g
    let (r, g') = uniformR (0, 1) gen
    writeSTRef g g'
    pure $
      if r < weigh v nbs
        then (if v == Stone then Dirt else Stone)
        else v

algo' :: StdGen -> [Array (Int, Int) Block]
algo' g = runST $ do
  gg <- strictToLazyST $ newSTRef g
  board <- initBoard Dirt 10 200
  let go = do
        a <- freeze board
        algo gg board
        rest <- unsafeInterleaveST go
        pure (a : rest)
  go

chunkRows :: Array (Int, Int) b -> [[b]]
chunkRows array = go (elems array)
 where
  go [] = []
  go xs =
    let (h, t) = splitAt width xs
     in h : go t

  width =
    let ((_, jl), (_, ju)) = A.bounds array
     in ju - jl + 1

printBoard :: Array (Int, Int) Block -> String
printBoard = ("-------\n" ++) . unlines . map (map toChar) . chunkRows
 where
  toChar Stone = utf8block
  toChar Dirt = 'x'

utf8block :: Char
utf8block = '█'
