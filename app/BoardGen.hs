module BoardGen where

import Data.Array.MArray (getAssocs, getBounds, readArray, writeArray)
import Data.STRef (STRef, newSTRef, readSTRef, writeSTRef)

import Control.Monad (forM_, guard)
import Control.Monad.ST.Lazy (ST, runST, strictToLazyST)
import Control.Monad.ST.Lazy.Unsafe (unsafeInterleaveST)
import Data.Array (Array, inRange)
import Data.Array.ST (STArray, freeze, newArray)

import Control.Monad.State (State, runState)
import System.Random (RandomGen)

type Board s b = STArray s (Int, Int) b

type CellUpdater g b = (b -> [b] -> State g b)

type BoardSize = (Int, Int)

makePureBoards ::
  forall g b.
  (RandomGen g) =>
  BoardSize ->
  g ->
  b ->
  CellUpdater g b ->
  [Array (Int, Int) b]
makePureBoards size g defBlock weigh = runST $ do
  gRef <- strictToLazyST $ newSTRef g
  board <- initBoard defBlock size
  let
    go = do
      a <- freeze board
      nextBoard gRef board weigh
      rest <- unsafeInterleaveST go
      pure (a : rest)
  go

nextBoard :: (RandomGen g) => STRef s g -> Board s b -> CellUpdater g b -> ST s ()
nextBoard gRef board weigh = do
  assocs <- getAssocs board
  forM_ assocs $ updateCell . fst
 where
  updateCell i = do
    nbs <- getNeighbours i board
    strictToLazyST $ do
      val <- readArray board i
      g <- readSTRef gRef
      let (res, g') = runState (weigh val nbs) g
      writeSTRef gRef g'
      writeArray board i res

getNeighbours :: (Int, Int) -> Board s b -> ST s [b]
getNeighbours (i, j) board = do
  bounds <- getBounds board
  let inBounds = inRange bounds
  sequence $ do
    i' <- [i - 1 .. i + 1]
    j' <- [j - 1 .. j + 1]
    guard $ (i', j') /= (i, j)
    guard $ inBounds (i', j')
    pure $ readArray board (i', j')

initBoard :: a -> BoardSize -> ST s (Board s a)
initBoard _ (r, c) | r < 1 || c < 1 = error "initBoard expects positive rows and cols."
initBoard c (rows, cols) = newArray ((0, 0), (rows - 1, cols - 1)) c
