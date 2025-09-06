module BoardGen where

import Data.Array.MArray (MArray)

import Control.Monad (guard)
import Control.Monad.ST.Lazy (ST, runST)
import Control.Monad.ST.Lazy.Unsafe (unsafeInterleaveST)
import Data.Array (Array)
import Data.Array.ST (STArray, freeze, newArray)

import Board (Board (..), Coord (Coord), Index (unIndex), MBoard (..), withArray)
import Control.Monad.State (MonadTrans (lift), StateT, execStateT)
import Data.Foldable (traverse_)
import Data.Maybe (catMaybes)
import System.Random (RandomGen)

type CellUpdater m s el = (Monad m) => (el -> [el] -> StateT s m el)

data BoardSize = BoardSize {rows :: Int, cols :: Int}

makePureBoards ::
    forall g b.
    (RandomGen g) =>
    BoardSize ->
    g ->
    b ->
    (forall m. CellUpdater m g b) ->
    [Array (Int, Int) b]
makePureBoards size g def weigh = runST $ do
    board <-
        initBoard def size ::
            -- type annotation is required here because initBoard is too general and GHC does not
            -- know to instantiate the same `s` variable on both positions here.
            (forall s. ST s (STArray s (Int, Int) b))
    withArray board $ \b ->
        let go = do
                a <- freeze board
                _ <- execStateT (nextBoard b weigh) g
                rest <- unsafeInterleaveST go
                pure (a : rest)
         in go

nextBoard ::
    forall g board ph m.
    (RandomGen g, MBoard board m) =>
    board ph ->
    CellUpdater m g (Item board) ->
    StateT g m ()
nextBoard board weigh =
    mapMM_
        updateCell
        (lift (elems board))
  where
    updateCell (i, val) = do
        neighbours <- lift $ getNeighbours i board
        nextVal <- weigh val neighbours
        lift $ write board i nextVal

getNeighbours ::
    forall m board ph.
    (MBoard board m) =>
    Index ph ->
    board ph ->
    m [Item board]
getNeighbours pos board = mapMM (board !) neighbours
  where
    neighbours :: m [Index ph]
    neighbours = fmap catMaybes $ sequence $ do
        let (Coord i j) = unIndex pos
        i' <- [i - 1 .. i + 1]
        j' <- [j - 1 .. j + 1]
        guard $ (i', j') /= (i, j)
        pure $ justify board (Coord i' j')

initBoard ::
    forall m el array.
    (MArray array el m) =>
    el ->
    BoardSize ->
    m (array (Int, Int) el)
initBoard _ bs | rows bs < 1 || cols bs < 1 = error "initBoard expects positive rows and cols."
initBoard c bs = newArray ((0, 0), (rows bs - 1, cols bs - 1)) c

mapMM_ :: (Foldable t, Monad m) => (a -> m b) -> m (t a) -> m ()
mapMM_ f as = traverse_ f =<< as

mapMM :: (Traversable t, Monad m) => (a -> m b) -> m (t a) -> m (t b)
mapMM f as = traverse f =<< as
