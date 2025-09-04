module BoardGen where

import Data.Array.MArray (MArray)

import Control.Monad (filterM, guard)
import Control.Monad.ST.Lazy (ST, runST)
import Control.Monad.ST.Lazy.Unsafe (unsafeInterleaveST)
import Data.Array (Array)
import Data.Array.ST (STArray, freeze, newArray)

import Board (Board (..), Coord (Coord), MBoard (..))
import Control.Monad.State (MonadTrans (lift), StateT, execStateT)
import Data.Foldable (traverse_)
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
makePureBoards size g defBlock weigh = runST $ do
    board <-
        initBoard defBlock size ::
            -- type annotation is required here because initBoard is too general and GHC does not
            -- know to instantiate the same `s` variable on both positions here.
            (forall s. ST s (STArray s (Int, Int) b))
    let go = do
            a <- freeze board
            _ <- execStateT (nextBoard board weigh) g
            rest <- unsafeInterleaveST go
            pure (a : rest)
    go

nextBoard ::
    forall g board m.
    (RandomGen g, MBoard board m) =>
    board ->
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
    (MBoard board m) =>
    Coord ->
    board ->
    m [Item board]
getNeighbours pos board =
    mapMM
        (board !)
        validNeighbourIndices
  where
    validNeighbourIndices = filterM (hasIndex board) neighbourIndices

    neighbourIndices :: [Coord]
    neighbourIndices = do
        let (Coord i j) = pos
        i' <- [i - 1 .. i + 1]
        j' <- [j - 1 .. j + 1]
        guard $ (i', j') /= (i, j)
        pure (Coord i' j')

initBoard :: forall m el board. (MArray el board m) => board -> BoardSize -> m (el (Int, Int) board)
initBoard _ bs | rows bs < 1 || cols bs < 1 = error "initBoard expects positive rows and cols."
initBoard c bs = newArray ((0, 0), (rows bs - 1, cols bs - 1)) c

mapMM_ :: (Foldable t, Monad m) => (a -> m b) -> m (t a) -> m ()
mapMM_ f as = traverse_ f =<< as

mapMM :: (Traversable t, Monad m) => (a -> m b) -> m (t a) -> m (t b)
mapMM f as = traverse f =<< as
