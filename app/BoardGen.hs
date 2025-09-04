module BoardGen where

import Data.Array.MArray (MArray)

import Control.Monad (filterM, guard, join, liftM2)
import Control.Monad.ST.Lazy (ST, runST)
import Control.Monad.ST.Lazy.Unsafe (unsafeInterleaveST)
import Data.Array (Array)
import Data.Array.ST (STArray, freeze, newArray)

import Board (Board (..), Coord (Coord), MBoard (..))
import Control.Monad.State (MonadTrans (lift), StateT, execStateT)
import Data.Foldable (traverse_)
import System.Random (RandomGen)

type CellUpdater m s a = (Monad m) => (a -> [a] -> StateT s m a)

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
    let
        go = do
            a <- freeze board
            _ <- execStateT (nextBoard board weigh) g
            rest <- unsafeInterleaveST go
            pure (a : rest)
    go

nextBoard ::
    forall g a b m.
    (RandomGen g, MBoard b m a) =>
    b ->
    CellUpdater m g a ->
    StateT g m ()
nextBoard board weigh = traverse_ updateCell =<< lift (elems board)
  where
    updateCell :: (Coord, a) -> StateT g m ()
    updateCell (i, val) = lift . write board i =<< nextVal
      where
        neighbours = lift (getNeighbours i board)
        nextVal = weigh val =<< neighbours

getNeighbours :: forall b m a. (MBoard b m a) => Coord -> b -> m [a]
getNeighbours pos board = traverse (board !) =<< validNeighbourIndices
  where
    validNeighbourIndices :: m [Coord]
    validNeighbourIndices = filterM (hasIndex board) neighbourIndices

    neighbourIndices :: [Coord]
    neighbourIndices = do
        let (Coord i j) = pos
        i' <- [i - 1 .. i + 1]
        j' <- [j - 1 .. j + 1]
        guard $ (i', j') /= (i, j)
        pure (Coord i' j')

initBoard :: forall m a b. (MArray a b m) => b -> BoardSize -> m (a (Int, Int) b)
initBoard _ bs | rows bs < 1 || cols bs < 1 = error "initBoard expects positive rows and cols."
initBoard c bs = newArray ((0, 0), (rows bs - 1, cols bs - 1)) c

bind2 :: (Monad m) => (a -> b -> m r) -> m a -> m b -> m r
bind2 f a b = join $ liftM2 f a b
