module BoardGen where

import Data.Array.MArray (MArray)

import Control.Monad (filterM, guard)
import Control.Monad.ST.Lazy (ST, runST)
import Control.Monad.ST.Lazy.Unsafe (unsafeInterleaveST)
import Data.Array (Array)
import Data.Array.ST (STArray, freeze, newArray)

import Board (Board (..), Coord (Coord), MBoard (..))
import Control.Monad.State (MonadTrans (lift), State, StateT, execStateT, runState)
import qualified Control.Monad.State as S (MonadState (get, put))
import Data.Foldable (traverse_)
import System.Random (RandomGen)

type CellUpdater g a = (a -> [a] -> State g a)

data BoardSize = BoardSize {rows :: Int, cols :: Int}

makePureBoards ::
    forall g b.
    (RandomGen g) =>
    BoardSize ->
    g ->
    b ->
    CellUpdater g b ->
    [Array (Int, Int) b]
makePureBoards size g defBlock weigh = runST $ do
    board <- initBoard' defBlock size
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
    CellUpdater g a ->
    StateT g m ()
nextBoard board weigh = traverse_ updateCell =<< lift (indices board)
  where
    updateCell :: Coord -> StateT g m ()
    updateCell i = do
        cellValue <- lift $ board ! i
        neighbours <- lift $ getNeighbours i board
        g <- S.get
        let (res, g') = runState (weigh cellValue neighbours) g
        S.put g'
        lift $ write board i res

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

initBoard' :: b -> BoardSize -> ST s (STArray s (Int, Int) b)
initBoard' _ bs | rows bs < 1 || cols bs < 1 = error "initBoard expects positive rows and cols."
initBoard' c bs = newArray ((0, 0), (rows bs - 1, cols bs - 1)) c
