{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}

module Board (Board (..), MBoard (..), Coord (..)) where

import Control.Monad.State.Lazy (MonadTrans (lift), StateT)
import Data.Array.ST (Ix (inRange, range), MArray (getBounds), getElems, readArray, writeArray)
import Data.Bifunctor (Bifunctor (bimap))
import Data.Kind (Type)

data Coord = Coord {x :: Int, y :: Int} deriving (Show, Eq)

toCoord :: (Int, Int) -> Coord
toCoord (i, j) = Coord j i

fromCoord :: Coord -> (Int, Int)
fromCoord (Coord _x _y) = (_y, _x)

fromCoordPair :: (Coord, Coord) -> ((Int, Int), (Int, Int))
fromCoordPair = bimap fromCoord fromCoord

inRangeCoord :: (Coord, Coord) -> Coord -> Bool
inRangeCoord bb i = fromCoordPair bb `inRange` fromCoord i

-- A `board` is an abstraction over a 2D matrix of elements `el`, that lives in a monad `m`.
class (Monad m) => Board board m where
    type Item board :: Type
    (!) :: board -> Coord -> m (Item board)
    lines :: board -> m [[(Item board)]]
    bounds :: board -> m (Coord, Coord)

    indices :: board -> m [Coord]
    -- smells like a space leak if the whole list is computed before returned
    indices array = map toCoord . range . fromCoordPair <$> bounds array

    hasIndex :: board -> Coord -> m Bool
    hasIndex array i = (`inRangeCoord` i) <$> bounds array

    elems :: board -> m [(Coord, (Item board))]
    elems b = indices b >>= traverse coupleValue
      where
        coupleValue i = (i,) <$> (b ! i :: m (Item board))

-- A mutable board is a board that can be mutated
class (Board board m) => MBoard board m where
    write :: board -> Coord -> Item board -> m ()

instance (MArray arr el m) => Board (arr (Int, Int) el) m where
    type Item (arr (Int, Int) el) = el

    array ! i = readArray array (y i, x i)

    lines array = do
        width <- getWidth array

        let go [] = []
            go xs =
                let (h, t) = splitAt width xs
                 in h : go t
        go <$> getElems array
      where
        getWidth = fmap boundsToWidth . getBounds
        boundsToWidth ((_, xmin), (_, xmax)) = xmax - xmin + 1

    bounds array = toCoordPair <$> getBounds array
      where
        toCoordPair :: ((Int, Int), (Int, Int)) -> (Coord, Coord)
        toCoordPair = bimap toCoord toCoord

instance (MArray arr el m) => MBoard (arr (Int, Int) el) m where
    write array i = writeArray array (y i, x i)
