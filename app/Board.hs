{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TupleSections #-}

module Board (Board (..), MBoard (..), Coord (..)) where

import Control.Monad (liftM2)
import Data.Array.ST (Ix (inRange, range), MArray (getBounds), getElems, readArray, writeArray)
import Data.Bifunctor (Bifunctor (bimap))

data Coord = Coord {x :: Int, y :: Int} deriving (Show, Eq)

toCoord :: (Int, Int) -> Coord
toCoord (i, j) = Coord j i

fromCoord :: Coord -> (Int, Int)
fromCoord (Coord _x _y) = (_y, _x)

fromCoordPair :: (Coord, Coord) -> ((Int, Int), (Int, Int))
fromCoordPair = bimap fromCoord fromCoord

inRangeCoord :: (Coord, Coord) -> Coord -> Bool
inRangeCoord bb i = fromCoordPair bb `inRange` fromCoord i

class (Monad m) => Board b m e | b -> e where
    (!) :: b -> Coord -> m e
    lines :: b -> m [[e]]
    bounds :: b -> m (Coord, Coord)

    indices :: b -> m [Coord]
    -- smells like a space leak if the whole list is computed before returned
    indices array = map toCoord . range . fromCoordPair <$> bounds array

    hasIndex :: b -> Coord -> m Bool
    hasIndex array i = (`inRangeCoord` i) <$> bounds array

    elems :: b -> m [(Coord, e)]
    elems b = indices b >>= traverse coupleValue
      where
        coupleValue i = (i,) <$> (b ! i :: m e)

class (Board b m e) => MBoard b m e where
    write :: b -> Coord -> e -> m ()

instance (MArray arr el m) => Board (arr (Int, Int) el) m el where
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

instance (MArray arr el m) => MBoard (arr (Int, Int) el) m el where
    write array i = writeArray array (y i, x i)
