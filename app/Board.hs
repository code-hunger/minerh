{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}

module Board (Board (..), MBoard (..), Coord (..)) where

import Data.Array.ST (Ix (inRange, range), MArray (getBounds), getElems, readArray, writeArray)

data Coord = Coord {x :: Int, y :: Int}

class (Monad m) => Board b m e | b -> e where
    (!) :: b -> Coord -> m e
    lines :: b -> m [[e]]
    hasIndex :: b -> Coord -> m Bool
    indices :: b -> m [Coord]

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

    hasIndex array i = do
        bounds <- getBounds array
        pure $ bounds `inRange` (y i, x i)

    indices array = map (\(i, j) -> Coord j i) . range <$> getBounds array

instance (MArray arr el m) => MBoard (arr (Int, Int) el) m el where
    write array i = writeArray array (y i, x i)
