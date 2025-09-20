{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}

module Board (Board (..), MBoard (..), Coord (..), Index (unIndex), withArray, unArrayS, ArrayS) where

import Control.Monad.Extra (forM, ifM)
import Data.Array.ST (Ix (inRange, range), MArray (getBounds), getElems, readArray, writeArray)
import Data.Bifunctor (Bifunctor (bimap))
import Data.Kind (Type)

data Coord = Coord {x :: Int, y :: Int} deriving (Show, Read, Eq)

toCoord :: (Int, Int) -> Coord
toCoord (i, j) = Coord j i

fromCoord :: Coord -> (Int, Int)
fromCoord (Coord _x _y) = (_y, _x)

fromCoordPair :: (Coord, Coord) -> ((Int, Int), (Int, Int))
fromCoordPair = bimap fromCoord fromCoord

inRange' :: Coord -> (Coord, Coord) -> Bool
inRange' i bb = fromCoordPair bb `inRange` fromCoord i

newtype Index b = Index {unIndex :: Coord} -- do NOT export constructor
    deriving newtype (Show, Read)

-- A `board` is an abstraction over a 2D matrix of elements `el`, that lives in a monad `m`.
class (Monad m) => Board board m where
    type Item board :: Type
    (!) :: board ph -> Index ph -> m (Item board)
    lines :: board ph -> m [[Item board]]
    bounds :: board ph -> m (Coord, Coord)

    streamRow :: board ph -> Index ph -> Int -> ([Item board] -> r) -> m r

    getWidth :: board ph -> m Int
    getWidth array = boundsToWidth <$> bounds array
      where
        boundsToWidth (Coord xmin _, Coord xmax _) = xmax - xmin + 1

    indices :: board ph -> m [Index ph]
    -- smells like a space leak if the whole list is computed before returned
    indices array = map (Index . toCoord) . range . fromCoordPair <$> bounds array

    hasIndex :: board ph -> Coord -> m Bool
    hasIndex array i = inRange' i <$> bounds array

    justify :: board ph -> Coord -> m (Maybe (Index ph))
    justify array i = do
        isValid <- array `hasIndex` i
        pure $
            if isValid
                then Just (Index i)
                else Nothing

    elems :: board ph -> m [(Index ph, Item board)]
    elems b = indices b >>= traverse coupleValue
      where
        coupleValue i = (i,) <$> (b ! i :: m (Item board))

-- A mutable board is a board that can be mutated
class (Board board m) => MBoard board m where
    write :: board ph -> Index ph -> Item board -> m ()

newtype ArrayS a ph = ArrayS {unArrayS :: a}

withArray :: a -> (forall ph. ArrayS a ph -> t) -> t
withArray a f = f (ArrayS a)

instance (MArray arr el m) => Board (ArrayS (arr (Int, Int) el)) m where
    type Item (ArrayS (arr (Int, Int) el)) = el

    array ! (Index i) = readArray (unArrayS array) (y i, x i)

    streamRow array (Index i) n f = do
        els <- forM [x i .. x i + n - 1] $ \x' -> readArray (unArrayS array) (fromCoord $ Coord x' (y i))
        pure $ f els

    lines array = do
        width <- getWidth array

        let go [] = []
            go xs =
                let (h, t) = splitAt width xs
                 in h : go t
        go <$> getElems (unArrayS array)

    bounds array = toCoordPair <$> getBounds (unArrayS array)
      where
        toCoordPair :: ((Int, Int), (Int, Int)) -> (Coord, Coord)
        toCoordPair = bimap toCoord toCoord

instance (MArray arr el m) => MBoard (ArrayS (arr (Int, Int) el)) m where
    write array (Index i) = writeArray (unArrayS array) (y i, x i)
