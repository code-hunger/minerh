{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}

module Board (SafeArray (..), Board (..), MBoard (..), Coord (..), Index (unIndex), withPass, unArrayS, WithPass, BoardSize (..)) where

import Data.Array.ST (Ix (inRange, range), MArray (getBounds), getElems, readArray, writeArray)
import Data.Bifunctor (Bifunctor (bimap))
import Data.Kind (Type)

-- Adds a pass to a type `a`. The pass is in the form of a type parameter which is used to track
-- type-safe indexing. This module exports only the type constructor, but not the data constructor.
--
-- That is, users can operate with an existing `WithPass a ph`, but cannot construct one, i.e. they
-- cannot construct the password.
newtype WithPass a ph = WithPass {unArrayS :: a}

-- Provides a pass to the given function, which it can use for type-safe operations on the given `a`.
withPass :: a -> (forall ph. WithPass a ph -> t) -> t
withPass a f = f (WithPass a)

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

data BoardSize = BoardSize {rows :: Int, cols :: Int} deriving (Read, Show)

-- A `justified-containers`-based way of providing typesafe indexing.
-- The user receives a typesafe index only if it is valid for the given board.
-- Because a board cannot change size, this index is then valid forever.
class (Functor m) => SafeArray board m where
    type Item board :: Type

    (!) :: board ph -> Index ph -> m (Item board)

    hasIndex :: board ph -> Coord -> m Bool

    justify :: board ph -> Coord -> m (Maybe (Index ph))
    justify array i = do
        isValid <- array `hasIndex` i
        pure $
            if isValid
                then Just (Index i)
                else Nothing

-- A `board` is an abstraction over a 2D matrix of elements `el`, that lives in a monad `m`.
-- It extends the generic SafeArray to work with a rectangular structure providing `indices`,
-- `lines` and `bounds`
class (SafeArray board m) => Board board m where
    lines :: board ph -> m [[Item board]]
    bounds :: board ph -> m (Coord, Coord)

    getSize :: board ph -> m BoardSize
    getSize array = do
        ~(Coord xmin ymin, Coord xmax ymax) <- bounds array
        pure BoardSize{cols = xmax - xmin + 1, rows = ymax - ymin + 1}

    indices :: board ph -> m [Index ph]
    -- smells like a space leak if the whole list is computed before returned
    indices array = map (Index . toCoord) . range . fromCoordPair <$> bounds array

-- A mutable board is a board that can be mutated
class (Board board m) => MBoard board m where
    write :: board ph -> Index ph -> Item board -> m ()

instance (MArray arr el m) => SafeArray (WithPass (arr (Int, Int) el)) m where
    type Item (WithPass (arr (Int, Int) el)) = el

    array ! (Index i) = readArray (unArrayS array) (y i, x i)

    hasIndex array i = inRange' i <$> bounds array

instance (MArray arr el m) => Board (WithPass (arr (Int, Int) el)) m where
    lines array = do
        (cols -> width) <- getSize array

        let go [] = []
            go xs =
                let (h, t) = splitAt width xs
                 in h : go t
        go <$> getElems (unArrayS array)

    bounds array = toCoordPair <$> getBounds (unArrayS array)
      where
        toCoordPair :: ((Int, Int), (Int, Int)) -> (Coord, Coord)
        toCoordPair = bimap toCoord toCoord

instance (MArray arr el m) => MBoard (WithPass (arr (Int, Int) el)) m where
    write array (Index i) = writeArray (unArrayS array) (y i, x i)
