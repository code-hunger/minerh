module Render.Common where

import Board (Board (), Coord (Coord), Item, SafeArray (justify, (!)))
import Data.Maybe (fromMaybe)

data BoardSlice = BoardSlice {rows :: Int, cols :: Int, startX :: Int, startY :: Int}

data UserEvent = KEsc | KQ | KDown | KLeft | KUp | KUpShift | KDownShift | KRight | Save | Other deriving (Show, Eq)

sliceImage :: (Monad m, Board board m) => board ph -> BoardSlice -> m [[Item board]]
sliceImage board slice =
    let visibleRows =
            [ startY slice + i
            | i <- [0 .. rows slice - 1]
            ]
     in mapM (sliceRow board slice) visibleRows

sliceRow :: forall m board ph. (Board board m, Monad m) => board ph -> BoardSlice -> Int -> m [Item board]
sliceRow board slice row = mapM forceRead wantedCoords
  where
    wantedCoords =
        [ Coord (startX slice + i) row
        | i <- [0 .. cols slice - 1]
        ]
    forceRead :: Coord -> m (Item board)
    forceRead k =
        -- As of now, we can't convince the type system that our coordinates are valid,
        -- so we have to use fromJust. I leave a message just in case.
        -- A more 'proper' solution would be to export richer functions from the Board module, which
        -- provide more justified indices, e.g. a function that returns an already justified view
        -- around a justified index.
        fromMaybe (error "Board index out of bounds during rendering")
            <$> (justify board k >>= mapM (board !))
