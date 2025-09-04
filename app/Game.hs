module Game where

import Board (Coord)

data Block = Air | Dirt | Stone | Stairs | Fire
    deriving (Eq)

data Game board = Game
    { player :: Coord
    , board :: board
    , movingParts :: [Coord]
    }
