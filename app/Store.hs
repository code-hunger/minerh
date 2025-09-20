{-# LANGUAGE ViewPatterns #-}

module Store (serialize, deserialize) where

import Board (ArrayS, Board (Item, bounds, lines), Coord (..), withArray)
import BoardGen (BoardSize (..), loadBoard)
import Data.Array.Base (MArray)
import Data.Char (ord)
import Game (Block (..), Game (..))

serialize ::
    forall ph m board.
    (Board board m, Item board ~ Block) =>
    Game (board ph) ph ->
    m String
serialize (Game (pos, fallingState, _) board _) = do
    lines_ <- Board.lines board
    size <- flip fmap (bounds board) $ \((Coord 0 0), bound) ->
        BoardSize{rows = y bound + 1, cols = x bound + 1} -- bounds are inclusive
    pure $
        unlines $
            show pos
                : show fallingState
                : show size
                : fmap (map printBlock) lines_

deserialize ::
    (MArray a Block m) =>
    String ->
    (forall ph. Game (ArrayS (a (Int, Int) Block) ph) ph -> m r) ->
    m r
deserialize string f = do
    let ( pos
                : fallingState
                : size
                : boardData
            ) = Prelude.lines string
    array <- loadBoard (readBlock <$> concat boardData) (read size)
    withArray array $ \board ->
        f $
            Game
                { player =
                    ( read pos
                    , read fallingState
                    , Nothing
                    )
                , movingParts = []
                , board = board
                }

readBlock :: Char -> Block
readBlock '🪨' = Stone
readBlock 'D' = Dirt
readBlock ' ' = Air
readBlock '🔥' = Fire
readBlock '🪜' = Stairs
readBlock c = error $ "Unrecognized character on readBlock (" ++ [c] ++ ", code=" ++ show (ord c) ++ ")"

printBlock :: Block -> Char
printBlock Stone = '🪨'
printBlock Dirt = 'D'
printBlock Air = ' '
printBlock Fire = '🔥'
printBlock Stairs = '🪜'
