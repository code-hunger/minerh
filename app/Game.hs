{-# LANGUAGE TypeOperators #-}

module Game where

import Board (Board (Item, hasIndex, (!)), Coord (..), MBoard (write))
import Control.Monad.Extra (forM, when, whenM)
import Control.Monad.State (MonadTrans (lift), StateT)
import qualified Control.Monad.State as State

data Block = Air | Dirt | Stone | Stairs | Fire
    deriving (Eq)

data Game board = Game
    { player :: Coord
    , board :: board
    , movingParts :: [Coord]
    }

trackDug :: (MBoard b m, MonadFail m, Item b ~ Block) => Coord -> StateT (Game b) m ()
trackDug pos = do
    (Game _ board _) <- State.get
    thisBlockType <- lift $ board ! pos
    when (thisBlockType /= Air) $ fail "Called dug on non-air!"
    let abovePos = movePos pos GoUp
    whenM (lift $ hasIndex board abovePos) $ do
        aboveBlockType <- lift $ board ! abovePos
        when (aboveBlockType == Stone) $
            State.modify' (\g -> g{movingParts = abovePos : movingParts g})

movePlayer ::
    (MBoard board m, MonadFail m, Item board ~ Block) =>
    Dir ->
    StateT (Game board) m ()
movePlayer dir = do
    (Game pos board _) <- State.get
    let nextPos = movePos pos dir
    inBounds <- lift $ hasIndex board nextPos
    if not inBounds
        then pure ()
        else do
            nextBlock <- lift $ board ! nextPos
            thisBlock <- lift $ board ! pos
            let needsDig = nextBlock == Dirt
            when needsDig $
                lift (write board nextPos Air) >> trackDug nextPos
            let willMove =
                    not (needsDig && dir == GoUp)
                        && nextBlock /= Stone
                        && nextBlock /= Fire
            let needsStairs = dir == GoUp && thisBlock == Air && willMove
            when needsStairs $
                lift $
                    write board pos Stairs
            when willMove $
                State.modify' (\g -> g{player = nextPos})

update ::
    forall m board.
    (MBoard board m, Item board ~ Block) =>
    StateT (Game board) m ()
update = do
    (Game player board movingParts) <- State.get
    movingParts' <- fmap concat $ forM movingParts $ \i -> lift $ do
        blockType <- board ! i
        let below = movePos i GoDown
        belowType <- board ! below
        if blockType == Stone && (belowType == Air || belowType == Stairs)
            then do
                write board i Air
                write board below Stone
                pure [below]
            else
                pure []
    State.modify' (\g -> g{movingParts = movingParts'})

    let belowPlayer = movePos player GoDown
    belowPlayerType <- lift $ board ! belowPlayer
    when (belowPlayerType == Air) $ do
        State.modify' $ \g -> g{player = belowPlayer}

data Dir = GoLeft | GoRight | GoUp | GoDown
    deriving (Eq, Show)

movePos :: Coord -> Dir -> Coord
movePos (Coord x y) GoLeft = Coord (x - 1) y
movePos (Coord x y) GoRight = Coord (x + 1) y
movePos (Coord x y) GoUp = Coord x (y - 1)
movePos (Coord x y) GoDown = Coord x (y + 1)
