{-# LANGUAGE TypeOperators #-}

module Game where

import Board (Board (Item, hasIndex, (!)), Coord (..), MBoard (write))
import Control.Monad.Extra (forM, unlessM, when, whenM, (&&^))
import Control.Monad.State (MonadTrans (lift), StateT)
import qualified Control.Monad.State as State
import Data.Function (fix)

data Block = Air | Dirt | Stone | Stairs | Fire
    deriving (Eq)

data Game board = Game
    { player :: Coord
    , board :: board
    , movingParts :: [Coord]
    }

trackDug :: forall m b. (MBoard b m, MonadFail m, Item b ~ Block) => Coord -> StateT (Game b) m ()
trackDug pos = do
    unlessM ((== Air) <$> blockTypeAt pos) $
        fail "Called dug on non-air!"

    let above = moveUp pos
    whenM
        ( hasIndex' above
            &&^ (canFall <$> blockTypeAt above)
        )
        $ addToTracked above
  where
    hasIndex' i = State.get >>= (\(Game _ b _) -> lift $ hasIndex b i)
    blockTypeAt i = State.get >>= (\(Game _ b _) -> lift $ b ! i)
    addToTracked i = State.modify' (\g -> g{movingParts = i : movingParts g})

canFall :: Block -> Bool
canFall blockType = blockType == Stone || blockType == Fire

moveUp :: Coord -> Coord
moveUp = (`movePos` GoUp)

movePlayer ::
    ( MBoard board m
    , Item board ~ Block
    , MonadFail m
    ) =>
    Dir ->
    StateT (Game board) m ()
movePlayer dir = do
    (Game pos board _) <- State.get
    let nextPos = movePos pos dir
    inBounds <- lift $ hasIndex board nextPos
    when inBounds $ do
        nextBlock <- lift $ board ! nextPos
        thisBlock <- lift $ board ! pos
        let needsToDig = nextBlock == Dirt
        when needsToDig $
            lift (write board nextPos Air)
                >> trackDug nextPos
        let willMove =
                not (needsToDig && dir == GoUp)
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
        let below = movePos i GoDown
        belowType <- board ! below
        if belowType == Air || belowType == Stairs
            then collapseAt board i >> pure [below]
            else pure []
    State.modify' (\g -> g{movingParts = movingParts'})

    let belowPlayer = movePos player GoDown
    belowPlayerType <- lift $ board ! belowPlayer
    when (belowPlayerType == Air) $ do
        State.modify' $ \g -> g{player = belowPlayer}

collapseAt ::
    forall m board.
    (MBoard board m, Item board ~ Block) =>
    board ->
    Coord ->
    m ()
collapseAt board j = do
    isValid <- hasIndex board j
    blockType <- board ! j
    when (isValid && canFall blockType) $ do
        write board j Air
        write board (movePos j GoDown) blockType
        collapseAt board (moveUp j)

data Dir = GoLeft | GoRight | GoUp | GoDown
    deriving (Eq, Show)

movePos :: Coord -> Dir -> Coord
movePos (Coord x y) GoLeft = Coord (x - 1) y
movePos (Coord x y) GoRight = Coord (x + 1) y
movePos (Coord x y) GoUp = Coord x (y - 1)
movePos (Coord x y) GoDown = Coord x (y + 1)
