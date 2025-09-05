{-# LANGUAGE TypeOperators #-}

module Game where

import Board (Board (Item, hasIndex, (!)), Coord (..), MBoard (write))
import Control.Monad.Extra (forM, unlessM, when, whenJust, whenM, (&&^))
import Control.Monad.State (MonadTrans (lift), StateT)
import qualified Control.Monad.State as State
import System.IO.Error (alreadyExistsErrorType)

data Block = Air | Dirt | Stone | Stairs | Fire
    deriving (Eq)

data DigRequest = DigRequest Dir Int

data Game board = Game
    { player :: (Coord, Maybe DigRequest)
    , board :: board
    , movingParts ::
        [ ( Int -- how many ticks should pass until thhe part drops by 1 block
          , Coord -- the bottom part of a collapsing column
          )
        ]
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
        $ addToTracked (30, above)
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
    (Game (pos, digRequest) board _) <- State.get
    let nextPos = movePos pos dir
    inBounds <- lift $ hasIndex board nextPos

    let alreadyWantsToDigThere =
            maybe False (\(DigRequest requestDir _) -> requestDir == dir) digRequest

    when (inBounds && not alreadyWantsToDigThere) $ do
        nextBlock <- lift $ board ! nextPos
        thisBlock <- lift $ board ! pos
        let needsToDig = nextBlock == Dirt
        when needsToDig $
            State.modify' (\g -> g{player = (pos, Just $ DigRequest dir 4)})
        let willMove =
                not needsToDig
                    && nextBlock /= Stone
                    && nextBlock /= Fire
        let needsStairs = dir == GoUp && thisBlock == Air && willMove
        when needsStairs $
            lift $
                write board pos Stairs
        when willMove $
            State.modify' (\g -> g{player = (nextPos, Nothing)})

update ::
    forall m board.
    (MBoard board m, Item board ~ Block, MonadFail m) =>
    StateT (Game board) m ()
update = do
    (Game (playerPos, digRequest) board movingParts) <- State.get
    movingParts' <- fmap concat $ forM movingParts $ \(tick, i) -> lift $ do
        let below = movePos i GoDown
        belowType <- board ! below
        if belowType == Air || belowType == Stairs
            then
                if tick > 1
                    then pure [(tick - 1, i)]
                    else collapseAt board i >> pure [(5, below)]
            else pure []
    State.modify' (\g -> g{movingParts = movingParts'})

    let belowPlayer = movePos playerPos GoDown
    belowPlayerType <- lift $ board ! belowPlayer
    when (belowPlayerType == Air) $ do
        State.modify' $ \g -> g{player = (belowPlayer, Nothing)}

    whenJust digRequest $ \(DigRequest dir ticks) ->
        if ticks > 0
            then State.modify' (\g -> g{player = (playerPos, Just (DigRequest dir (ticks - 1)))})
            else do
                let nextPos = playerPos `movePos` dir
                lift (write board nextPos Air)
                trackDug nextPos
                State.modify'
                    ( \g ->
                        g
                            { player =
                                -- if we dug up, we stay here; if we dug down - we'll fall anyway,
                                -- so we only need to care for left and right
                                ( if dir == GoLeft || dir == GoRight then nextPos else playerPos
                                , -- dig request evaluated, reset it!
                                  Nothing
                                )
                            }
                    )

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
